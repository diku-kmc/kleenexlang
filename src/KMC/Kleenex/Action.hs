{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE MultiParamTypeClasses #-}

module KMC.Kleenex.Action where


import Data.Monoid (Monoid, mempty)
import Data.ByteString (ByteString, pack)
import Data.Char
import Data.Word
import Data.List
import Control.Monad.State
import Control.Arrow
import qualified Data.Map as M
import qualified Data.Set as S
import Foreign.Marshal.Utils

import KMC.Coding
import KMC.Expression
import qualified KMC.RangeSet as RS
import KMC.FSTConstruction
import KMC.SymbolicFST
import KMC.SymbolicSST
import KMC.Theories
import KMC.OutputTerm
import Debug.Trace

type KleenexAction = (ActionExpr String) :+: (Const [Bool] [Word8])
type KleenexActionMu a = Mu BitInputTerm KleenexAction a

type BitInputTerm = RS.RangeSet Word8

matchVal :: BitInputTerm
matchVal = RS.singleton minBound

-- | The term that copies the input char to output.
parseBitsAction :: RS.RangeSet Word8 -> KleenexAction
parseBitsAction rs = Inl $ ParseBits rs

-- | The term that outputs nothing.
nop :: a -> KleenexAction
nop = const (Inr $ Const [])

data ActionFunc var = ParseBitsFunc (RS.RangeSet Word8)
                    | PushOutFunc var
                    | PopOutFunc
    deriving (Ord, Show, Eq)

data (Ord var, Eq var, Show var) => 
    ActionExpr var = RegUpdate (RegisterUpdate var (ActionFunc var))
                   | ParseBits (RS.RangeSet Word8)
                   | PushOut var
                   | PopOut
    deriving (Ord, Show, Eq)


instance Function (ActionExpr var) where
    type Dom (ActionExpr var) = Word8
    type Rng (ActionExpr var) = [Word8]
    eval (ParseBits rs) x = [decodeRangeSet rs x]
    isConst _ = Nothing
    inDom x (ParseBits rs) = member x rs
    inDom _ _ = True

instance Function (ActionFunc var) where
    type Dom (ActionFunc var) = Word8
    type Rng (ActionFunc var) = [Word8]
    eval (ParseBitsFunc rs) x = [decodeRangeSet rs x]
    isConst _ = Nothing
    inDom x (ParseBitsFunc rs) = (fromEnum x) < size rs

actionConstruct :: (Enum st, Ord st, Monoid (Rng KleenexAction))
             => st -> Mu BitInputTerm KleenexAction st -> Construct st BitInputTerm KleenexAction st
actionConstruct _ (Var q) = return q
actionConstruct qf (Loop e) = mfix (actionConstruct qf . e)
actionConstruct qf (RW p f e) = do
  q' <- actionConstruct qf e
  q <- fresh
  addEdge q (Left (p, f)) q'
  return q
actionConstruct qf (W d e) = do
  q' <- actionConstruct qf e
  q <- fresh
  addEdge q (Right d) q'
  return q
actionConstruct qf (Action a e) = do
  q' <- actionConstruct qf e
  q <- fresh
  addEdge q (Left (bFalse, a)) q'
  return q
actionConstruct qf (Alt e1 e2) = do
  q1 <- actionConstruct qf e1
  q2 <- actionConstruct qf e2
  q <- fresh
  addEdge q (Left (bFalse, Inr $ Const [])) q1
  addEdge q (Left (bTrue, Inr $ Const [])) q2
  return q
actionConstruct qf Accept = return qf
actionConstruct qf (Seq e1 e2) = do
  q2 <- actionConstruct qf e2
  actionConstruct q2 e1

fromBitcodeMu :: (Enum st, Ord st, Monoid (Rng KleenexAction)) =>
          Mu BitInputTerm KleenexAction st
       -> FST st BitInputTerm KleenexAction
fromBitcodeMu e =
  let (qin, cs) = runState (actionConstruct (toEnum 0) e)
                           (ConstructState { edges     = []
                                           , nextState = toEnum 1
                                           , states    = S.singleton (toEnum 0)
                                           })
  in FST { fstS = states cs
         , fstE = edgesFromList (edges cs)
         , fstI = qin
         , fstF = S.singleton (toEnum 0)
         }

genActionSST :: (Ord st, Enum st) => KleenexActionMu st -> SST st BitInputTerm (ActionFunc String) Int
genActionSST mu = evalState sst []
    where
        fst = fromBitcodeMu mu
        sst = do es <- edges
                 return $ SST { sstS = fstS fst
                              , sstE = es
                              , sstI = fstI fst
                              , sstF = final
                              }
            where
                getBufId :: String -> State [String] Int
                getBufId var = do names <- get
                                  if var == "outbuf"
                                  then return 0
                                  else case elemIndex var names of
                                        Just idx -> return $ idx + 1
                                        Nothing  -> do put $ names ++ [var]
                                                       return $ length names + 1
                outputbuf = 0
                mmapM :: (Monad m, Functor m, Ord c) => ((a,b) -> m (c,d)) -> M.Map a b -> m (M.Map c d)
                mmapM f = fmap M.fromList . mapM f . M.toList

                updateAtom (VarA var) = do id <- getBufId var 
                                           return $ VarA id
                updateAtom (ConstA c) = return $ ConstA c
                updateAtom (FuncA f i) = return $ FuncA f i

                edges = mmapM (\(a,b) -> do c <- mapM (\(p,f,st) -> do ru <- toRegUpd f
                                                                       return ([p], ru, st)) b
                                            return (a,c)) $ eForward $ fstE fst
                toRegUpd :: KleenexAction -> State [String] (RegisterUpdate Int (ActionFunc String))
                toRegUpd (Inl (RegUpdate ru)) = mmapM (\(a,b) -> do id <- getBufId a
                                                                    atoms <- mapM updateAtom b
                                                                    return (id,atoms)) ru
                toRegUpd (Inl (ParseBits rs)) = return $ M.singleton outputbuf [VarA outputbuf, FuncA (ParseBitsFunc rs) 0]
                toRegUpd (Inr (Const xs))  = return $ M.singleton outputbuf [VarA outputbuf, ConstA xs]
                final = M.fromList $ map (\s -> (s, [Left outputbuf])) $  S.toList $ fstF fst
