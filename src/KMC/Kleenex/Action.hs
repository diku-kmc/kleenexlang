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
import qualified KMC.SymbolicSST as S
import KMC.Theories
import KMC.OutputTerm
import Debug.Trace

type KleenexAction = (S.ActionExpr Int) :+: (Const Word8 [Word8])
type KleenexActionMu a = Mu BitInputTerm KleenexAction a

type BitInputTerm = RS.RangeSet Word8

matchVal :: BitInputTerm
matchVal = RS.singleton minBound

-- | The term that copies the input char to output.
parseBitsAction :: RS.RangeSet Word8 -> KleenexAction
parseBitsAction rs = Inl $ S.ParseBits $ rs

-- | The term that outputs nothing.
nop :: a -> KleenexAction
nop = const (Inr $ Const [])

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
  addEdge q (Left (bFalse, Inr $ Const d)) q'
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

genActionSST :: (Ord st, Enum st) => KleenexActionMu st -> S.SST st BitInputTerm KleenexAction Int
genActionSST mu = sst
    where
        fst = fromBitcodeMu mu
        sst = do S.SST { S.sstS = fstS fst
                       , S.sstE = edges
                       , S.sstI = fstI fst
                       , S.sstF = final
                       }
            where
                edges = M.fromList $ do (st, e) <- M.toList $ eForward $ fstE fst
                                        let e' = [ ([p], convert a, st') | (p, a, st') <- e]
                                        return (st, e') 
                final = M.fromList $ map (\s -> (s, [Left 0])) $  S.toList $ fstF fst
                convert (Inl (S.RegUpdate var atoms)) = Inl $ M.singleton var (map fixup atoms)
                convert (Inl (S.ParseBits rs)) = Inl $ M.singleton 0 [S.VarA 0, S.FuncA (Inl $ S.ParseBits rs) 0]
                convert (Inl (S.PushOut var)) = Inr $ S.PushOut var
                convert (Inl S.PopOut)        = Inr S.PopOut
                convert (Inr (Const c)) = Inl $ M.singleton 0 [S.VarA 0, S.FuncA (Inl $ S.OutputConst c) 0]

                -- This construction is kinda dumb, but the inherent constraints
                -- in the SST makes this a necessity.
                fixup :: S.Atom var (S.ActionExpr var) 
                      -> S.Atom var (S.ActionExpr var :+: Const Word8 [Word8])
                fixup (S.FuncA f i) = S.FuncA (Inl f) i
                fixup (S.ConstA c) = (S.ConstA c)
                fixup (S.VarA var) = (S.VarA var)
