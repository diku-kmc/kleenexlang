{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}

module KMC.Kleenex.Action where


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

type KleenexAction = (ActionExpr String) :+: (Const [Bool] [Word8])
type KleenexActionMu a = Mu BitInputTerm KleenexAction a

type BitInputTerm = RS.RangeSet Word8

--newtype Bits sigma = Bits [Bool]
--    deriving (Ord, Eq, Show)

matchVal :: Int -> BitInputTerm
matchVal = RS.singleton . fromIntegral

--instance Enum (Bits sigma) where
--    toEnum = Bits . toBinary
--    fromEnum (Bits b) = fromBinary b
--
--instance (Bounded sigma, Enum sigma) => Bounded (Bits sigma) where
--    minBound = Bits []
--    maxBound = Bits $ toBinary $ fromEnum (maxBound :: sigma)
--
--instance (Bounded sigma, Enum sigma) => Boolean (BitInputTerm sigma) where
--    bot = BitInputTerm RS.empty
--    top = BitInputTerm $ RS.rangeSet [(minBound, maxBound)]
--    neg (BitInputTerm rs) = BitInputTerm $ RS.complement rs
--    conj (BitInputTerm rs1) (BitInputTerm rs2) = BitInputTerm $ RS.intersection rs1 rs2 
--    disj (BitInputTerm rs1) (BitInputTerm rs2) = BitInputTerm $ RS.union rs1 rs2 
--
--toBinary :: (Integral a) => a -> [Bool]
--toBinary 0 = []
--toBinary n = (n `mod` 2 == 1) : toBinary (n `div` 2)
--
--fromBinary :: (Num a) => [Bool] -> a
--fromBinary bs = go (length bs) bs
--    where
--        go n (True : bs)  = 2^n + go (n-1) bs
--        go n (False : bs) = go (n-1) bs


data ActionFunc = ParseBitsFunc
    deriving (Ord, Show, Eq)

data (Ord var, Eq var, Show var) => 
    ActionExpr var = Id
                   | RegUpdate (RegisterUpdate var ActionFunc)
                   | ParseBits
    deriving (Ord, Show, Eq)


instance Function (ActionExpr var) where
    type Dom (ActionExpr var) = Word8
    type Rng (ActionExpr var) = [Word8]
    eval Id x = const [] x
--    eval (RegUpdate _) x = const [] x
    eval ParseBits x = const [] x
    isConst _ = Nothing
    inDom _ _ = True

instance Function (ActionFunc) where
    type Dom (ActionFunc) = Word8
    type Rng (ActionFunc) = [Word8]
    eval ParseBitsFunc x = const [] x
    isConst _ = Nothing
    inDom _ _ = True


genActionSST :: (Ord st, Enum st) => KleenexActionMu st -> SST st BitInputTerm (ActionFunc) Int
genActionSST mu = evalState sst []
    where
        fst = fromMu mu
        sst = do es <- edges
                 return $ SST { sstS = fstS fst
                              , sstE = es
                              , sstI = fstI fst
                              , sstF = final
                              }
            where
                getBufId :: String -> State [String] Int
                getBufId var = do names <- get
                                  case elemIndex var names of
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

--                edges :: State [String] (EdgeSet st BitInputTerm ActionFunc Int)
                edges = mmapM (\(a,b) -> do c <- mapM (\(p,f,st) -> do ru <- toRegUpd f
                                                                       return ([p], ru, st)) b
                                            return (a,c)) $ eForward $ fstE fst
                toRegUpd :: KleenexAction -> State [String] (RegisterUpdate Int ActionFunc)
                toRegUpd (Inl (RegUpdate ru)) = mmapM (\(a,b) -> do id <- getBufId a
                                                                    atoms <- mapM updateAtom b
                                                                    return (id,atoms)) ru
                toRegUpd (Inl (ParseBits))    = return $ M.singleton outputbuf [FuncA ParseBitsFunc 0]
                final = M.fromList $ map (\s -> (s, [Left outputbuf])) $  S.toList $ fstF fst
