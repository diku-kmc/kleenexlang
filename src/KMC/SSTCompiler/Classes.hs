{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module KMC.SSTCompiler.Classes where

import           Control.Applicative
import           Control.Monad (zipWithM)
import           Control.Monad.Reader
import           Data.Functor.Identity (Identity(runIdentity))
import qualified Data.Map as M

import           KMC.Program.IL
import qualified KMC.RangeSet as RS
import           KMC.SSTCompiler.Env
import           KMC.Theories
import           KMC.Util.Coding

import           KMC.SymbolicFST.ActionMachine (CodeInputLab(..),DecodeFunc(..))
import           KMC.SymbolicFST.OracleMachine (CodeFunc(..))
import           KMC.SymbolicFST.Transducer (CopyFunc(..))
import           KMC.SymbolicSST.ActionSST (ConstOrAnyLab(..))

import           Prelude

----------
-- Classes
----------

class PredicateListToExpr p where
    predListToExpr :: [p] -> Int -> Expr

class Function f => CompilableFunction f tid | f -> tid where
  -- ^ Lookup table generation
  tables :: f -> M.Map tid Table
  -- ^ Function constants
  funcConstants :: f -> [[Int]]
  -- ^ Code generation
  compileFuncAppend :: (Ord st, Ord var, Ord tid) => BufferId -> f -> EnvReader st var tid Block

{-
-- | Tabulate a function. It is assumed that the codomain is a set of
-- bit-vectors with pairwise equal length.
tabulate :: (Function t, Rng t ~ [delta], Enum delta) => t -> Table
tabulate f = Table bitTable bitSize
  where
    bitTable = map (map fromEnum . eval f) (domain f)
    bitSize = foldr max 0 (map length bitTable)
-}

------------
-- Instances
------------

instance (Eq a, Enum a) => PredicateListToExpr (RS.RangeSet a) where
    predListToExpr [] _ = TrueE
    predListToExpr xs i =
      let (xsEq, xsRest') = span (\rs -> RS.size rs == 1) xs
          (xsComplex, xsRest) = span (\rs -> RS.size rs > 1) xsRest'
          eqExprs =
            case xsEq of
             []    -> []
             [sgl] -> [predToExpr sgl i]
             _     -> [CompareE i $ map (fromEnum . fromSingleton) xsEq]
          complexExprs = zipWith predToExpr xsComplex [i + length xsEq..]
          recExpr      = predListToExpr xsRest (i + length xsEq + length xsComplex)
          allExprs     = eqExprs
                         ++ complexExprs
                         ++ [recExpr]
      in foldr1 AndE allExprs
         where
          fromSingleton rs | [(l,h)] <- RS.ranges rs, l == h = l
                           | otherwise = error "not a singleton rangeset"

          predToExpr rs j = case map rangeTest (RS.ranges rs) of
                              []     -> FalseE
                              ranges -> foldr1 OrE ranges
              where
                rangeTest (l, h)
                  | l == h    = EqE (SymE j) (ConstE $ fromEnum l)
                  | otherwise = AndE (LteE (ConstE $ fromEnum l) (SymE j))
                                     (LteE (SymE j) (ConstE $ fromEnum h))

instance (Eq a, Enum a) => PredicateListToExpr (CodeInputLab a) where
  predListToExpr xs i
    | all (== InputAny 1) xs = TrueE
    | all isConstDigit xs = CompareE i [ fromEnum b | InputConst [b] <- xs ]
    | otherwise = error $ "input labels not normalized"
    where
      isConstDigit (InputConst [_]) = True
      isConstDigit _ = False

instance (Enum digit) => PredicateListToExpr (ConstOrAnyLab digit) where
  predListToExpr xs i = case concat $ zipWith testE xs [i..] of
                          [] -> TrueE
                          ys -> foldr1 AndE ys
    where
      testE AnyLab _ = []
      testE (ConstLab c) j = [EqE (SymE j) (ConstE (fromEnum c))]

instance (Enum digit, Enum sigma, Bounded digit, Ord enum, Enumerable enum sigma)
         => CompilableFunction (DecodeFunc enum digit (Identity sigma)) enum where
  tables (DecodeConst _) = M.empty
  tables (DecodeArg es) =
    M.fromList [ (e, Table [ [fromEnum (lookupIndex i e)] | i <- [0 .. size e - 1] ] 1) | e <- es ]
  funcConstants (DecodeConst cs) = [map (fromEnum . runIdentity) cs]
  funcConstants (DecodeArg _) = []
  compileFuncAppend bid (DecodeConst cs)
    = (:[]) . AppendI bid <$> ((M.!) <$> asks cmap <*> pure (map (fromEnum . runIdentity) cs))
  compileFuncAppend bid (DecodeArg es) = zipWithM aux [0..] es
    where
      aux i e = AppendTblI bid <$> ((M.!) <$> asks tmap <*> pure e) <*> pure i

instance (Bounded dom, Enum dom, Bounded digit, Enum digit, Enumerable enum dom, Ord enum)
         => CompilableFunction [EpsFunc (CodeFunc enum dom digit)] enum where
  tables xs = M.fromList [ (e, tbl e) | JustFunc (CodeArg e) <- xs]
    where
      tbl (e :: enum) =
        let bits = bitWidth (boundedSize (undefined :: digit)) (size e)
            code (x :: dom) | member x e =
              let digits = codeFixedWidthEnumSized (size e) (indexOf x e) :: [digit]
               in map fromEnum digits
            code _ = replicate bits 0
         in Table [ code x | x <- [minBound .. (maxBound :: dom)] ] bits
  funcConstants xs = [ map fromEnum ds | JustFunc (CodeConst ds) <- xs ]
  compileFuncAppend bid xs = concat <$> zipWithM aux [0..] xs
    where
      aux _ EpsFunc = pure []
      aux _ (JustFunc (CodeConst ds))
        = ((:[]) . AppendI bid) <$> ((M.!) <$> asks cmap <*> pure (map fromEnum ds))
      aux i (JustFunc (CodeArg e))
        = (:[]) <$> (AppendTblI bid <$> ((M.!) <$> asks tmap <*> pure e) <*> pure i)

instance (Bounded sigma, Enum sigma)
         => CompilableFunction [EpsFunc (CopyFunc sigma [Identity sigma])] () where
  tables _ = M.empty
  funcConstants fs = [ [fromEnum $ runIdentity y | y <- ys] | JustFunc (CopyConst ys) <- fs ]
  compileFuncAppend bid fs = concat <$> zipWithM aux [0..] fs
    where
      aux _ EpsFunc = pure []
      aux i (JustFunc CopyArg)
        = return [AppendSymI bid i]
      aux _ (JustFunc (CopyConst ys))
        = ((:[]) . AppendI bid) <$> ((M.!) <$> asks cmap <*> pure (map (fromEnum . runIdentity) ys))
