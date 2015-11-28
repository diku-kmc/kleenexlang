{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ParallelListComp #-}
module KMC.SymbolicFST.OracleMachine where

import KMC.RangeSet (RangeSet)
import KMC.SymbolicFST (FST, mapEdges)
import KMC.Theories (Function(..), Enumerable(..), SetLike(..))
import KMC.SymbolicFST.Transducer (Transducer, CopyFunc(..))
import KMC.Util.Coding

type OracleMachine st sigma digit
  = FST st (RangeSet sigma) (CodeFunc (RangeSet sigma) sigma digit)

-----------------------------------------------------------
-- Predicate and function types for symbolic representation
-----------------------------------------------------------

-- | Represents functions which code values as fixed-width codes in an arbitrary
-- base. The functions may also ignore their arguments and return a constant
-- code.
data CodeFunc enum dom digit = CodeArg enum | CodeConst [digit]
  deriving (Eq, Ord)

instance (Enum dom, Bounded dom, Enum digit, Bounded digit, Enumerable enum dom)
         => Function (CodeFunc enum dom digit) where
  type Dom (CodeFunc enum dom digit) = dom
  type Rng (CodeFunc enum dom digit) = [digit]
  eval (CodeArg p) x = codeFixedWidthEnumSized (size p) (indexOf x p)
  eval (CodeConst y) _ = y
  isConst (CodeArg p) = if size p == 1 then
                          Just $ eval (CodeArg p) (lookupIndex 0 p)
                        else
                          Nothing
  isConst (CodeConst y) = Just y
  inDom x (CodeArg p) = member x p
  inDom _ (CodeConst _) = True
  domain (CodeArg p) = [lookupIndex i p | i <- [0 .. size p - 1]]
  domain (CodeConst _) = [minBound .. maxBound]

-----------------------------
-- Oracle macine construction
-----------------------------

-- | Get the underlying oracle for a transducer. This is obtained by removing
-- output labels on symbol transitions; and adding bit code outputs to every
-- non-deterministic transition.
oracle :: forall st sigma act digit.
          (Ord st, Ord sigma, Enum sigma, Bounded sigma, Enum digit, Bounded digit)
       => Transducer st sigma act -> OracleMachine st sigma digit
oracle = mapEdges symsym symeps epssym epseps
  where
    symsym _q ts = [ (p, f, q') | (p, CopyArg, q') <- ts
                                , let f = if size p > 1 then CodeArg p else CodeConst [] ]
                   ++ [ (p, CodeConst [], q') | (p, CopyConst _, q') <- ts ]
    symeps _q _ts = []
    epssym _q _ts = []
    epseps _q [(_y, q')] = ([([], q')])
    epseps _q ts = let n = length ts
                   in [ (codeFixedWidthEnumSized n ix, q') | ([], q') <- ts | ix <- [0..] ]
