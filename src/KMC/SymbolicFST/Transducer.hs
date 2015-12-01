{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module KMC.SymbolicFST.Transducer
       (Transducer,CopyFunc(..),constructTransducer)
       where

import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           KMC.Kleenex.Syntax (RIdent, RProg(..), RTerm(..))
import           KMC.RangeSet (RangeSet)
import           KMC.SymbolicFST (FST(..), edgesFromList)
import           KMC.Theories (Function(..))

type Transducer st sigma gamma
  = FST st (RangeSet sigma) (CopyFunc sigma [gamma])

-----------------------------------------------------------
-- Predicate and function types for symbolic representation
-----------------------------------------------------------

-- | Represents functions which inject their argument into the range type. The
-- functions may also ignore their argument and return a constant.
data CopyFunc a c = CopyArg | CopyConst c
  deriving (Eq, Ord)

instance (Enum a, Bounded a) => Function (CopyFunc a [Either a x]) where
  type Dom (CopyFunc a [Either a x]) = a
  type Rng (CopyFunc a [Either a x]) = [Either a x]
  eval CopyArg x = [Left x]
  eval (CopyConst y) _ = y
  isConst CopyArg = Nothing
  isConst (CopyConst y) = Just y
  inDom _ _ = True
  domain _ = [minBound .. maxBound]

---------------------------------------------
-- Construction of action and oracle machines
---------------------------------------------

-- | Converts a Kleenex program to a transducer.
constructTransducer
  :: (Rng (CopyFunc a [b]) ~ [b]) => RProg a b -> RIdent -> Transducer [RIdent] a b
constructTransducer rprog initial =
  FST { fstS = allStates
      , fstE = edgesFromList allTransitions
      , fstI = [initial]
      , fstF = S.singleton []
      }
  where
    (allStates, allTransitions) = go (S.singleton [initial]) S.empty []
    go ws states trans
      | S.null ws                         = (states, trans)
      | (q, ws') <- S.deleteFindMin ws, S.member q states
                                          = go ws' states trans
      | ([], ws') <- S.deleteFindMin ws   = go ws' (S.insert [] states) trans
      | (i:is, ws') <- S.deleteFindMin ws =
          let states' = S.insert (i:is) states in
          case getDecl i of
          RConst y      ->
            let q' = follow is
            in go (S.insert q' ws') states' ((i:is, Right [y], q'):trans)
          RRead p False ->
            let q' = follow is
            in go (S.insert q' ws') states' ((i:is, Left (p, CopyConst []), q'):trans)
          RRead p True  ->
            let q' = follow is
            in go (S.insert q' ws') states'((i:is, Left (p, CopyArg), q'):trans)
          RSeq js       ->
            let q' = follow (js ++ is)
            in go (S.insert q' ws') states' ((i:is, Right [], q'):trans)
          RSum js       ->
            let qs' = [follow (j:is) | j <- js]
                -- Indexed epsilons are implicitly represented by the transition order
                trans' = [(i:is, Right [], q') | q' <- qs']
            in go (S.union (S.fromList qs') ws') states' (trans' ++ trans)
      | otherwise = error "impossible"

    -- Optimization: Reduce number of generated states by contracting
    -- non-deterministic edges with no output. This is done by "skipping" states
    -- whose head nonterminal is declared to be a Seq term, or an RSum with only
    -- one successor.
    follow [] = []
    follow (i:is) =
      case getDecl i of
      RSeq js -> follow (js ++ is)
      RSum [j] -> follow (j:is)
      _ -> i:is

    getDecl i =
      fromMaybe (error $ "internal error: identifier without declaration: " ++ show i)
        $ M.lookup i (rprogDecls rprog)
