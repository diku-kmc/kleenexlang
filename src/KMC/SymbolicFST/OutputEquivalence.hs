{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module KMC.SymbolicFST.OutputEquivalence where

import           Data.Map (Map, (!))
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S

import           KMC.SymbolicFST
import           KMC.SymbolicFST.Transducer (Transducer, CopyFunc(..))
import           KMC.SymbolicFST.OracleMachine (OracleMachine)
import           KMC.SymbolicFST.ActionMachine (ActionMachine)
import           KMC.Theories

-- | Takes a transducer and forgets all transitions with non-epsilon output
-- labels, and considers the result as a control-flow graph. Based on this
-- graph, computes a map which maps each state to its set of /post-dominators/,
-- defined by the following data-flow equation:
--   PDom(p) = ⋂{ PDom(q) | p --> q } ∪ { p }
postDominators :: (Ord st, Function (CopyFunc sigma [gamma]), Rng (CopyFunc sigma [gamma]) ~ [gamma'])
           => Transducer st sigma gamma -> Map st (Set st)
postDominators fst' = do
  let dom0 = M.fromList [ (s, fstS fst') | s <- S.toList $ fstS fst' ] in go dom0
  where
    succs s = [ t | ([],t) <- fstEvalEpsilonEdges fst' s ]
              ++ [ t | (_,f,t) <- fromMaybe [] (M.lookup s $ eForward $ fstE fst')
                     , Just [] <- [isConst f] ]
    newDoms dom s =
      let psets = [ dom!t | t <- succs s ]
      in if null psets then
           S.singleton s
         else
           S.insert s $ foldl1 S.intersection psets
    go dom =
      let dom' = M.fromList [ (s, newDoms dom s) | s <- S.toList $ fstS fst' ]
      in if dom == dom' then dom else go dom'

-- | Given a post dominator map, computes for each state s the /last/
-- post-dominator lpdom(s). This is the unique l ∈ pdom(s) such that l≠s and for
-- all z ∈ pdom(s) where z ≠ s we have l ∈ pdom(z).
lastPostDominator :: (Ord st) => Map st (Set st) -> st -> Maybe st
lastPostDominator pdom =
  let lpdom = M.fromList [ (s, t) | (s, pdoms) <- M.toList pdom
                                  , t <- findlastpdom s pdoms (S.toList pdoms) ]
      findlastpdom _ _ [] = []
      findlastpdom s pdoms (t:ts)
        | all (\t' -> t' == t || t' == s || S.member t (pdom!t')) pdoms = [t]
        | otherwise = findlastpdom s pdoms ts
  in \x -> M.lookup x lpdom

optAction :: (Ord st) => (st -> Maybe st) -> ActionMachine st sigma act digit -> ActionMachine st sigma act digit
optAction lpdom fst' = fst' { fstE = edgesFromList $ map aux $ edgesToList $ fstE fst' }
  where
    aux (p, lab, q) | Just q' <- lpdom q = (p, lab, q')
                    | otherwise          = (p, lab, q)

optOracle :: (Ord st) => (st -> Maybe st) -> OracleMachine st sigma digit -> OracleMachine st sigma digit
optOracle lpdom fst' = fst' { fstE = edgesFromList $ map aux $ edgesToList $ fstE fst' }
  where
    aux (p, lab, q) | Just p' <- lpdom p, p /= p' = (p, either Left (const (Right [])) lab, q)
                    | otherwise                   = (p, lab, q)
