module KMC.DFAConstruction (dfaFromMu, minimizeDFA, nfaToDFA, mergeEdges) where

import           Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Word

import           KMC.SymbolicAcceptor
import           KMC.RangeSet
import           KMC.NFAConstruction (nfa1,nfa2,nfaFromMu)
import           KMC.Theories
import           KMC.Expression
import           KMC.Util.Set (joinSets, occurs)
import           KMC.Util.List (foldr1ifEmpty)

dfaFromMu :: (Predicate pred, Enum st, Ord st)
          => Mu pred f st -> DFA (S.Set st) pred
dfaFromMu = nfaToDFA . nfaFromMu

data DFAConstructState st pred =
    DFAConstructState { edges  :: [DFAEdge st pred]
                      , states :: S.Set st
                      }
type DFAConstruct st pred = State (DFAConstructState st pred)

-- | Brzozowski's minimization algorithm:
--   1) reverse DFA D0 to get NFA N0 for reverse L
--   2) determinize N0 to get DFA D1 for reverse L
--   3) reverse D1 to get NFA N1 for non-reverse L
--   4) determinize N1 to get minimal DFA D2 for non-reverse L.
-- This algorithm runs in worst-case exponential time.
-- TODO: Implement more efficient algorithm.
minimizeDFA :: (Predicate pred, Ord st)
            => DFA st pred -> DFA Int pred
minimizeDFA = enumerateDFAStates . nfaToDFA . reverseDFA . nfaToDFA . reverseDFA


-- | Merge all edges between the same states in the DFA by OR'ing their
-- predicates together.
mergeEdges :: (Predicate pred, Ord st)
           => DFA st pred -> DFA st pred
mergeEdges (DFA dfa) = DFA $ dfa { accE = dfaEdgesFromList $ merge gathered }
    where
      edges = dfaEdgesToList $ accE dfa
      merge = map (\(q, ps, q') -> (q, mergePreds ps, q'))
      gathered = [ (q, ps, q') | (q, q') <- connected
                               , let ps = preds $ filter (isBetween q q') edges
                              ]
      preds = map (\(_,p,_) -> p)
      isBetween q q' (st1, _, st2) = q == st1 && q' == st2
      connected = S.toList $ S.fromList [ (q, q') | (q, _, q') <- edges ]

mergePreds :: (Predicate pred) => [pred] -> pred
mergePreds = foldr1ifEmpty disj bot

addState :: (Ord st) => st -> DFAConstruct st pred ()
addState q = modify (\s -> s { states = q `S.insert` (states s) })

addEdge :: st -> st -> pred -> DFAConstruct st pred ()
addEdge q q' p = modify (\s -> s { edges = (q, p, q') : (edges s) })

-- | Determinize NFA with subset construction.
nfaToDFA :: (Predicate pred, Ord st) => NFA st pred -> DFA (S.Set st) pred
nfaToDFA (NFA nfa) =
    let init = epsilonClosures (NFA nfa) (accI nfa)
        s = execState (toDFA (NFA nfa) init) (DFAConstructState [] (S.singleton init))
    in DFA $ Acceptor { accS = states s
                      , accE = dfaEdgesFromList (edges s)
                      , accI = Single init
                      , accF = occurs (accF nfa) (states s)
                      }
    
toDFA :: (Predicate pred, Ord st)
      => NFA st pred -> S.Set st -> DFAConstruct (S.Set st) pred ()
toDFA nfa dfaQ =
    let preds = coarsestPredicateSet nfa (S.toList dfaQ)
        newQs = [ (stepAll nfa pred dfaQ, pred) | pred <- preds ]
    in do
      oldQs <- gets states
      mapM_ (addState . fst) newQs
      mapM_ (uncurry (addEdge dfaQ)) newQs
      mapM_ (toDFA nfa) [ q | (q, _) <- newQs, not $ q `S.member` oldQs ]


stepAll :: (Predicate pred, Ord st) => NFA st pred -> pred -> S.Set st -> S.Set st
stepAll (NFA nfa) p = joinSets . S.map aux
    where
      aux q = epsilonClosures (NFA nfa) $
              stepPred (accE nfa) q p
      stepPred es q p =
          case M.lookup q (nfaForward es) of
            Nothing -> S.empty
            Just ns -> S.fromList [ q' | (p', q') <- S.toList ns
                                       , p `agreesWith` p' ]

agreesWith :: (Predicate pred) => pred -> pred -> Bool
agreesWith p p' = not $ (p `conj` p') `eq` bot

epsilonClosures :: (Ord st) => NFA st pred -> S.Set st -> S.Set st
epsilonClosures (NFA nfa) = joinSets . S.map epsClos
    where
      epsEdges = nfaForwardEpsilon (accE nfa)
      epsClos q = maybe (S.singleton q)
                        (S.insert q . epsilonClosures (NFA nfa))
                        $ M.lookup q epsEdges


-- | Identical to `coarsestPredicateSet` in KMC.SymbolicSST except that this
-- one is specialized to NFAs.
coarsestPredicateSet :: (Predicate pred, Ord st)
                     => NFA st pred -> [st] -> [pred]
coarsestPredicateSet (NFA nfa) qs = coarsestPartition ps
    where
    ps = S.toList $ S.fromList
         [ p | q <- qs
             , (p, _) <- maybe [] S.toList (M.lookup q (nfaForward $ accE nfa)) ]

