module KMC.SymbolicFST where

import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Set as S

import           KMC.Theories


-- | (Non-)deterministic Finite State Transducer. To be well-formed, the
-- following conditions must be met:
--
-- (i) For any state s, the only transitions out of s are epsilon-transitions or
-- symbol transitions, but not both.
data FST q pred func delta =
  FST
  { fstS :: S.Set q
  , fstE :: OrderedEdgeSet q pred func delta
  , fstI :: q
  , fstF :: S.Set q
  }
  deriving (Eq, Ord, Show)

type Edge q pred func delta = (q, Either (pred, func) delta, q)

data OrderedEdgeSet q pred func delta =
  OrderedEdgeSet { eForward :: M.Map q [(pred,func,q)]
                 , eBackward :: M.Map q [(pred,func,q)]
                 , eForwardEpsilon :: M.Map q [(delta, q)]
                 , eBackwardEpsilon :: M.Map q [(delta, q)]
                 }
  deriving (Eq, Ord, Show)

edgesFromList :: (Ord q) => [Edge q pred func delta] -> OrderedEdgeSet q pred func delta
edgesFromList es =
  OrderedEdgeSet
  { eForward = M.fromListWith (++) [ (q, [(a, b, q')]) | (q, Left (a,b), q') <- es ]
  , eBackward = M.fromListWith (++) [ (q', [(a, b, q)]) | (q, Left (a,b), q') <- es ]
  , eForwardEpsilon = M.fromListWith (++) [(q, [(y, q')]) | (q, Right y, q') <- es ]
  , eBackwardEpsilon = M.fromListWith (++) [(q', [(y,q)]) | (q, Right y, q') <- es ]
  }

unionEdges :: (Ord q) => OrderedEdgeSet q pred func delta
           -> OrderedEdgeSet q pred func delta
           -> OrderedEdgeSet q pred func delta
unionEdges es es' =
  OrderedEdgeSet
  { eForward  = M.unionWith (++) (eForward es) (eForward es')
  , eBackward = M.unionWith (++) (eBackward es) (eBackward es')
  , eForwardEpsilon = M.unionWith (++) (eForwardEpsilon es) (eForwardEpsilon es')
  , eBackwardEpsilon = M.unionWith (++) (eBackwardEpsilon es) (eBackwardEpsilon es')
  }

edgesToList :: OrderedEdgeSet q pred func delta -> [(q, Either (pred, func) delta, q)]
edgesToList es =
  [ (q, Left (a,b), q') | (q, xs) <- M.toList (eForward es), (a,b,q') <- xs ]
  ++ [ (q, Right y, q') | (q, xs) <- M.toList (eForwardEpsilon es), (y, q') <- xs ]

evalEdges :: (Ord st, SetLike pred dom, Function func dom delta)
          => OrderedEdgeSet st pred func delta
          -> st -> dom -> [(delta, st)]
evalEdges (OrderedEdgeSet { eForward = me }) q x =
  case M.lookup q me of
    Nothing -> []
    Just es -> concatMap evalEdge es
      where
        evalEdge (p, f, q')
          | member x p = [(evalFunction f x, q')]
          | otherwise = []

abstractEvalEdges :: (Ord st, PartialOrder pred)
                  => OrderedEdgeSet st pred func delta
                  -> st -> pred -> [(func, st)]
abstractEvalEdges (OrderedEdgeSet { eForward = me}) q p =
  case M.lookup q me of
    Nothing -> []
    Just es -> [ (f, q') | (p', f, q') <- es, p `lte` p' ]

evalEpsilonEdges :: (Ord st) => OrderedEdgeSet st pred func delta
                 -> st -> [(delta, st)]
evalEpsilonEdges (OrderedEdgeSet { eForwardEpsilon = meps }) q =
  case M.lookup q meps of
    Nothing -> []
    Just es -> es

fstEvalEpsilonEdges :: (Ord st) => FST st pred func delta -> st -> [(delta, st)]
fstEvalEpsilonEdges aut = evalEpsilonEdges (fstE aut)

fstEvalEdges :: (Ord st, SetLike pred dom, Function func dom delta)
                => FST st pred func delta -> st -> dom -> [(delta, st)]
fstEvalEdges fst' q a = evalEdges (fstE fst') q a

fstAbstractEvalEdges :: (Ord st, PartialOrder pred)
                     => FST st pred func delta -> st -> pred -> [(func, st)]
fstAbstractEvalEdges aut = abstractEvalEdges (fstE aut)

-- | Is the given state a non-deterministic choice state, or an input action
-- state?
isChoiceState :: (Ord st) => FST st pred func delta -> st -> Bool
isChoiceState fst' q =
  case (M.member q (eForward . fstE $ fst'), M.member q (eForwardEpsilon . fstE $ fst')) of
    (True, True) -> error "Inconsistent FST - a state is both a choice and symbol state"
    (_, b) -> b

coarsestPredicateSet :: (Boolean pred, PartialOrder pred, Ord st, Ord pred) =>
                        FST st pred func delta
                     -> [st]
                     -> [pred]
coarsestPredicateSet fst' qs = coarsestPartition ps
  where
    ps = S.toList $ S.fromList
           [ p | q <- qs
               , (p, _, _) <- maybe [] id (M.lookup q (eForward . fstE $ fst')) ]

run :: (Monoid delta, SetLike pred dom, Function func dom delta, Ord st)
       => FST st pred func delta -> [dom] -> [delta]
run fst' = go S.empty (fstI fst')
    where
      go _ q [] =
        [ mempty | S.member q (fstF fst') ]
      go vis q w | isChoiceState fst' q =
        [ mappend o o' | (o, q') <- fstEvalEpsilonEdges fst' q
                       , not (S.member q' vis)
                       , o' <- go (S.insert q' vis) q' w ]
      go _ q (a:as) =
        [ mappend o o' | (o, q') <- fstEvalEdges fst' q a, o' <- go S.empty q' as ]
