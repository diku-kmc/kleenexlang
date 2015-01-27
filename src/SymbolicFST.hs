module SymbolicFST where

import qualified Data.Set as S
import qualified Data.Map as M

import           Theories

data FST q pred func rng =
  FST
  { fstS :: S.Set q
  , fstE :: OrderedEdgeSet q pred func rng
  , fstI :: q
  , fstF :: S.Set q
  }
  deriving (Eq, Ord, Show)

type Edge q pred func rng = (q, Either (pred, func) rng, q)

data OrderedEdgeSet q pred func rng =
  OrderedEdgeSet { eForward :: M.Map q [(pred,func,q)]
                 , eBackward :: M.Map q [(pred,func,q)]
                 , eForwardEpsilon :: M.Map q [(rng, q)]
                 , eBackwardEpsilon :: M.Map q [(rng, q)]
                 }
  deriving (Eq, Ord, Show)

edgesFromList :: (Ord q) => [Edge q pred func rng] -> OrderedEdgeSet q pred func rng
edgesFromList es =
  OrderedEdgeSet
  { eForward = M.fromListWith (++) [ (q, [(a, b, q')]) | (q, Left (a,b), q') <- es ]
  , eBackward = M.fromListWith (++) [ (q', [(a, b, q)]) | (q, Left (a,b), q') <- es ]
  , eForwardEpsilon = M.fromListWith (++) [(q, [(y, q')]) | (q, Right y, q') <- es ]
  , eBackwardEpsilon = M.fromListWith (++) [(q', [(y,q)]) | (q, Right y, q') <- es ]
  }

unionEdges :: (Ord q) => OrderedEdgeSet q pred func rng
           -> OrderedEdgeSet q pred func rng
           -> OrderedEdgeSet q pred func rng
unionEdges es es' =
  OrderedEdgeSet
  { eForward  = M.unionWith (++) (eForward es) (eForward es')
  , eBackward = M.unionWith (++) (eBackward es) (eBackward es')
  , eForwardEpsilon = M.unionWith (++) (eForwardEpsilon es) (eForwardEpsilon es')
  , eBackwardEpsilon = M.unionWith (++) (eBackwardEpsilon es) (eBackwardEpsilon es')
  }

edgesToList :: OrderedEdgeSet q pred func rng -> [(q, Either (pred, func) rng, q)]
edgesToList es =
  [ (q, Left (a,b), q') | (q, xs) <- M.toList (eForward es), (a,b,q') <- xs ]
  ++ [ (q, Right y, q') | (q, xs) <- M.toList (eForwardEpsilon es), (y, q') <- xs ]


evalEdges :: (Ord st, EffBoolean pred dom, Function func dom rng)
          => OrderedEdgeSet st pred func rng
          -> st -> dom -> [(rng, st)]
evalEdges (OrderedEdgeSet { eForward = me }) q x =
  case M.lookup q me of
    Nothing -> []
    Just es -> concatMap evalEdge es
      where
        evalEdge (p, f, q')
          | evalBoolean p x = [(evalFunction f x, q')]
          | otherwise = []

evalEpsilonEdges :: (Ord st) => OrderedEdgeSet st pred func rng
                 -> st -> [(rng, st)]
evalEpsilonEdges (OrderedEdgeSet { eForwardEpsilon = meps }) q =
  case M.lookup q meps of
    Nothing -> []
    Just es -> es
