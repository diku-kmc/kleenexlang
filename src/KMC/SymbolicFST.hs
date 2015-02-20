{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
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
data FST q pred func =
  FST
  { fstS :: S.Set q
  , fstE :: OrderedEdgeSet q pred func
  , fstI :: q
  , fstF :: S.Set q
  }

type Edge q pred func = (q, Either (pred, func) (Rng func), q)

data OrderedEdgeSet q pred func =
  OrderedEdgeSet { eForward :: M.Map q [(pred,func,q)]
                 , eBackward :: M.Map q [(pred,func,q)]
                 , eForwardEpsilon :: M.Map q [(Rng func, q)]
                 , eBackwardEpsilon :: M.Map q [(Rng func, q)]
                 }

deriving instance (Eq q, Eq pred, Eq func, Eq (Rng func)) => Eq (OrderedEdgeSet q pred func)
deriving instance (Ord q, Ord pred, Ord func, Ord (Rng func)) => Ord (OrderedEdgeSet q pred func)
deriving instance (Show q, Show pred, Show func, Show (Rng func)) => Show (OrderedEdgeSet q pred func)
deriving instance (Eq q, Eq pred, Eq func, Eq (Rng func)) => Eq (FST q pred func)
deriving instance (Ord q, Ord pred, Ord func, Ord (Rng func)) => Ord (FST q pred func)
deriving instance (Show q, Show pred, Show func, Show (Rng func)) => Show (FST q pred func)

edgesFromList :: (Ord q) => [Edge q pred func] -> OrderedEdgeSet q pred func
edgesFromList es =
  OrderedEdgeSet
  { eForward = M.fromListWith (++) [ (q, [(a, b, q')]) | (q, Left (a,b), q') <- es ]
  , eBackward = M.fromListWith (++) [ (q', [(a, b, q)]) | (q, Left (a,b), q') <- es ]
  , eForwardEpsilon = M.fromListWith (++) [(q, [(y, q')]) | (q, Right y, q') <- es ]
  , eBackwardEpsilon = M.fromListWith (++) [(q', [(y,q)]) | (q, Right y, q') <- es ]
  }

unionEdges :: (Ord q) => OrderedEdgeSet q pred func
           -> OrderedEdgeSet q pred func
           -> OrderedEdgeSet q pred func
unionEdges es es' =
  OrderedEdgeSet
  { eForward  = M.unionWith (++) (eForward es) (eForward es')
  , eBackward = M.unionWith (++) (eBackward es) (eBackward es')
  , eForwardEpsilon = M.unionWith (++) (eForwardEpsilon es) (eForwardEpsilon es')
  , eBackwardEpsilon = M.unionWith (++) (eBackwardEpsilon es) (eBackwardEpsilon es')
  }

edgesToList :: OrderedEdgeSet q pred func -> [(q, Either (pred, func) (Rng func), q)]
edgesToList es =
  [ (q, Left (a,b), q') | (q, xs) <- M.toList (eForward es), (a,b,q') <- xs ]
  ++ [ (q, Right y, q') | (q, xs) <- M.toList (eForwardEpsilon es), (y, q') <- xs ]

evalEdges :: (Ord st
             ,Function func
             ,SetLike pred (Dom func))
          => OrderedEdgeSet st pred func
          -> st -> Dom func -> [(Rng func, st)]
evalEdges (OrderedEdgeSet { eForward = me }) q x =
  case M.lookup q me of
    Nothing -> []
    Just es -> concatMap evalEdge es
      where
        evalEdge (p, f, q')
          | member x p = [(eval f x, q')]
          | otherwise = []

abstractEvalEdges :: (Ord st
                     ,PartialOrder pred)
                  => OrderedEdgeSet st pred func
                  -> st -> pred -> [(func, st)]
abstractEvalEdges (OrderedEdgeSet { eForward = me}) q p =
  case M.lookup q me of
    Nothing -> []
    Just es -> [ (f, q') | (p', f, q') <- es, p `lte` p' ]

evalEpsilonEdges :: (Ord st) => OrderedEdgeSet st pred func
                 -> st -> [(Rng func, st)]
evalEpsilonEdges (OrderedEdgeSet { eForwardEpsilon = meps }) q =
  case M.lookup q meps of
    Nothing -> []
    Just es -> es

fstEvalEpsilonEdges :: (Ord st) => FST st pred func -> st -> [(Rng func, st)]
fstEvalEpsilonEdges aut = evalEpsilonEdges (fstE aut)

fstEvalEdges :: (Ord st
                ,Function func
                ,SetLike pred (Dom func))
                => FST st pred func -> st -> Dom func -> [(Rng func, st)]
fstEvalEdges fst' q a = evalEdges (fstE fst') q a

fstAbstractEvalEdges :: (Ord st
                        ,PartialOrder pred)
                     => FST st pred func -> st -> pred -> [(func, st)]
fstAbstractEvalEdges aut = abstractEvalEdges (fstE aut)

-- | Is the given state a non-deterministic choice state, or an input action
-- state?
isChoiceState :: (Ord st) => FST st pred func -> st -> Bool
isChoiceState fst' q =
  case (M.member q (eForward . fstE $ fst'), M.member q (eForwardEpsilon . fstE $ fst')) of
    (True, True) -> error "Inconsistent FST - a state is both a choice and symbol state"
    (_, b) -> b

coarsestPredicateSet :: (Boolean pred
                        ,PartialOrder pred
                        ,Ord st
                        ,Ord pred) =>
                        FST st pred func
                     -> [st]
                     -> [pred]
coarsestPredicateSet fst' qs = coarsestPartition ps
  where
    ps = S.toList $ S.fromList
           [ p | q <- qs
               , (p, _, _) <- maybe [] id (M.lookup q (eForward . fstE $ fst')) ]

{-- LCP analysis --}

-- | Compute the (right-projective) epsilon-closure from a given set of
-- states. The result is the set of all states reachable by epsilon-transitions
-- which have no outgoing epsilon-transitions.
epsilonClosure :: (Ord st) => FST st pred func -> S.Set st -> S.Set st
epsilonClosure fst' set = S.fromList $ concatMap (go S.empty) (S.toList set)
    where
      go vis q =
        let succs = map snd $ fstEvalEpsilonEdges fst' q in
        if null succs then
            [q]
        else
            let vis' = S.union vis (S.fromList succs) in
            concatMap (go vis') $ filter (not . flip S.member vis) succs

next :: (Ord st, Boolean pred, PartialOrder pred) => FST st pred func -> S.Set st -> S.Set st
next fst' set = epsilonClosure fst' $ S.unions $ map go $ S.toList set
    where
      go q = S.fromList $ map snd $ fstAbstractEvalEdges fst' q bot

-- | The conjunction of all predicates on outgoing transitions from all states
-- in the given set.
commonPred :: (Boolean pred, Ord st) => FST st pred func -> S.Set st -> pred
commonPred fst' set =
  foldr conj bot $ do
    q <- S.toList set
    return $ foldr conj bot
               [ p | Just xs <- [M.lookup q (eForward $ fstE fst')]
                   , (p, _, _) <- xs ]

lcp :: (Boolean pred, PartialOrder pred, Ord st) => FST st pred func -> st -> [pred]
lcp fst' q = go (epsilonClosure fst' $ S.singleton q)
    where go set =
              let c = commonPred fst' set in
              if c `eq` bot then
                  []
              else
                  c:go (next fst' set)

{-- Simulation --}

run :: (Function func
       ,SetLike pred (Dom func)
       ,Monoid (Rng func)
       ,Ord st)
       => FST st pred func -> [Dom func] -> [Rng func]
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
