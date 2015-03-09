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

-- | If abstractEvalEdgesAll fst q phi == [(f1, q1), ..., (fn, qn)], then
--   for all qi and for all a in [[phi]], q steps to qi reading a.
abstractEvalEdgesAll :: (Ord st
                        ,PartialOrder pred)
                       =>
                       OrderedEdgeSet st pred func
                     -> st -> pred -> [(func, st)]
abstractEvalEdgesAll (OrderedEdgeSet { eForward = me}) q p =
  case M.lookup q me of
    Nothing -> []
    Just es -> [ (f, q') | (p', f, q') <- es, p `lte` p' ]

-- | If abstractEvalEdgesAny fst q phi == [(f1, q1) ..., (fn, qn)], then
--   for all qi there exists an a in [[phi]] such that q steps to qi reading a.
abstractEvalEdgesAny :: (Ord st, PartialOrder pred, Boolean pred)
                        =>
                        OrderedEdgeSet st pred func
                     -> st -> pred -> [(func, st)]
abstractEvalEdgesAny (OrderedEdgeSet { eForward = me}) q p =
  case M.lookup q me of
    Nothing -> []
    Just es -> [ (f, q') | (p', f, q') <- es, not $ (p `conj` p') `eq` bot ]

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

fstAbstractEvalEdgesAll :: (Ord st
                           ,PartialOrder pred)
                          => FST st pred func -> st -> pred -> [(func, st)]
fstAbstractEvalEdgesAll aut = abstractEvalEdgesAll (fstE aut)

fstAbstractEvalEdgesAny :: (Ord st
                           ,PartialOrder pred, Boolean pred)
                          => FST st pred func -> st -> pred -> [(func, st)]
fstAbstractEvalEdgesAny aut = abstractEvalEdgesAny (fstE aut)

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

-- | Compute an unordered right epsilon closure on the input automaton of an FST.
rightClosure :: (Ord st) => FST st pred func -> st -> S.Set st
rightClosure fst' = snd . go S.empty
    where
      go vis q =
          foldl (\(vis', acc) q' ->
                     if S.member q' vis' then
                         (vis', acc)
                     else
                         let (vis'', ys) = go (S.insert q' vis') q' in
                         (vis'', S.union acc ys))
                (vis, S.singleton q)
                (map snd $ fstEvalEpsilonEdges fst' q)

stepAll :: (Ord st, PartialOrder pred) => FST st pred func -> pred -> S.Set st -> S.Set st
stepAll fst' p = S.unions . map aux . S.toList
    where
      aux q = S.unions $ map (rightClosure fst' . snd) $ fstAbstractEvalEdgesAll fst' q p

{-- LCP analysis --}

ldp :: (Ord st, Ord pred, PartialOrder pred, Boolean pred) =>
       FST st pred func -> S.Set st -> st -> [pred]
ldp fst' = go
    where
      go ctx q =
          case M.lookup q (eForward $ fstE fst') of
            Just [(p,_,q')] | p `elem` (coarsestPredicateSet fst' $ S.toList ctx) ->
                                p:go (stepAll fst' p ctx) q'
            _ -> []

prefixTests :: (Boolean pred, PartialOrder pred, Ord st, Ord pred) =>
               FST st pred func
            -> Bool
            -> [st]
            -> [([pred], S.Set st)]
prefixTests fst' singletonMode states =
  [ (t, killed t) | t <- tests ]
  where
    ldps = [ (ldp fst' (S.fromList states) q, q) | not singletonMode, q <- states ]
    tests = S.toList $ S.fromList $ [ [p] | p <- coarsestPredicateSet fst' states ] ++ map fst ldps
    killed t = S.fromList [ q | (ps, q) <- ldps, not (t `entails` ps) ]

    entails _ [] = True
    entails (t:ts) (p:ps) = (t `eq` p) && (ts `entails` ps)
    entails [] (_:_) = False

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
