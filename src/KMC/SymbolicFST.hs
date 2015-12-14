{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module KMC.SymbolicFST
       (FST(..)
       ,Edge
       ,fstStateSize
       ,fstTransSize
       ,edgesToList
       ,edgesFromList
       ,fstEvalEpsilonEdges
       ,fstAbstractEvalEdgesAll
       ,coarsestPredicateSet
       ,prefixTests
       ,rightClosure,rightInputClosure
       ,mapEdges
       ,eForward,eBackward,eForwardEpsilon,eBackwardEpsilon
       ,enumerateStates
       ,run
       ,runSequential
       ) where

import           Control.Monad (guard)
import qualified Data.Map as M
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

-- | Type synonym for a transition. A transition is either labeled by a
-- predicate and an output function; or it is labeled by an output string. This
-- models a set of transitions between the two states where the output label may
-- depend on the symbol being read; in the case of epsilon-transitions, a plain
-- output string is provided.
type Edge q pred func = (q, Either (pred, func) (Rng func), q)

-- | An ordered edge set with fast reverse lookup.
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

-- | Construct an edge set from a list of edges
edgesFromList :: (Ord q) => [Edge q pred func] -> OrderedEdgeSet q pred func
edgesFromList es =
  OrderedEdgeSet
  { eForward = M.fromListWith (flip (++)) [ (q, [(a, b, q')]) | (q, Left (a,b), q') <- es ]
  , eBackward = M.fromListWith (flip (++)) [ (q', [(a, b, q)]) | (q, Left (a,b), q') <- es ]
  , eForwardEpsilon = M.fromListWith (flip (++)) [(q, [(y, q')]) | (q, Right y, q') <- es ]
  , eBackwardEpsilon = M.fromListWith (flip (++)) [(q', [(y,q)]) | (q, Right y, q') <- es ]
  }

-- | Get the number of states in the FST
fstStateSize :: FST q pred func -> Int
fstStateSize = S.size . fstS

-- | Get the number of transitions in the FST
fstTransSize :: FST q pred func -> Int
fstTransSize fst' = M.size (eForward (fstE fst')) + M.size (eForwardEpsilon (fstE fst'))

{-
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
-}

{-
flipEdges :: OrderedEdgeSet q pred func -> OrderedEdgeSet q pred func
flipEdges es = OrderedEdgeSet {eForward = eBackward es
                              ,eBackward = eForward es
                              ,eForwardEpsilon = eBackwardEpsilon es
                              ,eBackwardEpsilon = eForwardEpsilon es}
-}

-- | Get the underlying list representation of an ordered edge set
edgesToList :: OrderedEdgeSet q pred func -> [(q, Either (pred, func) (Rng func), q)]
edgesToList es = symEdgesToList (eForward es) ++ epsEdgesToList (eForwardEpsilon es)

-- | Get the list representation of only the symbol edges in an ordered edge set
symEdgesToList :: M.Map q [(pred, func, q)] -> [(q, Either (pred, func) (Rng func), q)]
symEdgesToList es = [ (q, Left (a,b), q') | (q, xs) <- M.toList es, (a,b,q') <- xs ]

-- | Get the list representation of only the epsilon edges in an ordered edge set
epsEdgesToList :: M.Map q [(Rng func, q)] -> [(q, Either (pred, func) (Rng func), q)]
epsEdgesToList es = [ (q, Right y, q') | (q, xs) <- M.toList es, (y, q') <- xs ]

-- | Map four functions over the edges in an FST. Symbol edges may be turned
-- into epsilon edges and vice versa.
mapEdges :: (Ord q)
         => (q -> [(pred1,func1,q)] -> [(pred2,func2,q)])
         -> (q -> [(pred1,func1,q)] -> [(Rng func2, q)])
         -> (q -> [(Rng func1, q)] -> [(pred2,func2,q)])
         -> (q -> [(Rng func1, q)] -> [(Rng func2, q)])
         -> FST q pred1 func1 -> FST q pred2 func2
mapEdges symsym symeps epssym epseps fst' =
  fst' { fstE = edgesFromList (symEdges1 ++ symEdges2 ++ epsEdges1 ++ epsEdges2) }
  where
    esym = eForward $ fstE fst'
    eeps = eForwardEpsilon $ fstE fst'
    symEdges1 = symEdgesToList $ M.mapWithKey symsym $ esym
    symEdges2 = symEdgesToList $ M.mapWithKey epssym $ eeps
    epsEdges1 = epsEdgesToList $ M.mapWithKey symeps $ esym
    epsEdges2 = epsEdgesToList $ M.mapWithKey epseps $ eeps

-- | Given an input symbol and a starting state, compute the concrete set of
-- transitions from that state.
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

-- | Given a state, compute the concrete set of epsilon-transitions from that
-- state.
evalEpsilonEdges :: (Ord st) => OrderedEdgeSet st pred func
                 -> st -> [(Rng func, st)]
evalEpsilonEdges (OrderedEdgeSet { eForwardEpsilon = meps }) q =
  case M.lookup q meps of
    Nothing -> []
    Just es -> es

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

-- | Like evalEpsilonEdges, but given an FST.
fstEvalEpsilonEdges :: (Ord st) => FST st pred func -> st -> [(Rng func, st)]
fstEvalEpsilonEdges aut = evalEpsilonEdges (fstE aut)

-- | Like fstEvalEdges, but given an FST.
fstEvalEdges :: (Ord st
                ,Function func
                ,SetLike pred (Dom func))
                => FST st pred func -> st -> Dom func -> [(Rng func, st)]
fstEvalEdges fst' q a = evalEdges (fstE fst') q a

-- | Like abstractEvalEdgesAll, but given an FST.
fstAbstractEvalEdgesAll :: (Ord st
                           ,PartialOrder pred)
                          => FST st pred func -> st -> pred -> [(func, st)]
fstAbstractEvalEdgesAll aut = abstractEvalEdgesAll (fstE aut)

-- | Is the given state a non-deterministic choice state, or an input action
-- state?
isChoiceState :: (Ord st) => FST st pred func -> st -> Bool
isChoiceState fst' q =
  case (M.member q (eForward . fstE $ fst'), M.member q (eForwardEpsilon . fstE $ fst')) of
    (True, True) -> error "Inconsistent FST - a state is both a choice and symbol state"
    (_, b)       -> b

-- | Given a state set A (represented as a list), compute the coarsest predicate
-- set obtained from the set
-- { p | p is a predicate on a transition starting in q, q in A }
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

-- | Compute an ordered right closure with output
rightClosure :: (Ord st, Monoid (Rng func)) => FST st pred func -> st -> [(Rng func, st)]
rightClosure fst' = snd . go S.empty mempty
    where
      go vis out q =
        case fstEvalEpsilonEdges fst' q of
          [] -> (vis, [(out, q)])
          xs -> foldl (\(vis', acc) (w, q') ->
                        if S.member q' vis' then
                            (vis', acc)
                        else
                            let (vis'', ys) = go (S.insert q' vis') (mappend out w) q'
                            in (vis'', acc ++ ys))
                      (vis, [])
                      xs

---------------
-- LCP analysis
---------------

-- | Compute the longest deterministic prefix for a pointed state set
-- (A, q).
ldp :: (Ord st, Ord pred, PartialOrder pred, Boolean pred) =>
       FST st pred func -> S.Set st -> st -> [pred]
ldp fst' = go
    where
      go ctx q =
          case M.lookup q (eForward $ fstE fst') of
            Just [(p,_,q')] | p `elem` (coarsestPredicateSet fst' $ S.toList ctx) ->
                                p:go (stepAll fst' p ctx) q'
            _ -> []

-- | Given a set A and a predicate p, compute the set A' of states that can be reached via a
-- transition from A labeled by a predicate containing p.
stepAll :: (Ord st, PartialOrder pred) => FST st pred func -> pred -> S.Set st -> S.Set st
stepAll fst' p = S.unions . map aux . S.toList
    where
      aux q = S.unions $ map (rightInputClosure fst' . snd)
                       $ fstAbstractEvalEdgesAll fst' q p

-- | Compute an unordered right closure without output on the input automaton of an FST
rightInputClosure :: (Ord st) => FST st pred func -> st -> S.Set st
rightInputClosure fst' = snd . go S.empty
    where
      go vis q =
       case fstEvalEpsilonEdges fst' q of
         [] -> (vis, S.singleton q)
         xs -> foldl (\(vis', acc) (_, q') ->
                       if S.member q' vis then
                         (vis', acc)
                       else
                         let (vis'', ys) = go (S.insert q' vis') q'
                         in (vis'', S.union acc ys))
                     (vis, S.empty)
                     xs

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

-- | Substitute state type by any enumerable type.
enumerateStates :: (Ord st, Ord a, Enum a) => FST st pred func -> FST a pred func
enumerateStates fst' =
  FST { fstS = S.fromList (M.elems statesMap)
      , fstE = edgesFromList
                 [ (aux q, lbl, aux q') | (q, lbl, q') <- edgesToList (fstE fst') ]
      , fstI = aux (fstI fst')
      , fstF = S.fromList [ aux q | q <- S.toList (fstF fst') ]
      }
  where
    statesMap = M.fromList (zip (S.toList (fstS fst')) [toEnum 0..])
    aux q = statesMap M.! q

{-
accessibleStates :: (Ord q, PartialOrder pred, Boolean pred)
                 => OrderedEdgeSet q pred func -> S.Set q -> S.Set q
accessibleStates es initialWS = go initialWS S.empty
  where
    go ws acc
      | S.null ws = acc
      | (q, ws') <- S.deleteFindMin ws =
          let succs = S.fromList $ map snd (abstractEvalEdgesAll es q bot)
                                   ++ map snd (evalEpsilonEdges es q)
          in go (S.union ws' (S.difference succs acc)) (S.insert q acc)

coaccessibleStates :: (Ord q, PartialOrder pred, Boolean pred)
                   => OrderedEdgeSet q pred func -> S.Set q -> S.Set q
coaccessibleStates es = accessibleStates (flipEdges es)

trim :: (Ord q, PartialOrder pred, Boolean pred)
     => FST q pred func -> FST q pred func
trim fst' =
  FST { fstI = fstI fst'
      , fstF = S.intersection (fstF fst') useful
      , fstS = useful
      , fstE = edgesFromList [ (q, lbl, q') | (q, lbl, q') <- edgesToList (fstE fst')
                                            , S.member q useful
                                            , S.member q' useful ]
      }
  where
    useful = S.intersection (accessibleStates (fstE fst') (S.singleton (fstI fst')))
                            (coaccessibleStates (fstE fst') (fstF fst'))
-}

-------------
-- Simulation
-------------

-- | Simulate a non-deterministic FST with single-symbol predicates
run :: (Function func
       ,SetLike pred (Dom func)
       ,Monoid (Rng func)
       ,Ord st)
       => FST st pred func -> [Dom func] -> [Rng func]
run fst' inp = do
  (os, q') <- go [ ([o], q') | (o,q') <- rightClosure fst' (fstI fst') ] inp
  guard (S.member q' (fstF fst'))
  return (mconcat $ reverse os)
  where
    go s [] = s
    go s (a:as) = go (close . step a $ s) as

    close s = prune S.empty [ (o':os, q') | (os, q) <- s, (o', q') <- rightClosure fst' q ]

    prune _   []        = []
    prune vis ((os,q):s) | S.member q vis = prune vis s
                         | otherwise      = (os,q):prune (S.insert q vis) s

    step a s = [ (o':os, q') | (os, q) <- s, (o',q') <- fstEvalEdges fst' q a ]

-- | Simulate a sequential machine with multi-symbol predicates
runSequential :: (Function func
                 ,Dom func ~ [a]
                 ,UniformListSet pred a
                 ,Monoid (Rng func)
                 ,Ord st)
              => FST st pred func -> [a] -> Maybe (Rng func)
runSequential fst' = go (fstI fst')
  where
    go q [] = if S.member q (fstF fst') then Just mempty else Nothing
    go q w | isChoiceState fst' q = do
               [(o, q')] <- pure (fstEvalEpsilonEdges fst' q)
               o' <- go q' w
               return $ mappend o o'
    go q w = do
      ts <- M.lookup q (eForward $ fstE fst')
      [(v, o, q')] <- pure [ (v, eval f u, q')
                           | (p, f, q') <- ts
                           , let n = listLength p
                           , let (u,v) = splitAt n w
                           , member u p ]
      o' <- go q' v
      return $ mappend o o'
