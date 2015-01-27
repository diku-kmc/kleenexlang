{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Theories where

import           RangeSet (RangeSet)
import qualified RangeSet as RS

import           Coding
import           OutputTerm

{----------------------------------------------------------------------}
{- Boolean algebras                                                   -}
{----------------------------------------------------------------------}

class Boolean b where
  top :: b
  bot :: b
  neg :: b -> b
  conj :: b -> b -> b
  disj :: b -> b -> b

  minus :: b -> b -> b
  minus b1 b2 = b1 `conj` neg b2

-- | An effective Boolean algebra is a Boolean algebra whose elements can be
-- interpreted as predicates on some domain.
class Boolean b => EffBoolean b dom | b -> dom where
  evalBoolean :: b -> (dom -> Bool)

-- | An Enumerable is a set-like boolean algebra for which we can enumerate all
-- members by consecutive none-negative integers.
class EffBoolean b dom => Enumerable b dom | b -> dom where
  indexOf :: dom -> b -> Int
  lookupIndex :: Int -> b -> dom
  size :: b -> Int

-- | A decidable Boolean algebra is a Boolean algebra for which we can decide
-- equivalence.
class Boolean b => DecBoolean b where
  (.==.) :: b -> b -> Bool
  b1 .==. b2 = not (b1 ./=. b2)

  (./=.) :: b -> b -> Bool
  b1 ./=. b2 = not (b1 .==. b2)

  (.<=.) :: b -> b -> Bool
  b1 .<=. b2 = (b1 `disj` b2) .==. b2

  (.<>.) :: b -> b -> Bool
  b1 .<>. b2 = (b1 `conj` b2) .==. bot


-- | Compute the coarsest partitioning of a list of elements of a decidable boolean algebra.
--   For a subset S of a boolean algebra, another subset P is a partition of S iff
--   (i) for all a, b in P, if a /= b, then a /\ b = 0
--   (ii) For all b in S, there are p_1, ..., p_m in P such that b = p_1 \/ ... \/ p_m
--
-- If P,Q are partitions of a subset S, then Q is coarser than P if P is a partition of Q.
-- TODO: Prove that the greedy approach employed in the algorithm below is actually correct.
coarsestPartition :: DecBoolean b => [b] -> [b]
coarsestPartition elems =
  case findOverlap elems of
    Just (x, y, elems') ->
      let xy' = x `minus` y
          yx' = y `minus` x
          i   = x `conj` y
          f p = if p .==. bot then [] else [p]
       in
      coarsestPartition (f xy' ++ f yx' ++ f i ++ elems')
    Nothing -> elems
  where
    findOverlapWith _ [] = Nothing
    findOverlapWith b (x:xs)
      | not (b .<>. x) = Just (x, xs)
      | Just (y, xs') <- findOverlapWith b xs = Just (y, x:xs')
      | otherwise = Nothing

    findOverlap [] = Nothing
    findOverlap (x:xs)
      | Just (y, xs') <- findOverlapWith x xs = Just (x, y, xs')
      | Just (x', y, xs') <- findOverlap xs = Just (x', y, x:xs')
      | otherwise = Nothing

{----------------------------------------------------------------------}
{- Functions                                                          -}
{----------------------------------------------------------------------}

class Function t dom rng where
  evalFunction :: t -> dom -> rng

{----------------------------------------------------------------------}
{- Instances                                                          -}
{----------------------------------------------------------------------}

instance (Enumerable ba dom, Enum rng, Bounded rng) => Function (OutputTerm ba rng) dom [rng] where
  evalFunction (OutputTerm ts) x = concatMap evalTerm ts
    where
      evalTerm (Const y) = [y]
      evalTerm (Code b) = codeFixedWidthEnumSized (size b) (indexOf x b)

-- | Constant function ignores its argument and always returns the same constant.
instance Function (ConstFunction rng) dom rng where
    evalFunction (ConstFunction x) = const x

instance (Ord a, Enum a, Bounded a) => Boolean (RangeSet a) where
  top  = RS.universe
  bot  = RS.empty
  neg  = RS.complement
  conj = RS.intersection
  disj = RS.union

instance (Ord a, Enum a, Bounded a) => EffBoolean (RangeSet a) a where
  evalBoolean = flip RS.member

instance (Ord a, Enum a, Bounded a) => Enumerable (RangeSet a) a where
  indexOf = RS.indexOf
  lookupIndex = RS.lookupIndex
  size = RS.size

instance (Ord a, Enum a, Bounded a) => DecBoolean (RangeSet a) where
  (.==.) = (==)
  (.<=.) = RS.isSubsetOf
