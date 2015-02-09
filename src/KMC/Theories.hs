{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module KMC.Theories where

import           KMC.RangeSet (RangeSet)
import qualified KMC.RangeSet as RS

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
class Boolean b => SetLike b dom | b -> dom where
  member :: dom -> b -> Bool

-- | An Enumerable is a set-like boolean algebra for which we can enumerate all
-- members by consecutive none-negative integers.
class SetLike b dom => Enumerable b dom | b -> dom where
  indexOf :: dom -> b -> Int
  lookupIndex :: Int -> b -> dom
  size :: b -> Int

-- | Compute the coarsest partitioning of a list of elements of a decidable boolean algebra.
--   For a subset S of a boolean algebra, another subset P is a partition of S iff
--   (i) for all a, b in P, if a /= b, then a /\ b = 0
--   (ii) For all b in S, there are p_1, ..., p_m in P such that b = p_1 \/ ... \/ p_m
--
-- If P,Q are partitions of a subset S, then Q is coarser than P if P is a partition of Q.
-- TODO: Prove that the greedy approach employed in the algorithm below is actually correct.
coarsestPartition :: (Boolean b, PartialOrder b) => [b] -> [b]
coarsestPartition elems =
  case findOverlap elems of
    Just (x, y, elems') ->
      let xy' = x `minus` y
          yx' = y `minus` x
          i   = x `conj` y
          f p = if p `eq` bot then [] else [p]
       in
      coarsestPartition (f xy' ++ f yx' ++ f i ++ elems')
    Nothing -> elems
  where
    findOverlapWith _ [] = Nothing
    findOverlapWith b (x:xs)
      | not ((b `conj` x) `eq` bot) = Just (x, xs)
      | Just (y, xs') <- findOverlapWith b xs = Just (y, x:xs')
      | otherwise = Nothing

    findOverlap [] = Nothing
    findOverlap (x:xs)
      | Just (y, xs') <- findOverlapWith x xs = Just (x, y, xs')
      | Just (x', y, xs') <- findOverlap xs = Just (x', y, x:xs')
      | otherwise = Nothing

{----------------------------------------------------------------------}
{- Functions, terms                                                   -}
{----------------------------------------------------------------------}

class Function t where
  type Dom t :: *
  type Rng t :: *
  eval    :: t -> Dom t -> Rng t
  isConst :: t -> Maybe (Rng t)
  inDom   :: Dom t -> t -> Bool

{----------------------------------------------------------------------}
{- Partial orders                                                     -}
{----------------------------------------------------------------------}

class PartialOrder a where
  lte :: a -> a -> Bool

  comparable :: a -> a -> Bool
  comparable x y = (x `lte` y) || (y `lte` x)

  gte :: a -> a -> Bool
  gte = flip lte

  lt :: a -> a -> Bool
  lt x y = lte x y && not (eq x y)

  gt :: a -> a -> Bool
  gt = flip lt

  eq :: a -> a -> Bool
  eq x y = lte x y && lte y x


{----------------------------------------------------------------------}
{- Instances                                                          -}
{----------------------------------------------------------------------}

instance (Ord a, Enum a, Bounded a) => Boolean (RangeSet a) where
  top  = RS.universe
  bot  = RS.empty
  neg  = RS.complement
  conj = RS.intersection
  disj = RS.union

instance (Ord a, Enum a, Bounded a) => SetLike (RangeSet a) a where
  member = RS.member

instance (Ord a, Enum a, Bounded a) => Enumerable (RangeSet a) a where
  indexOf = RS.indexOf
  lookupIndex = RS.lookupIndex
  size = RS.size

instance (Ord a, Enum a, Bounded a) => PartialOrder (RangeSet a) where
    lte = RS.isSubsetOf
    eq = (==)

data Prefix a = Prefix [a] deriving (Eq, Ord, Show)

instance (Eq a) => PartialOrder (Prefix a) where
    eq = (==)
    lte (Prefix xs) (Prefix ys) = prefixOf xs ys
        where
          prefixOf [] _ = True
          prefixOf (x:l) (y:r) = (x == y) && prefixOf l r
          prefixOf _ _ = False
