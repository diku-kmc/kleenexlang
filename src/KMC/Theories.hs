{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module KMC.Theories where

import           Control.Monad (liftM2)
    
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
-- members by consecutive non-negative integers.
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
  domain  :: t -> [Dom t]

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
{- Various nice shorthands.                                           -}
{----------------------------------------------------------------------}

-- | Convenience class: a predicate must always be an instance of `Ord`,
-- `Boolean`, and `PartialOrder`.
class (Ord pred, Boolean pred, PartialOrder pred) => Predicate pred where

class (Ord var, Show var, Eq var) => Variable var where

-- Finite alphabet
class (Ord delta, Show delta, Eq delta, Enum delta, Bounded delta) => Alphabet delta where

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

instance (Bounded a) => Bounded (RangeSet a) where
   maxBound = RS.singleton maxBound
   minBound = RS.singleton minBound

instance (Boolean a, Boolean b) => Boolean (a, b) where
    top = (top, top)
    bot = (bot, bot)
    neg (x, y) = (neg x, neg y)
    conj (x, y) (x', y') = (x `conj` x', y `conj` y')
    disj (x, y) (x', y') = (x `disj` x', y `disj` y')

instance (SetLike a doma, SetLike b domb) => SetLike (a, b) (doma, domb) where
    member (x, y) (l, r) = (x `member` l) && (y `member` r)

instance (PartialOrder a, PartialOrder b) => PartialOrder (a, b) where
    lte (x, y) (x', y') = (x `lte` x') && (y `lte` y')

-- The definition is general for (Monad m), but we get an overlapping instance:
--      instance (Ord a, Enum a, Bounded a) => Boolean (RangeSet a)
--      instance (Monad m, Boolean p) => Boolean (m p)
instance (Boolean b) => Boolean (Maybe b) where
    top      = return top
    bot      = return bot
    neg x    = x >>= return . neg
    conj x y = liftM2 conj x y
    disj x y = liftM2 disj x y

instance (SetLike a doma) => SetLike (Maybe a) (Maybe doma) where
    member x s = maybe False id $ liftM2 member x s

instance (PartialOrder p) => PartialOrder (Maybe p) where
    lte x y = maybe False id $ liftM2 lte x y

instance (Ord a, Enum a, Bounded a) => Predicate (RangeSet a) where
instance (Predicate p1, Predicate p2) => Predicate (p1, p2) where
instance (Predicate p) => Predicate (Maybe p) where

data Prefix a = Prefix [a] deriving (Eq, Ord, Show)

instance (Eq a) => PartialOrder (Prefix a) where
    eq = (==)
    lte (Prefix xs) (Prefix ys) = prefixOf xs ys
        where
          prefixOf [] _ = True
          prefixOf (x:l) (y:r) = (x == y) && prefixOf l r
          prefixOf _ _ = False
