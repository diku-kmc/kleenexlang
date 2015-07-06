{-# LANGUAGE FlexibleInstances #-}

module KMC.RangeSet
(RangeSet(..)
,rangeSet
,boundedRangeSet
,singleton
,findMin
,findMax
,toList
,union
,intersection
,complement
,empty
,universe
,isEmpty
,isUniverse
,size
,indexOf
,lookupIndex
,isSubsetOf
,member
) where

import           Data.List (sort)

import Debug.Trace

-- invariant: The list is always sorted.
data RangeSet a = RangeSet { minB :: a, maxB :: a, numSymbols :: Int, ranges :: [(a, a)] }
  deriving (Eq, Ord, Show) 

normalize :: (Enum a, Ord a) => RangeSet a -> RangeSet a
normalize (RangeSet minB maxB s rs) = RangeSet minB maxB s $ go $ filter validRange rs
  where
    validRange (l, h) = l <= h

    go [] = []
    go [r] = [r]
    go ((l1, h1):(l2, h2):rs')
       | l2 <= h1 || succ h1 == l2 = go ((l1, max h1 h2) : rs')
       | otherwise = (l1, h1):go ((l2, h2):rs')

-- Smart constructors that automatically infers min/max bound
rangeSet :: (Enum a, Ord a, Bounded a) => [(a, a)] -> RangeSet a
rangeSet rs = normalize $ RangeSet minBound maxBound 1 $ sort rs

empty :: (Bounded a) => RangeSet a
empty = RangeSet minBound maxBound 1 []

singleton :: (Bounded a) => a -> RangeSet a
singleton a = RangeSet minBound maxBound 1 [(a, a)]

-- Not-so-smart constructor
boundedRangeSet min max symbSize rs = normalize $ RangeSet min max symbSize $ sort rs 

findMin :: RangeSet a -> a
findMin (RangeSet _ _ _ []) = error "empty set has no minimal element"
findMin (RangeSet _ _ _ ((a,_):_)) = a

findMax :: RangeSet a -> a
findMax (RangeSet _ _ _ []) = error "empty set has no maximal element"
findMax (RangeSet _ _ _ [(_,b)]) = b
findMax (RangeSet min max s (_:rs)) = findMax (RangeSet min max s rs)

isSubsetOf :: (Ord a) => RangeSet a -> RangeSet a -> Bool
isSubsetOf (RangeSet _ _ _ rs) (RangeSet _ _ _ rs') = go rs rs'
  where
    go [] _ = True
    go _ [] = False
    go xs@((l,h):xs') ys@((l',h'):ys')
        | l < l' = False
        | h' < l = go xs ys'
        | l >= l' && h' >= l = h <= h' && go xs' ys
        | otherwise = error "RangeSet.isSubsetOf: cannot happen"

universe :: (Ord a, Bounded a, Enum a) => RangeSet a
universe = complement empty

isEmpty :: RangeSet a -> Bool
isEmpty (RangeSet _ _ _ []) = True
isEmpty _ = False

complement :: (Enum a, Ord a) => RangeSet a -> RangeSet a
complement (RangeSet minB maxB s rs) = RangeSet minB maxB s $ go (Just minB) rs
  where
    go Nothing []  = []
    go Nothing _   = error "Inconsistent range set"
    go (Just lb) [] = [(lb, maxB)]
    go (Just lb) ((l,h):rs) =
      let lb' = if h < maxB then Just (succ h) else Nothing
      in if lb < l then
             (lb, pred l):go lb' rs
         else
             go lb' rs

isUniverse :: (Enum a, Bounded a, Ord a) => RangeSet a -> Bool
isUniverse bc = isEmpty (complement bc)

union :: (Enum a, Ord a) => RangeSet a -> RangeSet a -> RangeSet a
union (RangeSet minB maxB s rs) (RangeSet minB' maxB' s' rs') = 
        if minB == minB' && maxB == maxB' && s == s'
        then normalize $ RangeSet minB maxB s $ go rs rs'
        else error "Tried to union rangesets with different bounds or symbol size" 
    where go [] ys = ys
          go xs [] = xs
          go xs@((l,h):xs') ys@((l',h'):ys')
            | h  <  l'  = (l, h)   : go xs' ys
            | h' <  l   = (l', h') : go xs  ys'
            | h  <= h'  = go xs' ((min l l', h'):ys')
            | h' <  h   = go ((min l l', h):xs') ys'
            | otherwise = error "RangeSet.union: cannot happen"

size :: (Enum a) => RangeSet a -> Int
size (RangeSet _ _ _ []) = 0
size (RangeSet minB maxB s ((l, h):rs)) = 1 + (fromEnum h - fromEnum l) + size (RangeSet minB maxB s rs)

intersection :: (Enum a, Bounded a, Ord a) => RangeSet a -> RangeSet a -> RangeSet a
intersection (RangeSet minB maxB s rs) (RangeSet minB' maxB' s' rs') =
        if minB == minB' && maxB == maxB' && s == s'
        then normalize $ RangeSet minB maxB s $ go rs rs'
        else error "Tried to intersect rangesets with different bounds" 
    where
        go [] _ = []
        go _ [] = []
        go xs@((l,h):xs') ys@((l',h'):ys')
            | l' > h = go xs' ys 
            | l > h' = go xs ys'
            | h > h' = (max l l', h') : go xs ys'
            | otherwise = (max l l', h) : go xs' ys

member :: (Ord a) => a -> RangeSet a -> Bool
member w (RangeSet _ _ _ rs) = any (\(l, h) -> l <= w && w <= h) rs

toList :: (Enum a, Ord a) => RangeSet a -> [a]
toList bc@(RangeSet minB maxB _ _)  = filter (flip member bc) [minB .. maxB]

indexOf :: (Enum a, Ord a) => a -> RangeSet a -> Int
indexOf w (RangeSet minB maxB s ((l, h):rs))
    | l <= w = if w <= h then
                 fromEnum w - fromEnum l
               else
                 (fromEnum h - fromEnum l + 1) + indexOf w (RangeSet minB maxB s rs)
indexOf _ _ = error "RangeSet: Element not in range set"

lookupIndex :: (Enum a) => Int -> RangeSet a -> a
lookupIndex i (RangeSet minB maxB s ((l, h):rs)) =
  if fromEnum l + i <= fromEnum h then
    toEnum (fromEnum l + i)
  else
    lookupIndex (i - (fromEnum h - fromEnum l + 1)) (RangeSet minB maxB s rs)
lookupIndex _ _ = error "RangeSet: index out of bounds"
