{-# LANGUAGE FlexibleInstances #-}

module KMC.RangeSet
(RangeSet
,rangeSet
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
,ranges
,isSubsetOf
,member
) where

import           Data.List (sort)

-- invariant: The list is always sorted.
data RangeSet a = RangeSet { ranges :: [(a, a)] }
  deriving (Eq, Ord, Show)

normalize :: (Enum a, Ord a) => RangeSet a -> RangeSet a
normalize (RangeSet rs) = RangeSet $ go $ filter validRange rs
  where
    validRange (l, h) = l <= h

    go [] = []
    go [r] = [r]
    go ((l1, h1):(l2, h2):rs')
       | l2 <= h1 || succ h1 == l2 = go ((l1, max h1 h2) : rs')
       | otherwise = (l1, h1):go ((l2, h2):rs')

rangeSet :: (Enum a, Ord a) => [(a, a)] -> RangeSet a
rangeSet rs = normalize $ RangeSet $ sort rs

singleton :: a -> RangeSet a
singleton a = RangeSet [(a, a)]

findMin :: RangeSet a -> a
findMin (RangeSet []) = error "empty set has no minimal element"
findMin (RangeSet ((a,_):_)) = a

findMax :: RangeSet a -> a
findMax (RangeSet []) = error "empty set has no maximal element"
findMax (RangeSet [(_,b)]) = b
findMax (RangeSet (_:rs)) = findMax (RangeSet rs)

isSubsetOf :: (Ord a) => RangeSet a -> RangeSet a -> Bool
isSubsetOf (RangeSet rs1) (RangeSet rs2) = go rs1 rs2
  where
    go [] _ = True
    go _ [] = False
    go xs@((l,h):xs') ys@((l',h'):ys')
        | l < l' = False
        | h' < l = go xs ys'
        | l >= l' && h' >= l = h <= h' && go xs' ys
        | otherwise = error "cannot happen"

empty :: RangeSet a
empty = RangeSet []

universe :: (Ord a, Enum a, Bounded a) => RangeSet a
universe = complement empty

isEmpty :: RangeSet a -> Bool
isEmpty (RangeSet []) = True
isEmpty _ = False

complement :: (Enum a, Bounded a, Ord a) => RangeSet a -> RangeSet a
complement (RangeSet theranges) = RangeSet $ go (Just minBound) theranges
  where
    go Nothing []  = []
    go Nothing _   = error "Inconsistent range set"
    go (Just lb) [] = [(lb, maxBound)]
    go (Just lb) ((l,h):rs) =
      let lb' = if h < maxBound then Just (succ h) else Nothing
      in if lb < l then
             (lb, pred l):go lb' rs
         else
             go lb' rs

isUniverse :: (Enum a, Bounded a, Ord a) => RangeSet a -> Bool
isUniverse bc = isEmpty (complement bc)

union :: (Enum a, Ord a) => RangeSet a -> RangeSet a -> RangeSet a
union (RangeSet rs) (RangeSet rs') = normalize $ RangeSet $ go rs rs'
    where go [] ys = ys
          go xs [] = xs
          go xs@((l,h):xs') ys@((l',h'):ys')
            | h  <  l'  = (l, h)   : go xs' ys
            | h' <  l   = (l', h') : go xs  ys'
            | h  <= h'  = go xs' ((min l l', h'):ys')
            | h' <  h   = go ((min l l', h):xs') ys'
            | otherwise = error "cannot happen"

size :: (Enum a) => RangeSet a -> Int
size (RangeSet []) = 0
size (RangeSet ((l, h):rs)) = 1 + (fromEnum h - fromEnum l) + size (RangeSet rs)

intersection :: (Enum a, Bounded a, Ord a) => RangeSet a -> RangeSet a -> RangeSet a
intersection (RangeSet bc) (RangeSet bc') = RangeSet $ go bc bc'
    where
        go [] _ = []
        go _ [] = []
        go xs@((l,h):xs') ys@((l',h'):ys')
            | l' > h = go xs' ys 
            | l > h' = go xs ys'
            | h > h' = (max l l', h') : go xs ys'
            | otherwise = (max l l', h) : go xs' ys

member :: (Ord a) => a -> RangeSet a -> Bool
member w (RangeSet rs) = any (\(l, h) -> l <= w && w <= h) rs

toList :: (Enum a, Bounded a, Ord a) => RangeSet a -> [a]
toList bc = filter (flip member bc) [minBound .. maxBound]

indexOf :: (Enum a, Ord a) => a -> RangeSet a -> Int
indexOf w (RangeSet ((l, h):rs))
    | l <= w = if w <= h then
                 fromEnum w - fromEnum l
               else
                 (fromEnum h - fromEnum l + 1) + indexOf w (RangeSet rs)
indexOf _ _ = error "Element not in range set"

lookupIndex :: (Enum a) => Int -> RangeSet a -> a
lookupIndex i (RangeSet ((l, h):rs)) =
  if fromEnum l + i <= fromEnum h then
    toEnum (fromEnum l + i)
  else
    lookupIndex (i - (fromEnum h - fromEnum l + 1)) (RangeSet rs)
lookupIndex _ _ = error "index out of bounds"
