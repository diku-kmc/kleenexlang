module KMC.Util.List where

-- | Behave like foldr1 if the list is non-empty and emit the second argument otherwise.
foldr1ifEmpty :: (a -> a -> a) -> a -> [a] -> a
foldr1ifEmpty _ e [] = e
foldr1ifEmpty f _ l  = foldr1 f l

-- | Mix two lists together by alternating between elements of the first
-- and the second.  The resulting list will be twice the length of the
-- shortest of the argument lists.
alternate :: [a] -> [a] -> [a]
alternate [] _      = []
alternate (x:xs) ys = x : alternate ys xs

