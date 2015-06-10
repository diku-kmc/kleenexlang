module KMC.Util.List where

-- | Behave like foldr1 if the list is non-empty and emit the second argument otherwise.
foldr1ifEmpty :: (a -> a -> a) -> a -> [a] -> a
foldr1ifEmpty _ e [] = e
foldr1ifEmpty f _ l  = foldr1 f l
