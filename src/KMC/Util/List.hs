module KMC.Util.List where

lcp :: (Eq a) => Maybe [a] -> Maybe [a] -> Maybe [a]
lcp Nothing y = y
lcp x Nothing = x
lcp (Just u) (Just v) = case (u, v) of
                         ([], _) -> Just []
                         (_, []) -> Just []
                         (a:u', b:v') | a == b -> (a:) <$> lcp (Just u') (Just  v')
                                      | otherwise -> Just []

lcpMany :: Eq a => [[a]] -> Maybe [a]
lcpMany xs = foldr lcp Nothing (map Just xs)

lcpMany1 :: Eq a => [[a]] -> [a]
lcpMany1 xs = maybe (error "empty list") id (lcpMany xs)

linv :: Eq a => [a] -> [a] -> Maybe [a]
linv [] v = Just v
linv (a:u') v = case v of
                  (b:v') | a == b -> linv u' v'
                  _ -> Nothing

linv' :: Eq a => [a] -> [a] -> [a]
linv' u v = maybe (error "not a prefix") id (linv u v)

-- | Behave like foldr1 if the list is non-empty and emit the second argument otherwise.
foldr1ifEmpty :: (a -> a -> a) -> a -> [a] -> a
foldr1ifEmpty _ e [] = e
foldr1ifEmpty f _ l  = foldr1 f l
