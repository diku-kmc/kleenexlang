module KMC.Util.List where

-- | Replace all sublists of a given form with another list.
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace a b
  | null a = id
  | otherwise = go a []
    where
      go _ acc [] = reverse acc
      go [] _ zs = b ++ go a [] zs
      go (x:xs) acc (z:zs) | x == z = go xs (x:acc) zs
                           | otherwise = (reverse acc) ++ (z:go a [] zs)
