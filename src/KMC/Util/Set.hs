module KMC.Util.Set where

import qualified Data.Set as S

-- | The monad `join` operation for sets.  Data.Set is not a monad
-- because of the (Ord a) constraint!
joinSets :: (Ord a) => S.Set (S.Set a) -> S.Set a
joinSets = S.foldl S.union S.empty


-- | Given a set S of `a` and a set of sets of `a`, return those sets that
-- contain an element from S.
occurs :: (Ord a) => S.Set a -> S.Set (S.Set a) -> S.Set (S.Set a)
occurs needle sets = S.filter (not . S.null . S.intersection needle) sets

