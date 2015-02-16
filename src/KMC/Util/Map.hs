module KMC.Util.Map where

import qualified Data.Map as M
import           Data.Tuple (swap)

-- | Swap the keys and values in a map.  If the map is not bijective the result
-- will probably be unusable.
swapMap :: (Ord v) => M.Map k v -> M.Map v k
swapMap = M.fromList . map swap . M.toList

