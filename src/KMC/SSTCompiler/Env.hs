module KMC.SSTCompiler.Env(Env(..), EnvReader) where

import           Control.Monad.Reader
import qualified Data.Map as M

import           KMC.Program.IL

-----------------------
-- Compiler environment
-----------------------
data Env st var func = Env
    { bmap   :: M.Map var BufferId      -- ^ Variable to buffer map
    , tmap   :: M.Map func TableId      -- ^ Function to table map
    , cmap   :: M.Map [Int] ConstId     -- ^ Constant to const id map
    , smap   :: M.Map st BlockId        -- ^ State to block id map
    , outvar :: var                     -- ^ Designated output variable
    }
type EnvReader st var func = Reader (Env st var func)
