module Tests(tests) where

import           Test.QuickCheck
import qualified Distribution.TestSuite as TS

import KMC.Expression
import KMC.RangeSet
import KMC.OutputTerm
import KMC.Coding
import KMC.SymbolicSST as SST
import KMC.SymbolicFST
import KMC.FSTConstruction
import KMC.SSTConstruction

simpleTest :: String -> IO TS.Result -> TS.Test
simpleTest name' action = TS.Test inst
    where
      inst = TS.TestInstance
             { TS.name = name'
             , TS.run = action >>= return . TS.Finished
             , TS.tags = []
             , TS.options = []
             , TS.setOption = \ _ _ -> Right inst
             }

simpleGroup :: Bool -> String -> [TS.Test] -> TS.Test
simpleGroup concurrently' groupName' groupTests' =
  TS.Group
  { TS.groupName = groupName'
  , TS.concurrently = concurrently'
  , TS.groupTests = groupTests'
  }

quickTest :: Testable prop => prop -> IO TS.Result
quickTest prop = do
  res <- quickCheckResult prop
  case res of
    Success _ _ _ -> return TS.Pass
    _ -> return $ TS.Error "QuickCheck failed"

{--------------------------------------------------------------------}

tests :: IO [TS.Test]
tests = return [simpleGroup True "Coding" codingTests
               ,simpleGroup True "Regression" regressionTests]

prop_coding_bijective :: Property
prop_coding_bijective = forAll (elements [2..32]) $ \base x ->
                            decode base (codeFixedWidth base 64 x) == x

codingTests :: [TS.Test]
codingTests =
  [simpleTest "prop_coding_bijective" (quickTest prop_coding_bijective)]

regressionTests :: [TS.Test]
regressionTests = [simpleTest "unsound_lookahead" unsound_lookahead]

unsound_lookahead :: IO TS.Result
unsound_lookahead =
  let fst' = fromMu la_mu :: FST Int (RangeSet Int) (Const Int [Int])
      sst1 = sstFromFST fst' True
      sst2 = sstFromFST fst' False
  in if flattenStream (SST.run sst1 [0,1]) == flattenStream (SST.run sst2 [0,1]) then
         return TS.Pass
     else
         return $ TS.Fail "Lookahead optimization changes output semantics of FST"

la_mu :: Mu (RangeSet Int) (Const Int [Int]) a
la_mu = Alt (RW (singleton 0) (Const [])
             $ Seq (Alt (RW (singleton 1) (Const []) Accept)
                        (RW (singleton 2) (Const []) Accept))
                   (W [0] Accept))
            (RW (singleton 0) (Const [])
            $ RW (rangeSet [(1,2)]) (Const [])
            $ W [1] Accept)
