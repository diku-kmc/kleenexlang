module Tests(tests) where

import           Test.QuickCheck
import qualified Distribution.TestSuite as TS

import KMC.Coding

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
tests = return [simpleGroup True "Coding" codingTests]

prop_coding_bijective :: Property
prop_coding_bijective = forAll (elements [2..32]) $ \base x ->
                            decode base (codeFixedWidth base 64 x) == x

codingTests :: [TS.Test]
codingTests =
  [simpleTest "prop_coding_bijective" (quickTest prop_coding_bijective)]
