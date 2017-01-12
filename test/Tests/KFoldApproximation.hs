{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.KFoldApproximation ( kApproximationTests ) where

import qualified Data.Map as M
import           Data.Monoid
import           Data.Word
import qualified Distribution.TestSuite as TS
import           Test.QuickCheck
import           Tests.OneApproximation hiding (testCombinator, prop_approx_match)
import           Tests.TestUtils

import           KMC.Determinization
import qualified KMC.Kleenex.Approximation as A
import           KMC.Kleenex.ApproximationMetrics
import           KMC.Kleenex.Desugaring
import           KMC.Kleenex.Syntax
import           KMC.SymbolicFST.Transducer
import           KMC.SymbolicSST
import           KMC.Theories
import           KMC.RangeSet
import           KMC.Util.Heredoc

kApproximationTests :: [TS.Test]
kApproximationTests = testCombinatorFold [ ("testProg3", testProg3)]
                                         [ LCS, Hamming, Levenshtein]
                                         [ 1,2 ]

prop_approx_fold :: RProgAct -> TestSST -> TestSST -> ApproxMetric -> Int -> Property
prop_approx_fold p sst1 sst2 m k = forAll (generateString p m k) $
                 \s -> (validateMatch sst1 s) == (validateMatch sst2 s)


testCombinatorFold :: [(String, RProgAct)] -> [ApproxMetric] -> [Int] -> [TS.Test]
testCombinatorFold ps ms ks = concat $ map (\p -> concat ( map
                          (\m -> map (\k -> qtest p m k) ks) ms)) ps
  where
    qtest p m k = simpleTest (desc p m k) $ quickTest rpp $
      prop_approx_fold (snd p) (testSST (snd p) m k Matching)
                        (testSSTFold (snd p) m k Matching) m k
    desc p m k = "ApproxQC: " ++ (fst p) ++ " " ++ (show m) ++ " " ++ show k
    rpp = 20      -- ^ Number of quickcheck runs pr program

-- | Creates a SST from a RProg, an ErrorMetric and a approximation value k
testSSTFold :: RProgAct -> ApproxMetric -> Int -> ApproxMode -> TestSST
testSSTFold p m k mode =
  let aprog = A.approxProg p k 0 m mode
      ffst = constructTransducer aprog 0
  in enumerateVariables $ enumerateStates $ sstFromFST ffst True
