module Tests.Coding ( codingTests ) where

import qualified Distribution.TestSuite as TS
import           Test.QuickCheck

import           KMC.Util.Coding

import           Tests.TestUtils

codingTests :: [TS.Test]
codingTests =
    [ simpleTest "prop_coding_bijective" (quickTest prop_coding_bijective) ]

prop_coding_bijective :: Property
prop_coding_bijective = forAll (elements [2..32]) $ \base x ->
                            decode base (codeFixedWidth base 64 x) == x
