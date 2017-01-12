module Tests ( tests ) where

import qualified Distribution.TestSuite as TS

import           Tests.TestUtils
import           Tests.KleenexParser
import           Tests.Coding
import           Tests.Regression
import           Tests.KFoldApproximation
import           Tests.OneApproximation


tests :: IO [TS.Test]
tests = return [ simpleGroup True "Coding" codingTests
               , simpleGroup True "Regression" regressionTests
               , simpleGroup True "Kleenex parsing" kleenexParserTests
               , simpleGroup True "Iterative Approximations test" oneApproximationTests
               , simpleGroup True "K-fold Approximations test" kApproximationTests
               ]
