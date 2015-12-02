module Tests ( tests ) where

import qualified Distribution.TestSuite as TS

import           Tests.TestUtils
import           Tests.KleenexParser
import           Tests.Coding
import           Tests.Regression


tests :: IO [TS.Test]
tests = return [ simpleGroup True "Coding" codingTests
               , simpleGroup True "Regression" regressionTests
               , simpleGroup True "Kleenex parsing" kleenexParserTests
               ]
