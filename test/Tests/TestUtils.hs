{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Tests.TestUtils where

-- Various useful utility functions for the test suite.

import           Control.Exception (catch, SomeException, Exception)
import           Control.Exception.Base (evaluate)
import           Control.DeepSeq (NFData(..))
import           Control.Seq (withStrategy, rdeepseq)
import qualified Distribution.TestSuite as TS
import qualified Test.QuickCheck as QC

type TestName = String

-- | We need a strict `catch` function to capture any calls to `error` in the
-- tested code so it can be handled gracefully in the testing framework.
strictCatch :: (NFData a, Exception e) => IO a -> (e -> IO a) -> IO a
strictCatch = catch . (toNF =<<)
    where
      toNF :: (NFData a) => a -> IO a
      toNF = evaluate . withStrategy rdeepseq

catchAllExceptions :: IO TS.Result -> IO TS.Result
catchAllExceptions test = test `strictCatch`
                          \(e :: SomeException) -> return $ TS.Error $ show e
             
-- | A TS.Result can be forced to normal form.
instance NFData TS.Result where
    rnf TS.Pass      = ()
    rnf (TS.Fail _)  = ()
    rnf (TS.Error _) = ()

-- | Convenient way of putting the names in the test.
(<@>) :: TestName -> IO TS.Result -> (String, IO TS.Result)
(<@>) n t = (n, t)
infix 0 <@>

simpleTest :: String -> IO TS.Result -> TS.Test
simpleTest name' action = TS.Test inst
    where
      inst = TS.TestInstance
             { TS.name = name'
             , TS.run = (catchAllExceptions action) >>= return . TS.Finished
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

-- | Convert a QuickCheck test to a test case.
quickTest :: QC.Testable prop => prop -> IO TS.Result
quickTest prop = do
  res <- QC.quickCheckResult prop
  case res of
    QC.Success _ _ _ -> return TS.Pass
    _ -> return $ TS.Error "QuickCheck failed"

-- | For debugging purposes.  Manually run a test case.
runTest :: TS.Test -> IO TS.Result
runTest (TS.Test t) = runTestInstance t

runTestInstance :: TS.TestInstance -> IO TS.Result
runTestInstance test = TS.run test >>= go
    where
      go :: TS.Progress -> IO TS.Result
      go (TS.Finished r) = return r
      go (TS.Progress _ p) = p >>= go
