{-# LANGUAGE QuasiQuotes #-}
module Tests(tests) where

import           Test.QuickCheck
import qualified Distribution.TestSuite as TS
import           Data.Word
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)

import           KMC.Coding
import           KMC.Expression
import           KMC.FSTConstruction
import qualified KMC.Hased.Lang as H
import           KMC.OutputTerm
import           KMC.RangeSet
import           KMC.SSTConstruction
import           KMC.SymbolicFST as FST
import           KMC.SymbolicSST as SST
import           KMC.Util.Heredoc

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
tests = return [ simpleGroup True "Coding" codingTests
               , simpleGroup True "Regression" regressionTests
               ]

prop_coding_bijective :: Property
prop_coding_bijective = forAll (elements [2..32]) $ \base x ->
                            decode base (codeFixedWidth base 64 x) == x

codingTests :: [TS.Test]
codingTests =
    [ simpleTest "prop_coding_bijective" (quickTest prop_coding_bijective) ]

regressionTests :: [TS.Test]
regressionTests =
    [ simpleTest "unsound_lookahead" unsound_lookahead
    , simpleTest "character class accepts dash" charclass_accept_dash
    ]

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

encodeChar :: Char -> [Word8]
encodeChar = BS.unpack . encodeUtf8 . T.singleton

charclass_accept_dash :: IO TS.Result
charclass_accept_dash = 
    let prog = [strQ|test := <[a-z-]*>|]
    in hasedIdTest prog "a-b-c-d---d-f-eerasdfs-"

-- This was a bug in the C generation.  Works in the SST world it seems.
newline_bug :: IO TS.Result
newline_bug =
    let prog = [strQ|
test := ( keep "\n" | ~drop ) ~<\n> test
      | ( keep "\n" | ~drop ) ~<\n>
keep := <(a|b)+(a|b)(a|b)+>
drop := <[^\n]*>
|]
    in hasedIdTest prog $ unlines ["aaaba","aabbaa"]


hasedIdTest :: String -> String -> IO TS.Result
hasedIdTest prog str =
    let inp = concatMap encodeChar str in
    case H.testHased prog of
      Left err -> return $ TS.Fail err
      Right m  ->
          let sst = sstFromFST (fromMu m :: FST Int (RangeSet Word8) H.HasedOutTerm) True
              out = SST.flattenStream $ SST.run sst inp
          in if inp == out
             then return TS.Pass
             else return $ TS.Fail "Identity failed"
