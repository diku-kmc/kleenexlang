{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests ( tests ) where

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
import qualified KMC.Kleenex.Lang as H
import           KMC.OutputTerm
import           KMC.RangeSet
import           KMC.SSTConstruction
import           KMC.SymbolicFST as FST
import           KMC.SymbolicSST as SST
import           KMC.Util.Heredoc

import           Tests.DFAConstruction
import           Tests.TestUtils
import           Tests.KleenexParser

{--------------------------------------------------------------------}


tests :: IO [TS.Test]
tests = return [ simpleGroup True "Coding" codingTests
               , simpleGroup True "Regression" regressionTests
               , simpleGroup True "Kleenex parsing" kleenexParserTests
               , simpleGroup True "DFA optimization" dfaOptimizationTests
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
    , simpleTest "newline bug" newline_bug
    , simpleTest "pipeline" pipeline -- WTF?!
    ]

unsound_lookahead :: IO TS.Result
unsound_lookahead =
  let fst' = fromMu la_mu :: FST Int (RangeSet Int) (Const Int [Int] :+: (NullFun Int [Int]))
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
    let prog = [strQ|test
test := /[a-z-]*/|]
    in kleenexIdTest prog "a-b-c-d---d-f-eerasdfs-"

-- This was a bug in the C generation.  Works in the SST world it seems.
newline_bug :: IO TS.Result
newline_bug =
    let prog = [strQ|test
test := ( keep "\n" | ~drop ) ~/\n/ test
      | ( keep "\n" | ~drop ) ~/\n/
keep := /(a|b)+(a|b)(a|b)+/
drop := /[^\n]*/
|]
    in kleenexIdTest prog $ unlines ["aaaba","aabbaa"]

-- Test that pipelining works
pipeline :: IO TS.Result
pipeline =
    let prog = [strQ|p >> a >> b
p := ~/abc/ "a"
a := /./ "b"
b := /ab/ "c"
   | ~/[^ab]/ "lol"
|]
    in kleenexIdTest prog "abc"


kleenexIdTest :: String -> String -> IO TS.Result
kleenexIdTest prog str =
    let inp = concatMap encodeChar str in
    case H.testKleenex prog of
      Left err -> return $ TS.Error err
      Right m  ->
          let ssts = map (flip sstFromFST True) ((map (fromMu . fst) m) :: [FST Int (RangeSet Word8) (H.KleenexOutTerm :+: (NullFun Word8 [Word8]))])
              out  = foldl (\acc sst -> SST.flattenStream $ SST.run sst acc) inp ssts
          in if inp == out
             then return TS.Pass
             else return $ TS.Fail $ "Identity failed: expected '" ++ show inp
                                  ++ "', got '" ++ show out ++ "'"

