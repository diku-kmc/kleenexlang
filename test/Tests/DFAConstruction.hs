{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.DFAConstruction (dfaOptimizationTests) where

import           Control.Monad (join)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Monoid
import qualified Data.Set as S
import           Data.Word
import qualified Distribution.TestSuite as TS

import           KMC.DFAConstruction
import           KMC.FSTConstruction
import           KMC.SSTConstruction
import           KMC.TreeWriter
import           KMC.SymbolicFST
import qualified KMC.SymbolicSST as SST
import           KMC.OutputTerm
import           KMC.Theories
import           KMC.Kleenex.Lang
import           KMC.Util.Heredoc
import           KMC.RangeSet
import           KMC.Util.List
    
import           Tests.TestUtils

type TestPred = RangeSet Word8
type TestFunc = WithNull KleenexOutTerm
type TestFST  = FST Int TestPred TestFunc
type TestSST  = SST.SST (Maybe (Tree Var Int)) TestPred TestFunc Var
type W8String = [Word8]

dfaOptimizationTests :: [TS.Test]
dfaOptimizationTests =
    [ group "DFA/FST optimization correctness"
                  [ run test'dfa1
                  , run test'dfa2
                  , run test'dfa3
                  ]
    , group "DFA/SST optimization correctness"
                  [ run test'dfa4
                  , run test'dfa5
                  , run test'dfa6
                  , run test'dfa7
                  , run test'dfa8
                  ]
    ]
    where
      group = simpleGroup True
      run   = uncurry simpleTest
    

runOn :: (TestFST, TestFST) -> [String] -> [([W8String], [W8String])]
runOn (fst1, fst2) = map go
    where
      go :: String -> ([W8String], [W8String])
      go input = (run fst1 (conv input), run fst2 (conv input))

runOn'SST :: [TestSST] -> [String] -> [[W8String]]
runOn'SST ssts = map (go ssts)
    where
      go :: [TestSST] -> String -> [W8String]
      go ss input = map (SST.flattenStream . flip SST.run (conv input)) ss

conv :: String -> W8String
conv = join . map (BS.unpack . BS8.singleton)

assertOutputsEqual :: String -> [String] -> IO TS.Result
assertOutputsEqual program inputs =
    case testKleenex program of
      Left err    -> return $ TS.Fail err
      Right ((t, m):_) -> return $
          let (withDFA, withOutDFA) = (fromMuWithDFA m t, fromMu t)
              o = runOn (withDFA, withOutDFA) inputs
          in if all (uncurry (==)) o then
                 TS.Pass
             else
                 TS.Fail $ unlines
                       [ "FST outputs: " ++ show (map snd o)
                       , "DFA outputs: " ++ show (map fst o)
                       ]

assertOutputsEqual'SST :: String -> [String] -> IO TS.Result
assertOutputsEqual'SST program inputs =
    case testKleenex program of
      Left err -> return $ TS.Fail err
      Right ((t,m):_) -> return $
          let wDFA'sing       = sstFromFST (fromMuWithDFA m t :: TestFST) True
              wDFA'nonsing    = sstFromFST (fromMuWithDFA m t :: TestFST) False
              noDFA'sing      = sstFromFST (fromMu t :: TestFST) True
              noDFA'nonsing   = sstFromFST (fromMu t :: TestFST) False
              o = runOn'SST [ wDFA'sing, wDFA'nonsing
                            , noDFA'sing, noDFA'nonsing ]
                            inputs
          in if S.size (S.fromList o) == 1 then
                 TS.Pass
             else
                 TS.Fail $ unlines
                       [ "SST/DFA and singleton mode: " ++ show (o !! 0)
                       , "SST/DFA: " ++ show (o !! 1)
                       , "SST and singleton mode: " ++ show (o !! 2)
                       , "SST: " ++ show (o !! 3)
                       ]

test'dfa1 :: (TestName, IO TS.Result)
test'dfa1 = "DFA opt. #1" <@>
            let p = [strQ|x
x := (~aaa | aa)*
aaa := /aaa/ "bcd"
aa := /aa/ "de"
    |]
            in
              assertOutputsEqual p
                ["", "a", "aa", "aaa", "aaaa", "aaaaa", "aaaaaa"]

test'dfa2 :: (TestName, IO TS.Result)
test'dfa2 = "DFA opt. #2" <@>
            let p = [strQ|x
x  := ~(a ba*| ab* b)
a  := /a/
b  := /b/
ab := /ab/
ba := /ba/
|]
            in
              assertOutputsEqual p $
                   map (uncurry alternate)
                       [ (replicate n 'a', replicate n 'b') | n <- [0..10] ]


test'dfa3 :: (TestName, IO TS.Result)
test'dfa3 = "DFA opt. #3" <@>
            let p = [strQ|x
x := (keep | ~drop)*
keep := /[abc]/
drop := /[def]/
|]
            in
              assertOutputsEqual p
                  [ "", "a", "ad", "abcd", "defabd", "dabedefbadb"
                  , "dabcdeafd", "abcdefabcde", "defdefdeffedfedea"
                  , "abcabccbd", "qwqw"
                  ]

test'dfa4 :: (TestName, IO TS.Result)
test'dfa4 = "DFA/SST correctness #1" <@>
            let p = [strQ|x
x := ~(/def*/?)
|]
            in
              assertOutputsEqual'SST p
                  [ "", "de", "def", "deff", "defff"]

test'dfa5 :: (TestName, IO TS.Result)
test'dfa5 = "DFA/SST correctness #2" <@>
            let p = [strQ|x
x := ( y /\n/ | ~fb ~/\n/ ) x
   | ( y /\n/ | ~fb ~/\n/)
fb := /[^\n]*/
y := /[a-z0-9!#$%&'*+\/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+\/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?/
|]
            in
              assertOutputsEqual'SST p $ (:[]) $ unlines
                         [ "", "adfads", "test@example.com"
                         , "wrongmailaddress!", "", "", ""
                         , "someone@diku.dk", "[complex]@123.123.123.123"
                         ]

test'dfa6 :: (TestName, IO TS.Result)
test'dfa6 = "DFA/SST correctness #3" <@>
            let p = [strQ|e
e := ~x
x := ( y /\n/ | ~fb ~/\n/ ) x
   | ( y /\n/ | ~fb ~/\n/)
fb := /[^\n]*/
y := /[a-z0-9!#$%&'*+\/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+\/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?/
|]
            in
              assertOutputsEqual'SST p $ (:[]) $ unlines
                         [ "", "adfads", "test@example.com"
                         , "wrongmailaddress!", "", "", ""
                         , "someone@diku.dk", "[complex]@123.123.123.123"
                         ]

-- In general it is unsound to construct language acceptors and insert them
-- into the FST.  
test'dfa7 :: (TestName, IO TS.Result)
test'dfa7 = "Soundness #1" <@>
            let p = [strQ|x
x := /a/ ~/(b*)??/ /b?/
|]
            in
              assertOutputsEqual p ["ab", "abb"]

test'dfa8 :: (TestName, IO TS.Result)
test'dfa8 = "Soundness #2" <@>
            let p = [strQ|x
x := /a/ ~/(b|bb|b*)??/ /b?/
|]
            in
              assertOutputsEqual p ["a", "ab", "abb", "abbb"]
