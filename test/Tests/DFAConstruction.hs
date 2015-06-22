{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.DFAConstruction (dfaOptimizationTests) where

import           Control.Monad (join)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Monoid
import           Data.Word
import qualified Distribution.TestSuite as TS

import           KMC.DFAConstruction
import           KMC.FSTConstruction
import           KMC.SymbolicFST
import           KMC.OutputTerm
import           KMC.Theories
import           KMC.Kleenex.Lang
import           KMC.Util.Heredoc
import           KMC.RangeSet
import           KMC.Util.List
    
import           Tests.TestUtils

type TestFST = FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
type W8String = [Word8]

dfaOptimizationTests :: [TS.Test]
dfaOptimizationTests =
    [ simpleGroup True "DFA optimization correctness"
                  [ run test'dfa1
                  , run test'dfa2
                  , run test'dfa3
                  ]
    ]
    where
      run = uncurry simpleTest
    
-- Compare runs of FST with and without DFA optimization

data WithDFA = WithDFA | WithoutDFA deriving (Show)

getFST :: WithDFA -> String 
       -> Either String TestFST
getFST wdfa m = case testKleenex m of
                  Left err    -> Left err
                  Right ((t, m):_) -> case wdfa of
                                        WithDFA -> Right (fromMu t)
                                        WithoutDFA -> Right (fromMuWithDFA m t)

runOn :: (TestFST, TestFST) -> [String] -> [([W8String], [W8String])]
runOn (fst1, fst2) = map go
    where
      go :: String -> ([W8String], [W8String])
      go input = (run fst1 (conv input), run fst2 (conv input))

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
                 TS.Fail $ "FST outputs: " ++ show (map fst o)
                        ++ "DFA outputs: " ++ show (map snd o)

test'dfa1 :: (TestName, IO TS.Result)
test'dfa1 = "DFA opt. 1" <@>
            let p = [strQ|x
x := (~aaa | aa)*
aaa := /aaa/ "bcd"
aa := /aa/ "de"
    |]
            in
              assertOutputsEqual p
                ["", "a", "aa", "aaa", "aaaa", "aaaaa", "aaaaaa"]

test'dfa2 :: (TestName, IO TS.Result)
test'dfa2 = "DFA opt. 2" <@>
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
test'dfa3 = "DFA opt. 3" <@>
            let p = [strQ|x
x := (keep | ~drop)*
keep := /[abc]/
drop := /[def]/
|]
            in
              assertOutputsEqual p $
                  [ "", "a", "ad", "abcd", "defabd", "dabedefbadb"
                  , "dabcdeafd", "abcdefabcde", "defdefdeffedfedea"
                  , "abcabccbd", "qwqw"
                  ]
