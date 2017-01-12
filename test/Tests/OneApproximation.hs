{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.OneApproximation where

import           Data.ByteString.Char8 (pack)
import           Data.ByteString (unpack)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Word
import qualified Distribution.TestSuite as TS
import           Test.QuickCheck
import           Tests.TestUtils
import           KMC.Kleenex.Actions
import           KMC.Determinization
import qualified KMC.Kleenex.Approximation as A
import           KMC.Kleenex.ApproximationMetrics
import           KMC.Kleenex.Desugaring
import qualified KMC.Kleenex.Parser as HP
import           KMC.Kleenex.Syntax
import           KMC.SymbolicFST.Transducer
import           KMC.SymbolicSST
import           KMC.Theories
import           KMC.RangeSet

import           KMC.Util.Heredoc

type TestSST = SST Int (RangeSet Word8)
               (SSTFunc (CopyFunc Word8 [Either Word8 RegAction])) Int

type MDecls = M.Map RIdent RTermAct

-- | Converts a string of Kleenex code into a RProg
createRProg :: String -> RProgAct
createRProg s =
      case HP.parseKleenex "" s of
      Right p -> desugarProg p LCS Correction False
      Left  e -> error $ show e

oneApproximationTests :: [TS.Test]
oneApproximationTests =
    -- Unit tests
    [ simpleGroup True "approx_utest_hamming" testHamming
    , simpleGroup True "approx_utest_levenshtein" testLevenshtein
    ] ++
    -- Quickcheck tests
    testCombinator [ ("testProg3", testProg3)]
                   [ LCS, Hamming, Levenshtein]
                   [ 1,2 ]

testCombinator :: [(String, RProgAct)] -> [ApproxMetric] -> [Int] -> [TS.Test]
testCombinator ps ms ks = concat $ map (\p -> concat ( map
                          (\m -> map (\k -> qtest p m k) ks) ms)) ps
  where
    qtest p m k = simpleTest (desc p m k) $ quickTest rpp $
      prop_approx_match (snd p) (testSST (snd p) m k Matching) m k
    desc p m k = "ApproxQC: " ++ (fst p) ++ " " ++ (show m) ++ " " ++ show k
    rpp = 20      -- ^ Number of quickcheck runs pr program

prop_approx_match :: RProgAct -> TestSST -> ApproxMetric -> Int -> Property
prop_approx_match p sst m k = forAll (generateString p m k) $ validateMatch sst

--------------------------------------------------------------------------------
-- Generator
--------------------------------------------------------------------------------

-- | Generates a matching string and a fussed string from a program
generateString :: RProgAct -> ApproxMetric -> Int -> Gen [Word8]
generateString p m k = stringFuss k m $ gms (rprogDecls p) (head (rprogPipeline p))

-- | Generates a random matching string, given a program
gms :: MDecls -> RIdent -> Gen [Word8]
gms d ident =
  case M.lookup ident d of
    Just (RSeq []) -> return []
    Just (RSeq (x1:[])) -> gms d x1
    Just (RSeq (x1:x2:[])) -> do
      ms1 <- gms d x1
      ms2 <- gms d x2
      return $ ms1 ++ ms2
    Just (RRead rs _) -> do
      chr <- elements $ toList rs
      return [chr]
    Just (RConst _) -> return []
    Just (RSum xs)  -> do
      x   <- elements xs
      gms d x
    _ -> error $ "Ill formed program with decls: " ++ show d

-- | Randomly inserts a random element or deletes some element in the list k
-- times
stringFuss :: Int -> ApproxMetric -> Gen [Word8] -> Gen [Word8]
stringFuss 0 _ s = s
stringFuss k LCS st = do
  s <- st
  c <- choose (0,255)
  operation <- elements[addNth c, deleteNth]
  n <- choose(0,(length s))
  stringFuss (k-1) LCS $ return $ operation n s
stringFuss k Hamming st = do
  s <- st
  c <- choose (0,255)
  n <- choose(1,(length s))
  stringFuss (k-1) Hamming $ return $ replaceNth c n s
stringFuss k Levenshtein st = do
  s <- st
  c <- choose (0,255)
  operation <- elements[addNth c, deleteNth, replaceNth c]
  n <- choose(0,(length s))
  stringFuss (k-1) Levenshtein $ return $ operation n s

addNth :: a -> Int -> [a] -> [a]
addNth c n s = let (s1,s2) = splitAt n s in s1++(c:s2)

deleteNth :: Int -> [a] -> [a]
deleteNth n xs = take (n-1) xs ++ drop n xs

replaceNth :: a -> Int -> [a] -> [a]
replaceNth c n xs = take (n-1) xs ++ c : drop n xs

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

-- | Run SST simulation with input
validateMatch :: TestSST -> [Word8] -> Bool
validateMatch ssto a = validateStream a $ run ssto a


-- | Ensures that steam is matching
validateStream :: (Show a, Monoid m) => [a] -> Stream m -> Bool
validateStream i (Fail e) = False
validateStream i Done = True
validateStream i (Chunk x s) = validateStream i s

-- | Creates a SST from a RProg, an ErrorMetric and a approximation value k
testSST :: RProgAct -> ApproxMetric -> Int -> ApproxMode -> TestSST
testSST p m k mode =
  let aprog = A.approxProgIt p k 0 m mode
      ffst = constructTransducer aprog 0
  in enumerateVariables $ enumerateStates $ sstFromFST ffst True

--------------------------------------------------------------------------------
-- Test Programs
--------------------------------------------------------------------------------

testProg1 :: RProgAct
testProg1 = createRProg [strQ|
  main := /a/ |]

testProg2 :: RProgAct
testProg2 = createRProg [strQ|
  main := A | B
  A:= /a/ main
  B:= /b/ |]

testProg3 :: RProgAct
testProg3 = createRProg [strQ|
  main := /b/ B
  B := /b/ C | D
  C := /c/ main
  D := /d/ |]

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

testValidation :: TestSST -> TestSST -> String -> Bool -> String -> TS.Test
testValidation sst1 sst2 st pass name = simpleTest name result
  where
    result = if res1 /= res2
    then return $ TS.Fail "Explicit and non explicit programs does not agree"
    else if pass /= res1
         then return $ TS.Fail "Does not match expected result"
         else return TS.Pass
    res1 = validateMatch sst1 s
    res2 = validateMatch sst2 s
    s    = unpack $ pack st

testLCS :: [TS.Test]
testLCS =
  let sst1 = testSST testProg1 LCS 1 Explicit
      sst2 = testSST testProg1 LCS 1 Correction
  in [ testValidation sst1 sst2 "b" False "LCS: Replace"
     , testValidation sst1 sst2 "ab" True "LCS: Delete on eps"
     , testValidation sst1 sst2 "ba" True "LCS: Delete on read"
     , testValidation sst1 sst2 "" True "LCS: Insert"
     , testValidation sst1 sst2 "abb" False "LCS: Delete twice on eps"
     , testValidation sst1 sst2 "bba" False "LCS: Delete twice on read"]

testHamming :: [TS.Test]
testHamming =
  let sst1 = testSST testProg2 Hamming 1 Correction
      sst2 = testSST testProg2 Hamming 1 Matching
  in [ testValidation sst1 sst2 "bb" True "Hamming: Replace"
     , testValidation sst1 sst2 "abc" False "Hamming: Delete"
     , testValidation sst1 sst2 "" False "Hamming: Insert"
     , testValidation sst1 sst2 "cc" False "Hamming: Replace twice"]

testLevenshtein :: [TS.Test]
testLevenshtein =
  let sst1 = testSST testProg1 Levenshtein 1 Matching
      sst2 = testSST testProg1 Levenshtein 1 Explicit
  in [ testValidation sst1 sst2 "b" True "Levenshtein: Replace"
     , testValidation sst1 sst2 "ab" True "Levenshtein: Delete on eps"
     , testValidation sst1 sst2 "ba" True "Levenshtein: Delete on read"
     , testValidation sst1 sst2 "" True "Levenshtein: Insert"
     , testValidation sst1 sst2 "abb" False "Levenshtein: Delete twice on eps"
     , testValidation sst1 sst2 "bba" False "Levenshtein: Delete twice on read"]
