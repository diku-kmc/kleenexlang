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
import qualified KMC.Kleenex.Lang as H
import qualified KMC.Kleenex.Parser as HP
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
               , simpleGroup True "Kleenex parsing" kleenexParserTests
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
    in kleenexIdTest prog "a-b-c-d---d-f-eerasdfs-"

-- This was a bug in the C generation.  Works in the SST world it seems.
newline_bug :: IO TS.Result
newline_bug =
    let prog = [strQ|
test := ( keep "\n" | ~drop ) ~<\n> test
      | ( keep "\n" | ~drop ) ~<\n>
keep := <(a|b)+(a|b)(a|b)+>
drop := <[^\n]*>
|]
    in kleenexIdTest prog $ unlines ["aaaba","aabbaa"]


kleenexIdTest :: String -> String -> IO TS.Result
kleenexIdTest prog str =
    let inp = concatMap encodeChar str in
    case H.testKleenex prog of
      Left err -> return $ TS.Fail err
      Right m  ->
          let sst = sstFromFST (fromMu m :: FST Int (RangeSet Word8) H.KleenexOutTerm) True
              out = SST.flattenStream $ SST.run sst inp
          in if inp == out
             then return TS.Pass
             else return $ TS.Fail "Identity failed"


{--------------------------------------------------------------------}
{- Kleenex parser tests                                             -}
{--------------------------------------------------------------------}

kleenexParserTests :: [TS.Test]
kleenexParserTests =
    [ simpleGroup True "Comment behaviour"
                      [ uncurry simpleTest kp_test1
                      , uncurry simpleTest kp_test2
                      , uncurry simpleTest kp_test3
                      , uncurry simpleTest kp_test4
                      , uncurry simpleTest kp_test5
                      , uncurry simpleTest kp_test6
                      , uncurry simpleTest kp_test8
                      ]
    , simpleGroup True "Newline behaviour"
                      [ uncurry simpleTest kp_test7
                      ]
    ]

(<@>) :: String -> IO TS.Result -> (String, IO TS.Result)
(<@>) n t = (n, t)
infix 0 <@>

kp_compare :: String -> String -> IO TS.Result
kp_compare progA progB =
    case (HP.parseKleenex progA, HP.parseKleenex progB) of
      (Right sa, Right sb) -> if sa == sb
                            then return TS.Pass
                            else return $ TS.Fail $ "[ " ++ show sa ++
                                     "] != [ " ++ show sb ++ "]"
      (Left e, _)         -> return $ TS.Fail e
      (_, Left e)         -> return $ TS.Fail e

kp_test1 :: (String, IO TS.Result)
kp_test1 = "Mid-of-line //" <@>
    let pa = [strQ|p := "a" <b> // | "b" <a>
|]
        pb = [strQ|p := "a" <b>
|]
    in kp_compare pa pb

kp_test2 :: (String, IO TS.Result)
kp_test2 = "First-on-line // in def." <@>
    let pa = [strQ|
p := "a" <b>
//  | "c" <d>
    | "e" <f>
|]
        pb = [strQ|
p := "a" <b>
   | "e" <f>
|]
    in kp_compare pa pb

kp_test3 :: (String, IO TS.Result)
kp_test3 = "Last-line //" <@>
    let pa = [strQ|
p := "a" e
e := "b" c
c := "c" <a>
// c := "d" <b>
|]
        pb = [strQ|
p := "a" e
e := "b" c
c := "c" <a>
|]
    in kp_compare pa pb

kp_test4 :: (String, IO TS.Result)
kp_test4 = "C and C++ style equal" <@>
    let pa = [strQ|
//p := "a" <b>
p := "a" <b>
|]
        pb = [strQ|
/*p := "a" <b>*/
p := "a" <b>
|]
   in kp_compare pa pb

kp_test5 :: (String, IO TS.Result)
kp_test5 = "C comment in term" <@>
    let pa = [strQ|
p := "a" /*"b"*/ <b>
|]
        pb = [strQ|
p := "a" <b>
|]
    in kp_compare pa pb

kp_test6 :: (String, IO TS.Result)
kp_test6 = "C comment remove choice" <@>
    let pa = [strQ|
p := "a" <b> /* | */ <c>
|]
        pb = [strQ|
p := "a" <b> <c>
|]
    in kp_compare pa pb

kp_test8 :: (String, IO TS.Result)
kp_test8 = "Comment out last part of file" <@>
    let pa = [strQ|p := "a" <b> //|]
        pb = [strQ|p := "a" <b>|]
    in kp_compare pa pb

kp_test7 :: (String, IO TS.Result)
kp_test7 = "No newline at end of file" <@>
    let pa = [strQ|p := "a" <b>|]
        pb = [strQ|p := "a" <b>
|]
    in kp_compare pa pb
