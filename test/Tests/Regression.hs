{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.Regression ( regressionTests ) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Word (Word8)
import qualified Distribution.TestSuite as TS

import           KMC.Determinization
import           KMC.Kleenex.Actions
import           KMC.Kleenex.Desugaring (desugarProg)
import qualified KMC.Kleenex.Desugaring as DS
import           KMC.Kleenex.Parser (parseKleenex)
import           KMC.Kleenex.Syntax
import           KMC.RangeSet
import           KMC.SymbolicFST as FST
import           KMC.SymbolicFST.ActionMachine
import           KMC.SymbolicFST.OracleMachine
import           KMC.SymbolicFST.Transducer
import           KMC.SymbolicSST as SST
import           KMC.SymbolicSST.ActionSST
import           KMC.Theories
import           KMC.Util.Heredoc

import           Tests.TestUtils

regressionTests :: [TS.Test]
regressionTests =
    [ simpleTest "unsound_lookahead" unsound_lookahead
    , simpleTest "character class accepts dash" charclass_accept_dash
    , simpleTest "newline bug" newline_bug
    , simpleTest "pipeline" pipeline
    , simpleTest "range disambiguation" range_disamb
    , simpleTest "enumerateStates not idempotent" enumerateStates_idempotent
    , simpleTest "suppress output desugaring broken" suppressOutput_desugaring
    ]

unsound_lookahead :: IO TS.Result
unsound_lookahead =
  let fst' = constructTransducer la_prog 0
      sst1 = sstFromFST fst' True
      sst2 = sstFromFST fst' False
  in if flattenStream (SST.run sst1 [0,1]) == flattenStream (SST.run sst2 [0,1]) then
         return TS.Pass
     else
         return $ TS.Fail "Lookahead optimization changes output semantics of FST"

la_prog :: RProg Int (Either Int Int)
la_prog =
  RProg
  { rprogPipeline = [0]
  , rprogDecls
    = M.fromList
      [ (0, RSum [1,2])
      , (1, RSeq [3,4,7])
      , (3, RRead (singleton 0) False)
      , (4, RSum [5,6])
      , (5, RRead (singleton 1) False)
      , (6, RRead (singleton 2) False)
      , (7, RConst (Left 0))
      , (2, RSeq [3, 8, 9])
      , (8, RRead (rangeSet [(1,2)]) False)
      , (9, RConst (Left 1))
      ]}

-- This was a bug in the C generation.  Works in the SST world it seems.
newline_bug :: IO TS.Result
newline_bug =
    let prog = [strQ|
main := ( keep "\n" | ~drop ) ~/\n/ main
      | ( keep "\n" | ~drop ) ~/\n/
keep := /(a|b)+(a|b)(a|b)+/
drop := /[^\n]*/
|]
    in kleenexIdTest prog $ unlines ["aaaba","aabbaa"]


-- Test that pipelining works
pipeline :: IO TS.Result
pipeline =
    let prog = [strQ|start: p >> a >> b
p := ~/abc/ "a"
a := /./ "b"
b := /ab/ "c"
   | ~/[^ab]/ "lol"
|]
    in kleenexIdTest prog "abc"


-- This test was failing for compiled Kleenex. Make sure that it works when
-- simulating.
range_disamb :: IO TS.Result
range_disamb =
  let prog = [strQ|main := (test /\n/)*
test := as | bs | cs | ds

as := ~/a/{3} "yep a" | ~/a*/ "nope a"
bs := ~/b/{2,} "yep b" | ~/b*/ "nope b"
cs := ~/c/{,3} "yep c" | ~/c*/ "nope c"
ds := ~/d/{2,3} "yep d" | ~/d*/ "nope d"
|]
      cases = [("\n","nope a\n")
              ,("a\n","nope a\n")
              ,("aa\n","nope a\n")
              ,("aaa\n","yep a\n")
              ,("aaaa\n","nope a\n")
              ,("b\n","nope b\n")
              ,("bb\n","yep b\n")
              ,("bbb\n","yep b\n")
              ,("bbbb\n","yep b\n")
              ,("c\n","yep c\n")
              ,("cc\n","yep c\n")
              ,("ccc\n","yep c\n")
              ,("cccc\n","nope c\n")
              ,("d\n","nope d\n")
              ,("dd\n","yep d\n")
              ,("ddd\n","yep d\n")
              ,("dddd\n","nope d\n")
              ]
   in kleenexIOTest prog cases


kleenexIdTest :: String -> String -> IO TS.Result
kleenexIdTest prog str = kleenexIOTest prog [(str, str)]

kleenexDesugarProg :: String -> (String -> IO a) -> (DS.RProg -> IO a) -> IO a
kleenexDesugarProg prog kfail kret = case parseKleenex "verbatim" prog of
    Left err -> kfail (show err)
    Right p  -> kret (desugarProg p)

kleenexIOTest :: String -> [(String, String)] -> IO TS.Result
kleenexIOTest prog cases =
  kleenexDesugarProg prog (return . TS.Error . show) $ \dp ->
    let ts = map (constructTransducer dp) (rprogPipeline dp)
             :: [Transducer [RIdent] Word8 (Either Word8 RegAction)]
        ots = map oracle ts :: [OracleMachine [RIdent] Word8 Word8]
        ats = map action ts :: [ActionMachine [RIdent] Word8 RegAction Word8]
        ossts = map (SST.enumerateStates . flip sstFromFST True) ots
        assts = map actionToSST ats
        exec s  = foldl (\acc (osst, asst) ->
                          SST.flattenStream $ SST.run asst
                          $ SST.flattenStream $ SST.run osst acc)
                        (concatMap encodeChar s)
                        (zip ossts assts)
        fails = [ (inp,expected,out) | (inp, expected) <- cases
                                     , let out = exec inp
                                     , out /= concatMap encodeChar expected ]
        formatErr (inp,expected,out) =
          "  in: '" ++ inp ++
          "', expected: '" ++ expected ++
          "', got: '" ++ decodeChars out ++ "' ;; "
     in if null fails then return TS.Pass else
           return $ TS.Fail $ "I/O test failed: (" ++ concatMap formatErr fails ++ ")"

encodeChar :: Char -> [Word8]
encodeChar = BS.unpack . encodeUtf8 . T.singleton

decodeChars :: [Word8] -> String
decodeChars = T.unpack . decodeUtf8 . BS.pack


enumerateStates_idempotent :: IO TS.Result
enumerateStates_idempotent = do
  let prog = "main := ~/c/ | ~/c/ \"n\""
  kleenexDesugarProg prog (return . TS.Error . show) $ \dp -> do
    let mach = constructTransducer dp (head $ rprogPipeline dp)
                :: Transducer [RIdent] Word8 (Either Word8 RegAction)
    let mach1 = FST.enumerateStates mach :: Transducer Int Word8 (Either Word8 RegAction)
    let mach2 = FST.enumerateStates mach1 :: Transducer Int Word8 (Either Word8 RegAction)
    if (mach1 == mach2) then
      return TS.Pass
      else
        return $ TS.Fail $ "enumerateStates not idempotent: " ++ show mach1 ++ " =/= " ++ show mach2

suppressOutput_desugaring :: IO TS.Result
suppressOutput_desugaring = do
  let prog = "main := ~def def\ndef := /a|b/"
  kleenexIOTest prog [("aa","a"), ("ab", "b"), ("ba","a"), ("bb","b")]

charclass_accept_dash :: IO TS.Result
charclass_accept_dash =
    let prog = [strQ|
main := /[a-z-]*/|]
    in kleenexIdTest prog "a-b-c-d---d-f-eerasdfs-"
