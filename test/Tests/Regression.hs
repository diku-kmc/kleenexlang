{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.Regression ( regressionTests ) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Word (Word8)
import qualified Distribution.TestSuite as TS

import           KMC.Determinization
import           KMC.Kleenex.Actions
import           KMC.Kleenex.Desugaring (desugarProg)
import           KMC.Kleenex.Parser (parseKleenex)
import           KMC.Kleenex.Syntax
import           KMC.RangeSet
import           KMC.SymbolicFST as FST
import           KMC.SymbolicFST.ActionMachine
import           KMC.SymbolicFST.OracleMachine
import           KMC.SymbolicFST.Transducer
import           KMC.SymbolicSST as SST
import           KMC.SymbolicSST.ActionSST
import           KMC.Util.Heredoc
    
import           Tests.TestUtils
    
regressionTests :: [TS.Test]
regressionTests =
    [ simpleTest "unsound_lookahead" unsound_lookahead
--    , simpleTest "character class accepts dash" charclass_accept_dash
--    , simpleTest "newline bug" newline_bug
--    , simpleTest "pipeline" pipeline
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


kleenexIdTest :: String -> String -> IO TS.Result
kleenexIdTest prog str =
    let inp = concatMap encodeChar str in
    case parseKleenex prog of
      Left err -> return $ TS.Error (show err)
      Right p  ->
          let dp = desugarProg p
              ts = map (constructTransducer dp) (rprogPipeline dp)
                   :: [Transducer [RIdent] Word8 (Either Word8 RegAction)]
              ots = map oracle ts :: [OracleMachine [RIdent] Word8 Word8]
              ats = map action ts :: [ActionMachine [RIdent] Word8 RegAction Word8]
              ossts = map (SST.enumerateStates . flip sstFromFST True) ots
              assts = map actionToSST ats
              out  = foldl (\acc (osst, asst) ->
                             SST.flattenStream $ SST.run asst
                             $ SST.flattenStream $ SST.run osst acc)
                           inp
                           (zip ossts assts)
          in if inp == out
             then return TS.Pass
             else return $ TS.Fail $ "Identity failed: expected '" ++ show inp
                                  ++ "', got '" ++ show out ++ "'"



encodeChar :: Char -> [Word8]
encodeChar = BS.unpack . encodeUtf8 . T.singleton

charclass_accept_dash :: IO TS.Result
charclass_accept_dash = 
    let prog = [strQ|
main := /[a-z-]*/|]
    in kleenexIdTest prog "a-b-c-d---d-f-eerasdfs-"
