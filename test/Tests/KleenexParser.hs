{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.KleenexParser (kleenexParserTests) where

import qualified Data.Set as S
import qualified Distribution.TestSuite as TS
    
import qualified KMC.Kleenex.Parser as HP
import qualified KMC.Syntax.External as R
import           KMC.Util.Heredoc
import           KMC.Expression
import           KMC.Kleenex.Lang
    
import           Tests.TestUtils
    
    
{--------------------------------------------------------------------}
{- Kleenex parser tests                                             -}
{--------------------------------------------------------------------}

kleenexParserTests :: [TS.Test]
kleenexParserTests =
    [ simpleGroup True "Comment behavior"
                      [ run kp_test1
                      , run kp_test2
                      , run kp_test2'
                      , run kp_test3
                      , run kp_test4
                      , run kp_test5
                      , run kp_test6
                      , run kp_test8
                      , run kp_test8'
                      ]
    , simpleGroup True "Whitespace behavior"
                      [ run kp_test7
                      , run kp_test9
                      , run kp_test10
                      , run kp_test11
                      , run kp_test12
                      , run kp_test12'
                      ]
    , simpleGroup True "Basic sanity checks"
                      [ run kp_test13
                      , run kp_test14
                      , run kp_test15
                      , run kp_sanity4
                      ]
    , simpleGroup True "Find locations of suppressed subterms"
                  [ run kp_test16
                  , run kp_test17
                  ]
    ]
    where
      run = uncurry simpleTest


assertOutputsEqual :: String -> String -> IO TS.Result
assertOutputsEqual progA progB =
    case (HP.parseKleenex progA, HP.parseKleenex progB) of
      (Right sa, Right sb) -> if sa == sb
                            then return TS.Pass
                            else return $ TS.Fail $ "[ " ++ show sa ++
                                     "] != [ " ++ show sb ++ "]"
      (Left e, _)         -> return $ TS.Fail e
      (_, Left e)         -> return $ TS.Fail e

assertProgramIs :: String -> HP.Kleenex -> IO TS.Result
assertProgramIs prog expected =
    case (HP.parseKleenex prog) of
      Right (_, p) -> if p == expected
                      then return TS.Pass
                      else return $ TS.Fail $ "[ " ++ show p ++ "] != [ " ++ show expected ++ "]"
      Left e  -> return $ TS.Fail e

assertMarksAre :: String -> Marked -> IO TS.Result
assertMarksAre prog marks =
    case (testKleenex prog) of
      (Right ((_, marks'):_)) -> if marks' == marks
                                 then return TS.Pass
                                 else return $ TS.Fail $
                                      "Marked: " ++ show marks' ++
                                      " Expected: " ++ show marks
      Left e -> return $ TS.Fail e

kp_test1 :: (TestName, IO TS.Result)
kp_test1 = "Mid-of-line //" <@>
    let pa = [strQ|p
p := "a" /b/ // | "b" /a/ |]
        pb = [strQ|p
p := "a" /b/ |]
    in assertOutputsEqual pa pb

kp_test2 :: (TestName, IO TS.Result)
kp_test2 = "First-on-line // in def." <@>
    let pa = [strQ|p
p := "a" /b/
//  | "c" /d/
    | "e" /f/
|]
        pb = [strQ|p
p := "a" /b/
   | "e" /f/
|]
    in assertOutputsEqual pa pb

kp_test2' :: (TestName, IO TS.Result)
kp_test2' = "Second-on-line // in def." <@>
    let pa = [strQ|p
p := "a" /b/
 // | "c" /d/
    | "e" /f/
|]
        pb = [strQ|p
p := "a" /b/
   | "e" /f/
|]
    in assertOutputsEqual pa pb

kp_test3 :: (TestName, IO TS.Result)
kp_test3 = "Last-line //" <@>
    let pa = [strQ|p
p := "a" e
e := "b" c
c := "c" /a/
// c := "d" /b/
|]
        pb = [strQ|p
p := "a" e
e := "b" c
c := "c" /a/
|]
    in assertOutputsEqual pa pb

kp_test4 :: (TestName, IO TS.Result)
kp_test4 = "C and C++ style equal" <@>
    let pa = [strQ|p
//p := "a" /b/
p := "a" /b/
|]
        pb = [strQ|p
/*p := "a" /b/*/
p := "a" /b/
|]
   in assertOutputsEqual pa pb

kp_test5 :: (TestName, IO TS.Result)
kp_test5 = "C comment in term" <@>
    let pa = [strQ|p
p := "a" /*"b"*/ /b/
|]
        pb = [strQ|p
p := "a" /b/
|]
    in assertOutputsEqual pa pb

kp_test6 :: (TestName, IO TS.Result)
kp_test6 = "C comment remove choice" <@>
    let pa = [strQ|p
p := "a" /b/ /* | */ /c/
|]
        pb = [strQ|p
p := "a" /b/ /c/
|]
    in assertOutputsEqual pa pb

kp_test8 :: (TestName, IO TS.Result)
kp_test8 = "Comment out last part of file" <@>
    let pa = [strQ|p
p := "a" /b/ //|]
        pb = [strQ|p
p := "a" /b/|]
    in assertOutputsEqual pa pb

kp_test8' :: (TestName, IO TS.Result)
kp_test8' = "Non-empty comment at end of file" <@>
    let pa = [strQ|p
p := "a" /b/ // hello|]
        pb = [strQ|p
p := "a" /b/|]
    in assertOutputsEqual pa pb
            

kp_test7 :: (TestName, IO TS.Result)
kp_test7 = "No newline at end of file" <@>
    let pa = [strQ|p
p := "a" /b/|]
        pb = [strQ|p
p := "a" /b/
|]
    in assertOutputsEqual pa pb

kp_test9 :: (TestName, IO TS.Result)
kp_test9 = "Indented name def." <@>
    let pa = [strQ|p
p := "a"|]
        pb = [strQ|p
p := "a"|]
    in assertOutputsEqual pa pb

kp_test10 :: (TestName, IO TS.Result)
kp_test10 = "Comment before def." <@>
    let pa = [strQ|/* foo */ /* bar */
/*asdf */ p >> p /* hey dr. dickhead */
//asfd
p := "a"|]
        pb = [strQ|p >> p
p := "a"|]
    in assertOutputsEqual pa pb

kp_test11 :: (TestName, IO TS.Result)
kp_test11 = "Whitespace around := (1)" <@>
    let pa = [strQ|p
p:="a"|]
        pb = [strQ|p
p := "a"|]
    in assertOutputsEqual pa pb

kp_test12 :: (TestName, IO TS.Result)
kp_test12 = "Whitespace around := (2)" <@>
    let pa = [strQ|p
p
   :=
        "a"|]
        pb = [strQ|p
p := "a"|]
    in assertOutputsEqual pa pb

kp_test12' :: (TestName, IO TS.Result)
kp_test12' = "Whitespace before first def." <@>
    let pa = [strQ|
p
p := "a"|]
        pb = [strQ|p
p := "a"|]
    in assertOutputsEqual pa pb

kp_test13 :: (TestName, IO TS.Result)
kp_test13 = "Sanity check #1" <@>
    let p = [strQ|p
p := "a" /b/|]
        e = HP.Kleenex [HP.HA (HP.mkIdent "p", HP.Seq (HP.Constant "a") (HP.RE (R.Chr 'b')))]
    in assertProgramIs p e

kp_test14 :: (TestName, IO TS.Result)
kp_test14 = "Sanity check #2" <@>
    let p = [strQ|p
p:="a" q | "b" r
r:= /A/
q:= /B/ "B" p
|]
        e = HP.Kleenex [ HP.HA (HP.mkIdent "p", HP.Sum (HP.Seq (HP.Constant "a") (HP.Var (HP.mkIdent "q")))
                                                       (HP.Seq (HP.Constant "b") (HP.Var (HP.mkIdent "r"))))
                       , HP.HA (HP.mkIdent "r", HP.RE (R.Chr 'A'))
                       , HP.HA (HP.mkIdent "q", HP.Seq (HP.RE (R.Chr 'B'))
                                                       (HP.Seq (HP.Constant "B")
                                                               (HP.Var (HP.mkIdent "p"))))
                       ]
    in assertProgramIs p e

kp_test15 :: (TestName, IO TS.Result)
kp_test15 = "Sanity check #3" <@>
    let p = [strQ|p
p := (/a/* | /b/? | ~/c/+)*+?
|]
        e = HP.Kleenex [
            HP.HA (HP.mkIdent "p",
                HP.Question $ HP.Plus $ HP.Star $
                    HP.Sum (HP.Star $ HP.RE (R.Chr 'a')) $
                    HP.Sum (HP.Question $ HP.RE (R.Chr 'b')) $
                           (HP.Plus $ HP.Ignore $ HP.RE $ R.Chr 'c')
            )
          ]
    in assertProgramIs p e

kp_sanity4 :: (TestName, IO TS.Result)
kp_sanity4 = "Top-level suppression operator" <@>
             let p = [strQ|x
                           x := ~(/a/*)
                    |]
                 e = HP.Kleenex [
                      HP.HA (HP.mkIdent "x",
                            HP.Ignore $ HP.Star $ HP.RE $ (R.Chr 'a'))
                     ]
            in
              assertProgramIs p e

       
-------------------------------------------------------------------------------
-- Check that the correct subterms are identified as being suppressed.
-------------------------------------------------------------------------------
kp_test16 :: (TestName, IO TS.Result)
kp_test16 = "Find suppressed subterms #1" <@>
            let p = [strQ|p
p := ~(/abc/ /def/* | (~/abc/ | ~/abc/))
|]
                exp = S.fromList $ [[], [L]]
            in assertMarksAre p exp
              
kp_test17 :: (TestName, IO TS.Result)
kp_test17 = "Find suppressed subterms #2" <@>
            let p = [strQ|p
p := ((/abc/ /def/*) | (~/abc/ | ~/abc/))
|]
                exp = S.fromList $ [[R,L], [L,R,L], [R,R,L]]
            in assertMarksAre p exp
