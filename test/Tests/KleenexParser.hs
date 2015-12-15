{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.KleenexParser (kleenexParserTests) where

import qualified Distribution.TestSuite as TS
import qualified KMC.Kleenex.Syntax as S

import qualified KMC.Kleenex.Parser as HP
import qualified KMC.Syntax.External as R
import           KMC.Util.Heredoc

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
                      , run kp_one1
                      ]
    , simpleGroup True "Postfix operator stacking"
                      [ run kp_postfix1
                      , run kp_postfix2
                      ]
--    , simpleGroup True "Find locations of suppressed subterms"
--                  [ run kp_test16
--                  , run kp_test17
--                  ]
    ]
    where
      run = uncurry simpleTest


assertOutputsEqual :: String -> String -> IO TS.Result
assertOutputsEqual progA progB =
    case (HP.parseKleenex "" progA, HP.parseKleenex "" progB) of
      (Right sa, Right sb) ->
        let sa' = S.eraseInfoProg sa :: S.Prog ()
            sb' = S.eraseInfoProg sb :: S.Prog ()
         in if sa' == sb' then
               return TS.Pass
            else
              return $ TS.Fail $ "[ " ++ show sa' ++ "] != [ " ++ show sb' ++ "]"
      (Left e, _)         -> return $ TS.Fail (show e)
      (_, Left e)         -> return $ TS.Fail (show e)

assertProgramIs :: String -> S.Prog () -> IO TS.Result
assertProgramIs prog expected =
    case (HP.parseKleenex "" prog) of
      Right p ->
        let p' = S.eraseInfoProg p :: S.Prog ()
        in if p' == expected then
             return TS.Pass
           else
             return $ TS.Fail $ "[ " ++ show p' ++ "] != [ " ++ show expected ++ "]"
      Left e  -> return $ TS.Fail (show e)

{-
assertMarksAre :: String -> Marks -> IO TS.Result
assertMarksAre prog marks =
    case (testKleenex prog) of
      (Right ((_, marks'):_)) -> if marks' == marks
                                 then return TS.Pass
                                 else return $ TS.Fail $
                                      "Marked: " ++ show marks' ++
                                      " Expected: " ++ show marks
      Left e -> return $ TS.Fail e
-}

kp_test1 :: (TestName, IO TS.Result)
kp_test1 = "Mid-of-line //" <@>
    let pa = [strQ|main := "a" /b/ // | "b" /a/ |]
        pb = [strQ|main := "a" /b/ |]
    in assertOutputsEqual pa pb

kp_test2 :: (TestName, IO TS.Result)
kp_test2 = "First-on-line // in def." <@>
    let pa = [strQ|
main := "a" /b/
//    | "c" /d/
      | "e" /f/
|]
        pb = [strQ|
main := "a" /b/
      | "e" /f/
|]
    in assertOutputsEqual pa pb

kp_test2' :: (TestName, IO TS.Result)
kp_test2' = "Second-on-line // in def." <@>
    let pa = [strQ|
main := "a" /b/
 // | "c" /d/
    | "e" /f/
|]
        pb = [strQ|
main := "a" /b/
   | "e" /f/
|]
    in assertOutputsEqual pa pb

kp_test3 :: (TestName, IO TS.Result)
kp_test3 = "Last-line //" <@>
    let pa = [strQ|
main := "a" e
e := "b" c
c := "c" /a/
// c := "d" /b/
|]
        pb = [strQ|
main := "a" e
e := "b" c
c := "c" /a/
|]
    in assertOutputsEqual pa pb

kp_test4 :: (TestName, IO TS.Result)
kp_test4 = "C and C++ style equal" <@>
    let pa = [strQ|
//main := "a" /b/
main := "a" /b/
|]
        pb = [strQ|
/*main := "a" /b/*/
main := "a" /b/
|]
   in assertOutputsEqual pa pb

kp_test5 :: (TestName, IO TS.Result)
kp_test5 = "C comment in term" <@>
    let pa = [strQ|
main := "a" /*"b"*/ /b/
|]
        pb = [strQ|
main := "a" /b/
|]
    in assertOutputsEqual pa pb

kp_test6 :: (TestName, IO TS.Result)
kp_test6 = "C comment remove choice" <@>
    let pa = [strQ|
main := "a" /b/ /* | */ /c/
|]
        pb = [strQ|
main := "a" /b/ /c/
|]
    in assertOutputsEqual pa pb

kp_test8 :: (TestName, IO TS.Result)
kp_test8 = "Comment out last part of file" <@>
    let pa = [strQ|
main := "a" /b/ //|]
        pb = [strQ|
main := "a" /b/|]
    in assertOutputsEqual pa pb

kp_test8' :: (TestName, IO TS.Result)
kp_test8' = "Non-empty comment at end of file" <@>
    let pa = [strQ|
main := "a" /b/ // hello|]
        pb = [strQ|
main := "a" /b/|]
    in assertOutputsEqual pa pb


kp_test7 :: (TestName, IO TS.Result)
kp_test7 = "No newline at end of file" <@>
    let pa = [strQ|
main := "a" /b/|]
        pb = [strQ|
main := "a" /b/
|]
    in assertOutputsEqual pa pb

kp_test9 :: (TestName, IO TS.Result)
kp_test9 = "Indented name def." <@>
    let pa = [strQ|
 main := "a"|]
        pb = [strQ|
main := "a"|]
    in assertOutputsEqual pa pb

kp_test10 :: (TestName, IO TS.Result)
kp_test10 = "Comment before def." <@>
    let pa = [strQ|/* foo */ /* bar */
/*asdf */ start: p >> p /* hey dr. dickhead */
//asfd
p := "a"|]
        pb = [strQ|start: p >> p
p := "a"|]
    in assertOutputsEqual pa pb

kp_test11 :: (TestName, IO TS.Result)
kp_test11 = "Whitespace around := (1)" <@>
    let pa = [strQ|
main:="a"|]
        pb = [strQ|
main := "a"|]
    in assertOutputsEqual pa pb

kp_test12 :: (TestName, IO TS.Result)
kp_test12 = "Whitespace around := (2)" <@>
    let pa = [strQ|
main
   :=
        "a"|]
        pb = [strQ|
main := "a"|]
    in assertOutputsEqual pa pb

kp_test12' :: (TestName, IO TS.Result)
kp_test12' = "Whitespace before first def." <@>
    let pa = [strQ|
start: p
p := "a"|]
        pb = [strQ|start: p
p := "a"|]
    in assertOutputsEqual pa pb

kp_postfix1 :: (TestName, IO TS.Result)
kp_postfix1 = "Stack postfix operators #1" <@>
    let pa = [strQ|
main := /a/*+?**++??
|]
        pb = [strQ|
main := ((((((((/a/*)+)?)*)*)+)+)?)?
|]
    in assertOutputsEqual pa pb

kp_postfix2 :: (TestName, IO TS.Result)
kp_postfix2 = "Stack postfix operators #2" <@>
    let pa = [strQ|
main := x*+?**++??
x := /a/
|]
        pb = [strQ|
main := ((((((((x*)+)?)*)*)+)+)?)?
x := /a/
|]
    in assertOutputsEqual pa pb


-------------------------------------------------------------------------------
-- Some "sanity checks"; ensure that the parser produces the given ASTs.
-------------------------------------------------------------------------------

kp_test13 :: (TestName, IO TS.Result)
kp_test13 = "Sanity check #1" <@>
    let p = [strQ|main := "a" /b/|]
        e = S.Kleenex [S.Ident "main"]
                      [S.Decl (S.Ident "main") (S.Seq (S.Constant "a") (S.RE (R.Chr 'b')))]
    in assertProgramIs p e

kp_test14 :: (TestName, IO TS.Result)
kp_test14 = "Sanity check #2" <@>
    let p = [strQ|main:="a" q | "b" r
r:= /A/
q:= /B/ "B" p
|]
        e = S.Kleenex [ S.Ident "main" ]
                       [ S.Decl (S.Ident "main") (S.Sum (S.Seq (S.Constant "a") (S.Var (S.Ident "q")))
                                                       (S.Seq (S.Constant "b") (S.Var (S.Ident "r"))))
                       , S.Decl (S.Ident "r") (S.RE (R.Chr 'A'))
                       , S.Decl (S.Ident "q") (S.Seq (S.RE (R.Chr 'B'))
                                                       (S.Seq (S.Constant "B")
                                                               (S.Var (S.Ident "p"))))
                       ]
    in assertProgramIs p e

kp_test15 :: (TestName, IO TS.Result)
kp_test15 = "Sanity check #3" <@>
    let p = [strQ|main := (((/a/* | /b/? | ~/c/+)*)+)?
|]
        e = S.Kleenex [S.Ident "main"] [
            S.Decl (S.Ident "main")
                (S.Question $ S.Plus $ S.Star $
                    S.Sum (S.Star $ S.RE (R.Chr 'a')) $
                    S.Sum (S.Question $ S.RE (R.Chr 'b')) $
                           (S.Plus $ S.SuppressOutput $ S.RE $ R.Chr 'c')
            )
          ]
    in assertProgramIs p e

kp_sanity4 :: (TestName, IO TS.Result)
kp_sanity4 = "Sanity check #4" <@>
             let p = [strQ|main := ~(/a/*)|]
                 e = S.Kleenex [S.Ident "main"] [
                      S.Decl (S.Ident "main")
                            (S.SuppressOutput $ S.Star $ S.RE $ (R.Chr 'a'))
                     ]
            in
              assertProgramIs p e

kp_one1 :: (TestName, IO TS.Result)
kp_one1 = "One constructor #1" <@>
          let p = [strQ|main := 1 | (/a/ | 1)|]
              e = S.Kleenex [S.Ident "main"] [
                   S.Decl (S.Ident "main")
                         (S.Sum S.One (S.Sum (S.RE $ R.Chr 'a') S.One))
                  ]
          in
            assertProgramIs p e


{-
-------------------------------------------------------------------------------
-- Check that the correct subterms are identified as being suppressed.
-------------------------------------------------------------------------------
kp_test16 :: (TestName, IO TS.Result)
kp_test16 = "Find suppressed subterms #1" <@>
            let p = [strQ|
main := ~(/abc/ /def/* | (~/abc/ | ~/abc/))
|]
                exp = S.fromList $ [[], [L]]
            in assertMarksAre p exp

kp_test17 :: (TestName, IO TS.Result)
kp_test17 = "Find suppressed subterms #2" <@>
            let p = [strQ|
main := ((/abc/ /def/*) | (~/abc/ | ~/abc/))
|]
                exp = S.fromList $ [[R,L], [L,R,L], [R,R,L]]
            in assertMarksAre p exp
-}
