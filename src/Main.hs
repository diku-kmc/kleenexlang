{-# LANGUAGE TypeOperators #-}
module Main where

import KMC.Expression hiding (Var)
import KMC.FSTConstruction hiding (Var)
import KMC.OutputTerm
import KMC.Program.Backends.C
import KMC.Program.IL
import KMC.RangeSet
import KMC.SSTCompiler
import KMC.SSTConstruction
import KMC.SymbolicSST
import KMC.Syntax.Config
import KMC.Syntax.Parser

import Data.Word
import System.Exit (ExitCode)

type DFST sigma delta = SST (PathTree Var Int) (RangeSet sigma) (OutputTerm sigma delta) Var

sstFromFancy :: (Bounded sigma, Enum sigma, Ord sigma) =>
                String
                -> DFST sigma Bool
sstFromFancy str =
  case parseRegex fancyRegexParser str of
    Left e -> error e
    Right (_, re) -> optimize $ sstFromFST $ fromMu $ fromRegex re

progFromFancy :: String -> Program Bool
progFromFancy str = compileAutomaton (sstFromFancy str :: DFST Word8 Bool)

cFromFancy :: String -> String
cFromFancy str =
  renderProgram $ compileAutomaton (sstFromFancy str :: DFST Word8 Bool)

compileFancy :: String -> IO ExitCode
compileFancy str =
  compileProgram (compileAutomaton (sstFromFancy str :: DFST Word8 Bool)) "match"

runSST :: String -> [Char] -> Stream [Bool]
runSST str = run (sstFromFancy str)

main :: IO ()
main = return ()
