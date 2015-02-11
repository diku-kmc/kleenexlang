{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Monad (when)
import Control.Applicative
import Data.Word
import System.Environment
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (splitFileName, dropExtension, replaceExtension)
import Options

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
import KMC.Hased.Parser
import KMC.Hased.Lang

data MainOptions =
    MainOptions
    { optQuiet :: Bool
    }

data CompileOptions =
    CompileOptions
    { optOptimizeSST     :: Bool
    , optOptimizeLevelCC :: Int
    }

instance Options MainOptions where
    defineOptions =
      MainOptions
      <$> simpleOption "quiet" False "Be quiet"

instance Options CompileOptions where
    defineOptions =
      CompileOptions
      <$> simpleOption "opt" False "Enable SST optimization"
      <*> simpleOption "copt" 3 "C compiler optimization level"

compile :: MainOptions -> CompileOptions -> [String] -> IO ExitCode
compile mainOpts compileOpts args = do
   when (length args /= 1) $ do
     prog <- getProgName
     putStrLn $ "Usage: " ++ prog ++ " compile [options] <hased_file>"
     exitWith $ ExitFailure 1
   let [hasedFile] = args
   hasedSrc <- readFile hasedFile
   let binFile = snd $ splitFileName $ dropExtension hasedFile
   let cFile   = replaceExtension hasedFile "c"
   when (not $ optQuiet mainOpts) $
     putStrLn $ "Writing binary " ++ binFile ++ " and C source " ++ cFile
   compileProgram UInt8T (optOptimizeLevelCC compileOpts)
                         (optQuiet mainOpts)
                         (progFromHased (optOptimizeSST compileOpts) hasedSrc)
                         binFile
                         (Just cFile)

main :: IO ExitCode
main = runSubcommand
       [ subcommand "compile" compile
       ]

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
  renderProgram UInt8T $ compileAutomaton (sstFromFancy str :: DFST Word8 Bool)

compileFancy :: String -> IO ExitCode
compileFancy str =
  compileProgram UInt8T 3 False (compileAutomaton (sstFromFancy str :: DFST Word8 Bool)) "match" Nothing

sstFromHased :: Bool -> String -> SST (PathTree Var Int) (RangeSet Word8) HasedOutTerm Var
sstFromHased opt str = 
    case parseHased str of
      Left e -> error e
      Right ih -> (if opt then optimize else id) $ sstFromFST (fromMu (hasedToMuTerm ih))

progFromHased :: Bool -> String -> Program Word8
progFromHased opt = compileAutomaton . sstFromHased opt

cFromHased :: Bool -> String -> String
cFromHased opt = renderProgram UInt8T . progFromHased opt
                 
runSST :: String -> [Char] -> Stream [Bool]
runSST str = run (sstFromFancy str)
