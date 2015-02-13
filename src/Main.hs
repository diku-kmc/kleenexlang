{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Monad (when)
import Control.Applicative
import Data.Word
import System.Environment
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (splitFileName, dropExtension)
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
    { optOptimizeSST     :: Int
    , optOptimizeLevelCC :: Int
    , optOutFile         :: Maybe FilePath
    , optCFile           :: Maybe FilePath
    , optWordSize        :: CType
    }

ctypeOptionType :: OptionType CType
ctypeOptionType =
  optionType "8|16|32|64"
             UInt8T
             (\s -> case s of
                      "8" -> Right UInt8T
                      "16" -> Right UInt16T
                      "32" -> Right UInt32T
                      "64" -> Right UInt64T
                      _ -> Left $ "\"" ++ s ++ "\" is not a valid word size")
             (\c -> case c of
                      UInt8T -> "8"
                      UInt16T -> "16"
                      UInt32T -> "32"
                      UInt64T -> "64")

instance Options MainOptions where
    defineOptions =
      MainOptions
      <$> simpleOption "quiet" False "Be quiet"

instance Options CompileOptions where
    defineOptions =
      CompileOptions
      <$> simpleOption "opt" 0 "SST optimization level (1-3)"
      <*> simpleOption "copt" 3 "C compiler optimization level (1-3)"
      <*> simpleOption "out" Nothing "Output file"
      <*> simpleOption "srcout" Nothing "Write intermediate C program to given file path"
      <*> defineOption ctypeOptionType
              (\o -> o { optionLongFlags   = ["wordsize"]
                       , optionDefault     = UInt8T
                       , optionDescription = "Buffer word size"
                       })

compile :: MainOptions -> CompileOptions -> [String] -> IO ExitCode
compile mainOpts compileOpts args = do
   when (length args /= 1) $ do
     prog <- getProgName
     putStrLn $ "Usage: " ++ prog ++ " compile [options] <hased_file>"
     exitWith $ ExitFailure 1
   let [hasedFile] = args
   hasedSrc <- readFile hasedFile
   let binFile = maybe (snd $ splitFileName $ dropExtension hasedFile)
                       id
                       (optOutFile compileOpts)
   when (not $ optQuiet mainOpts) $ putStrLn $ "Writing binary " ++ binFile
   compileProgram (optWordSize compileOpts)
                  (optOptimizeLevelCC compileOpts)
                  (optQuiet mainOpts)
                  (progFromHased (optOptimizeSST compileOpts) hasedSrc)
                  binFile
                  (optCFile compileOpts)

main :: IO ExitCode
main = runSubcommand
       [ subcommand "compile" compile
       ]

type DFST sigma delta = SST (PathTree Var Int) (RangeSet sigma) (OutputTerm sigma delta) Var

sstFromHased :: Int -> String -> SST (PathTree Var Int) (RangeSet Word8) HasedOutTerm Var
sstFromHased opt str = 
    case parseHased str of
      Left e -> error e
      Right ih -> optimize opt $ sstFromFST (fromMu (hasedToMuTerm ih))

progFromHased :: Int -> String -> Program Word8
progFromHased opt = compileAutomaton . sstFromHased opt

cFromHased :: Int -> String -> String
cFromHased opt = renderProgram UInt8T . progFromHased opt


{-------------------------------------------}

sstFromFancy :: (Bounded sigma, Enum sigma, Ord sigma) =>
                String
                -> DFST sigma Bool
sstFromFancy str =
  case parseRegex fancyRegexParser str of
    Left e -> error e
    Right (_, re) -> optimize 3 $ sstFromFST $ fromMu $ fromRegex re

progFromFancy :: String -> Program Bool
progFromFancy str = compileAutomaton (sstFromFancy str :: DFST Word8 Bool)

cFromFancy :: String -> String
cFromFancy str =
  renderProgram UInt8T $ compileAutomaton (sstFromFancy str :: DFST Word8 Bool)

compileFancy :: String -> IO ExitCode
compileFancy str =
  compileProgram UInt8T 3 False (compileAutomaton (sstFromFancy str :: DFST Word8 Bool)) "match" Nothing

runSST :: String -> [Char] -> Stream [Bool]
runSST str = run (sstFromFancy str)
