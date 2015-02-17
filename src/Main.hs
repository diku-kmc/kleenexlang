{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Applicative
import           Control.Monad (when)
import qualified Data.Set as S
import           Data.List (intercalate)
import           Data.Time (getCurrentTime)
import           Data.Hash.MD5 (md5s, Str(..))
import           Data.Word
import           Options
import           System.Environment
import           System.Exit (ExitCode(..), exitWith)
import           System.FilePath (splitFileName, dropExtension)

import           KMC.Expression hiding (Var)
import           KMC.FSTConstruction hiding (Var)
import           KMC.Hased.Lang (HasedOutTerm, hasedToMuTerm)
import           KMC.Hased.Parser (parseHased)
import           KMC.OutputTerm (OutputTerm)
import           KMC.Program.Backends.C (CType(..), compileProgram, renderProgram)
import           KMC.Program.IL (Program)
import           KMC.RangeSet (RangeSet)
import           KMC.SSTCompiler (compileAutomaton)
import           KMC.SSTConstruction (PathTree, Var, sstFromFST)
import           KMC.SymbolicFST (FST, fstS)
import           KMC.SymbolicSST (SST, optimize, Stream, run, sstS, enumerateStates, enumerateVariables)
import           KMC.Syntax.Config (fancyRegexParser)
import           KMC.Syntax.Parser (parseRegex)
import           KMC.Visualization

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

data VisualizeOptions =
   VisualizeOptions
   {}

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

instance Options VisualizeOptions where
    defineOptions =
        pure VisualizeOptions

prettyOptions :: CompileOptions -> String
prettyOptions opts = intercalate "\\n"
                     [ "SST optimization level: " ++ show (optOptimizeSST opts)
                     , "Word size:              " ++ show (optWordSize opts)
                     ]
             
compile :: MainOptions -> CompileOptions -> [String] -> IO ExitCode
compile mainOpts compileOpts args = do
   when (length args /= 1) $ do
     prog <- getProgName
     putStrLn $ "Usage: " ++ prog ++ " compile [options] <hased_file>"
     exitWith $ ExitFailure 1
   let [hasedFile] = args
   hasedSrc <- readFile hasedFile
   let fst = fstFromHased hasedSrc
   when (not $ optQuiet mainOpts) $ putStrLn $ "FST states: " ++ show (S.size $ fstS fst)
   -- replace state and variable names with integers, which are much faster to
   -- compare (speeds up static analysis)
   let sst = enumerateVariables $ enumerateStates $ sstFromFST fst
   when (not $ optQuiet mainOpts) $ putStrLn $ "SST states: " ++ show (S.size $ sstS sst)
   let sstopt = optimize (optOptimizeSST compileOpts) sst
   let prog = compileAutomaton sstopt
   time <- getCurrentTime
   let envInfo = intercalate "\\n" [ "Options:"
                                   , prettyOptions compileOpts
                                   , ""
                                   , "Time:       " ++ show time
                                   , "Hased file: " ++ hasedFile
                                   , "Hased md5:  " ++ md5s (Str hasedSrc)
                                   , "SST states: " ++ show (S.size $ sstS sst)
                                   , "FST states: " ++ show (S.size $ fstS fst)
                                   ]
   compileProgram (optWordSize compileOpts)
                  (optOptimizeLevelCC compileOpts)
                  (optQuiet mainOpts)
                  prog
                  (Just envInfo)
                  (optOutFile compileOpts)
                  (optCFile compileOpts)

visualize :: MainOptions -> VisualizeOptions -> [String] -> IO ExitCode
visualize mainOpts visOpts args = do
  when (length args /= 1) $ do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " visualize [options] <hased_file>"
    exitWith $ ExitFailure 1
  let [hasedFile] = args
  hasedSrc <- readFile hasedFile
  mkViz fstToDot (fstFromHased hasedSrc)
  return ExitSuccess

main :: IO ExitCode
main = runSubcommand
       [ subcommand "compile" compile
       , subcommand "visualize" visualize
       ]

type DFST sigma delta = SST (PathTree Var Int) (RangeSet sigma) (OutputTerm sigma delta) Var

fstFromHased :: String -> FST Int (RangeSet Word8) HasedOutTerm
fstFromHased str =
  case parseHased str of
    Left e -> error e
    Right ih -> fromMu (hasedToMuTerm ih)

sstFromHased :: Int -> String -> SST (PathTree Var Int) (RangeSet Word8) HasedOutTerm Var
sstFromHased opt str = optimize opt $ sstFromFST (fstFromHased str)

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
  compileProgram UInt8T 3 False (compileAutomaton (sstFromFancy str :: DFST Word8 Bool)) Nothing (Just "match") Nothing

runSST :: String -> [Char] -> Stream [Bool]
runSST str = run (sstFromFancy str)
