{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Applicative
import           Control.Monad (when)
import           Data.Char (toLower)
import qualified Data.GraphViz.Commands as GC
import           Data.Hash.MD5 (md5s, Str(..))
import           Data.List (intercalate)
import qualified Data.Set as S
import           Data.Time (getCurrentTime, diffUTCTime)
import           Data.Word
import           Options
import           System.Environment
import           System.Exit (ExitCode(..), exitWith)
import           System.FilePath (takeExtension)

import           KMC.FSTConstruction hiding (Var)
import           KMC.Hased.Lang (HasedOutTerm, hasedToMuTerm)
import           KMC.Hased.Parser (parseHased)
import           KMC.Program.Backends.C (CType(..), compileProgram)
import           KMC.RangeSet (RangeSet)
import           KMC.SSTCompiler (compileAutomaton)
import           KMC.SSTConstruction (sstFromFST)
import           KMC.SymbolicFST (FST, fstS)
import           KMC.SymbolicSST (SST, optimize, sstS, enumerateStates, enumerateVariables)
--import           KMC.Syntax.Config (fancyRegexParser)
--import           KMC.Syntax.Parser (parseRegex)
import           KMC.Visualization

data MainOptions =
    MainOptions
    { optQuiet       :: Bool
    , optOptimizeSST :: Int
    }

data CompileOptions =
    CompileOptions
    { optOptimizeLevelCC :: Int
    , optOutFile         :: Maybe FilePath
    , optCFile           :: Maybe FilePath
    , optWordSize        :: CType
    , optAltCompiler     :: FilePath
    }

data VisualizeOptions =
   VisualizeOptions
   { optVisStage :: VisStage
   , optVisOut   :: Maybe FilePath
   }

data VisStage = VisFST | VisSST

visStageOptionType :: OptionType VisStage
visStageOptionType =
  optionType "fst|sst"
             VisFST
             (\s -> case s of
                      "fst" -> Right VisFST
                      "sst" -> Right VisSST
                      _ -> Left $ "\"" ++ s ++ "\" is not a valid automaton type")
             (\t -> case t of
                      VisFST -> "fst"
                      VisSST -> "sst")

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
      <*> simpleOption "opt" 0 "SST optimization level (1-3)"

instance Options CompileOptions where
    defineOptions =
      CompileOptions
      <$> simpleOption "copt" 3 "C compiler optimization level (1-3)"
      <*> simpleOption "out" Nothing "Output file"
      <*> simpleOption "srcout" Nothing "Write intermediate C program to given file path"
      <*> defineOption ctypeOptionType
              (\o -> o { optionLongFlags   = ["wordsize"]
                       , optionDefault     = UInt8T
                       , optionDescription = "Buffer word size"
                       })
      <*> simpleOption "cc" "gcc" "C compiler"

instance Options VisualizeOptions where
    defineOptions =
        VisualizeOptions
        <$> defineOption visStageOptionType
                (\o -> o { optionLongFlags = ["visstage"]
                         , optionDefault = VisFST
                         , optionDescription = "Automaton to visualize"
                         })
        <*> simpleOption "visout" Nothing ("Save visualization to file (determine type from extension). "
                                           ++ "If not set, attempt to show visualization in window.")

prettyOptions :: MainOptions -> CompileOptions -> String
prettyOptions mainOpts compileOpts = intercalate "\\n"
                     [ "SST optimization level: " ++ show (optOptimizeSST mainOpts)
                     , "Word size:              " ++ show (optWordSize compileOpts)
                     ]
             
compile :: MainOptions -> CompileOptions -> [String] -> IO ExitCode
compile mainOpts compileOpts args = do
   when (length args /= 1) $ do
     prog <- getProgName
     putStrLn $ "Usage: " ++ prog ++ " compile [options] <hased_file>"
     exitWith $ ExitFailure 1
   let [hasedFile] = args
   hasedSrc <- readFile hasedFile
   timeFSTgen <- getCurrentTime
   let fst' = fstFromHased hasedSrc
   when (not $ optQuiet mainOpts) $ putStrLn $ "FST states: " ++ show (S.size $ fstS fst')
   timeFSTgen' <- getCurrentTime
   timeSSTgen <- getCurrentTime
   -- replace state and variable names with integers, which are much faster to
   -- compare (speeds up static analysis)
   let sst = enumerateVariables $ enumerateStates $ sstFromFST fst'
   when (not $ optQuiet mainOpts) $ putStrLn $ "SST states: " ++ show (S.size $ sstS sst)
   timeSSTgen' <- getCurrentTime
   timeSSTopt <- getCurrentTime
   let (sstopt, i) = optimize (optOptimizeSST mainOpts) sst
   timeSSTopt' <- i `seq` getCurrentTime
   let prog = compileAutomaton sstopt
   timeCompile <- getCurrentTime
   let envInfo = intercalate "\\n" [ "Options:"
                                   , prettyOptions mainOpts compileOpts
                                   , ""
                                   , "Time:       " ++ show timeCompile
                                   , "Hased file: " ++ hasedFile
                                   , "Hased md5:  " ++ md5s (Str hasedSrc)
                                   , "SST states: " ++ show (S.size $ sstS sst)
                                   , "FST states: " ++ show (S.size $ fstS fst')
                                   ]
   ret <- compileProgram (optWordSize compileOpts)
                         (optOptimizeLevelCC compileOpts)
                         (optQuiet mainOpts)
                         prog
                         (Just envInfo)
                         (optAltCompiler compileOpts)
                         (optOutFile compileOpts)
                         (optCFile compileOpts)
   timeCompile' <- getCurrentTime
   when (not $ optQuiet mainOpts) $ do
     let fstGenDuration = diffUTCTime timeFSTgen' timeFSTgen
         sstGenDuration = diffUTCTime timeSSTgen' timeSSTgen
         sstOptDuration = diffUTCTime timeSSTopt' timeSSTopt
         compileDuration = diffUTCTime timeCompile' timeCompile
         fmt t = let s = show (round . toRational $ 1000 * t :: Integer)
                 in replicate (8 - length s) ' ' ++ s
     putStrLn $ "FST generation (ms)   : " ++ fmt fstGenDuration
     putStrLn $ "SST generation (ms)   : " ++ fmt sstGenDuration
     putStrLn $ "SST optimization (ms) : " ++ fmt sstOptDuration
     putStrLn $ "code generation (ms)  : " ++ fmt compileDuration
     putStrLn $ "total (ms)            : " ++ fmt (fstGenDuration + sstGenDuration + sstOptDuration + compileDuration)
   return ret

visualize :: MainOptions -> VisualizeOptions -> [String] -> IO ExitCode
visualize mainOpts visOpts args = do
  when (length args /= 1) $ do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " visualize [options] <hased_file>"
    exitWith $ ExitFailure 1
  let [hasedFile] = args
  hasedSrc <- readFile hasedFile
  let fst' = fstFromHased hasedSrc
  let sst = enumerateVariables $ enumerateStates $ sstFromFST fst'
  let (sstopt, _) = optimize (optOptimizeSST mainOpts) sst
  let dg = case optVisStage visOpts of
             VisFST -> fstToDot fst'
             VisSST -> sstToDot sstopt
  case optVisOut visOpts of
    Nothing -> do
        GC.runGraphvizCanvas GC.Dot dg GC.Xlib
        return ExitSuccess
    Just f -> do
        let ext = map toLower (takeExtension f)
        case output ext of
          Nothing -> putStrLn ("Unknown extension \"" ++ ext ++ "\"")
                     >> return (ExitFailure 1)
          Just out -> do
            _ <- GC.runGraphvizCommand GC.Dot dg out f
            return ExitSuccess

  where
    output e = case e of
                 ".pdf"  -> Just GC.Pdf
                 ".png"  -> Just GC.Png
                 ".eps"  -> Just GC.Eps
                 ".dot"  -> Just GC.DotOutput
                 ".svg"  -> Just GC.Svg
                 ".svgz" -> Just GC.SvgZ
                 _ -> Nothing

main :: IO ExitCode
main = runSubcommand
       [ subcommand "compile" compile
       , subcommand "visualize" visualize
       ]

fstFromHased :: String -> FST Int (RangeSet Word8) HasedOutTerm
fstFromHased str =
  case parseHased str of
    Left e -> error e
    Right ih -> fromMu (hasedToMuTerm ih)
