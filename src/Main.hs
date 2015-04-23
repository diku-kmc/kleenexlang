{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Applicative
import           Control.Monad (when, forM, forM_)
import           Data.Char (toLower)
import qualified Data.GraphViz.Commands as GC
import           Data.Hash.MD5 (md5s, Str(..))
import           Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Time (NominalDiffTime,getCurrentTime, diffUTCTime)
import           Data.Word
import           Options
import           System.Environment
import           System.Exit (ExitCode(..), exitWith)
import           System.FilePath (takeExtension)
import           System.IO

import           KMC.Bitcoder
import           KMC.FSTConstruction hiding (Var)
import           KMC.Kleenex.Lang (KleenexOutTerm, kleenexToMuTerm)
import           KMC.Kleenex.Parser (parseKleenex)
import           KMC.Program.Backends.C (CType(..), compileProgram)
import           KMC.RangeSet (RangeSet)
import           KMC.SSTCompiler (compileAutomaton)
import           KMC.SSTConstruction (sstFromFST)
import           KMC.SymbolicFST (FST, fstS)
import qualified KMC.SymbolicFST as FST
import           KMC.SymbolicFST.Functionalization (functionalize)
import           KMC.SymbolicSST
import           KMC.Syntax.Config
import           KMC.Syntax.Parser
import           KMC.Theories
import           KMC.Visualization

data MainOptions =
    MainOptions
    { optQuiet            :: Bool
    , optOptimizeSST      :: Int
    , optPreFunctionalize :: Bool
    , optLookahead        :: Bool
    , optExpressionArg    :: Bool
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
      <*> simpleOption "opt" 3 "SST optimization level (1-3)"
      <*> simpleOption "func" False "Functionalize FST before SST construction"
      <*> simpleOption "la" True "Enable lookahead"
      <*> simpleOption "re" False "Treat argument as a verbatim regular expression (generate bit-coder)"

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

-- | Existential type representing transducers that can be determinized,
-- compiled and pretty-printed.
data Transducers where
    Transducers :: (Function f, Ord f, Dom f ~ Word8, Rng f ~ [delta], Pretty f, Pretty delta
                   ,Ord delta, Enum delta, Bounded delta)
                   => [FST Int (RangeSet Word8) f] -> Transducers

-- | Existential type representing determinized transducers that can be compiled.
data DetTransducers where
    DetTransducers :: (Function f, Ord f, Pretty f
                      ,Ord delta, Enum delta, Bounded delta, Pretty delta
                      ,Dom f ~ Word8, Rng f ~ [delta]) =>
                      [SST Int (RangeSet Word8) f Int] -> DetTransducers

transducerSize :: Transducers -> Int
transducerSize (Transducers fsts) = sum $ map (S.size . fstS) fsts

transducerSizeTrans :: Transducer -> Int
transducerSizeTrans (Transducer fst') = length $ FST.edgesToList $ FST.fstE fst'

functionalizeTransducer :: Transducer -> Transducer
functionalizeTransducer (Transducer fst') =
  Transducer $ FST.trim $ FST.enumerateStates $ functionalize fst'

data Flavor = CompilingKleenex | CompilingRegex deriving (Show, Eq)

getCompileFlavor :: [String] -> Flavor
getCompileFlavor args =
    let [arg] = args in case takeExtension arg of
                          ".kex" -> CompilingKleenex
                          ".re"  -> CompilingRegex
                          ".rx"  -> CompilingRegex
                          f      -> error $ unlines [ "Unknown extension: '" ++ f ++ "'."
                                                    , "Expects one of '.kex', '.re', or '.rx'."
                                                    ]
                                   
buildTransducers :: MainOptions -> [String] -> IO (Transducers, String, String, NominalDiffTime)
buildTransducers mainOpts args = do
  let [arg] = args
  let flav = getCompileFlavor args
  timeFSTgen <- getCurrentTime
  (fsts', srcName, src) <-
    if optExpressionArg mainOpts || flav == CompilingRegex then do
      (reSrcName, reSrc) <-
        if optExpressionArg mainOpts then
            return ("<CLI>", arg)
        else do
            src <- readFile arg
            return (arg, src)
      case parseRegex fancyRegexParser reSrc of
        Left e -> do
          hPutStrLn stderr $ e
          exitWith $ ExitFailure 1
        Right (_, re) -> do
          let fst = fromMu $ fromRegex re
          return (Transducers [fst], reSrcName, reSrc)
    else if flav == CompilingKleenex then do
           kleenexSrc <- readFile arg
           let fsts = fstFromKleenex kleenexSrc
           return (Transducers fsts, arg, kleenexSrc)
         else do
           hPutStrLn stderr $ "Unknown compile flavor: " ++ show flav
           exitWith $ ExitFailure 1
  let fsts'' = if optPreFunctionalize mainOpts then functionalizeTransducer fsts' else fsts'
  when (not $ optQuiet mainOpts) $ putStrLn $ "FST states: " ++ show (transducerSize fsts'')
  when (not $ optQuiet mainOpts) $ putStrLn $ "FST transitions: " ++ show (transducerSizeTrans fsts'')
  timeFSTgen' <- getCurrentTime
  return (fsts'', srcName, md5s (Str src), diffUTCTime timeFSTgen' timeFSTgen)

compileTransducers :: MainOptions -> Transducers -> IO (DetTransducers, NominalDiffTime, NominalDiffTime)
compileTransducers mainOpts (Transducers fsts') = do
  timeSSTgen <- getCurrentTime
  let ssts = map (\fst' -> enumerateVariables $ enumerateStates $ 
                           sstFromFST fst' (not $ optLookahead mainOpts)) fsts'
--  when (not $ optQuiet mainOpts) $ do
--    putStrLn $ "SST states: " ++ show (S.size $ sstS sst)
--    putStrLn $ "SST edges : " ++ show (sum $ map length $ M.elems $ sstE sst)
  timeSSTgen' <- getCurrentTime
  timeSSTopt <- getCurrentTime
  let (sstopts, i) = unzip $ map (optimize (optOptimizeSST mainOpts)) ssts
  timeSSTopt' <- i `seq` getCurrentTime
  return (DetTransducers sstopts
         ,diffUTCTime timeSSTgen' timeSSTgen
         ,diffUTCTime timeSSTopt' timeSSTopt)

transducerToProgram :: MainOptions
                    -> CompileOptions
                    -> Bool
                    -> String
                    -> String
                    -> DetTransducers
                    -> IO (ExitCode, NominalDiffTime)
transducerToProgram mainOpts compileOpts useWordAlignment srcFile srcMd5 (DetTransducers ssts) = do
  timeCompile <- getCurrentTime
  let progs = map compileAutomaton ssts
  let envInfo = intercalate "\\n" [ "Options:"
                                  , prettyOptions mainOpts compileOpts
                                  , ""
                                  , "Time:        " ++ show timeCompile
                                  , "Source file: " ++ srcFile
                                  , "Source md5:  " ++ srcMd5
                                  , "SST states:  " ++ intercalate ", " (map (show . S.size . sstS) ssts)
                                  ]
  ret <- compileProgram (optWordSize compileOpts)
                        (optOptimizeLevelCC compileOpts)
                        (optQuiet mainOpts)
                        progs
                        (Just envInfo)
                        (optAltCompiler compileOpts)
                        (optOutFile compileOpts)
                        (optCFile compileOpts)
                        useWordAlignment
  timeCompile' <- getCurrentTime
  return (ret, diffUTCTime timeCompile' timeCompile)


checkArgs :: [String] -> IO ()
checkArgs args = do
  when (length args /= 1) $ do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " <compile|visualize> [options] <kleenex_file|re_file|re>"
    exitWith $ ExitFailure 1
  

compile :: MainOptions -> CompileOptions -> [String] -> IO ExitCode
compile mainOpts compileOpts args = do
  checkArgs args

  -- FST step
  (transducers, srcName, srcMd5, fstGenDuration) <- buildTransducers mainOpts args

  -- SST step
  (ssts, sstGenDuration, sstOptDuration) <- compileTransducers mainOpts transducers
  
  let useWordAlignment = getCompileFlavor args == CompilingKleenex
  (ret, compileDuration) <- transducerToProgram mainOpts compileOpts
                                                useWordAlignment srcName
                                                srcMd5 ssts
  when (not $ optQuiet mainOpts) $ do
    let fmt t = let s = show (round . toRational $ 1000 * t :: Integer)
                in replicate (8 - length s) ' ' ++ s
    putStrLn $ "FST generation (ms)   : " ++ fmt fstGenDuration
    putStrLn $ "SST generation (ms)   : " ++ fmt sstGenDuration
    putStrLn $ "SST optimization (ms) : " ++ fmt sstOptDuration
    putStrLn $ "code generation (ms)  : " ++ fmt compileDuration
    putStrLn $ "total (ms)            : " ++ fmt (sum [fstGenDuration
                                                      ,sstGenDuration
                                                      ,sstOptDuration
                                                      ,compileDuration])
  return ret

visualize :: MainOptions -> VisualizeOptions -> [String] -> IO ExitCode
visualize mainOpts visOpts args = do
  checkArgs args
  (Transducers (fst':rest), _, _, _) <- buildTransducers mainOpts args
  when (not $ null rest) $ hPutStrLn stderr "WARNING: Multiple stages, only the first is visualized"
  dg <- case optVisStage visOpts of
          VisFST -> return $ fstToDot fst'
          VisSST -> do
            (DetTransducers (sst:_),_,_) <- compileTransducers mainOpts (Transducers [fst'])
            return $ sstToDot sst
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

fstFromKleenex :: String -> [FST Int (RangeSet Word8) KleenexOutTerm]
fstFromKleenex str =
  case parseKleenex str of
    Left e -> error e
    Right ih -> map fromMu (kleenexToMuTerm ih)
