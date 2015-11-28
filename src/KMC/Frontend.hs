module KMC.Frontend where

import           KMC.Frontend.Options

import           Control.Monad
import           Data.Hash.MD5 (md5s, Str(..))
import           Data.List (intercalate)
import qualified Data.Set as S
import           Data.Time (NominalDiffTime,getCurrentTime, diffUTCTime)
import           Data.Word (Word8)
import           KMC.Determinization (sstFromFST)
import           KMC.Kleenex.Actions
import           KMC.Kleenex.Desugaring
import           KMC.Kleenex.Parser
import           KMC.Kleenex.Syntax
import           KMC.Program.Backends.C (compileProgram)
import           KMC.Program.IL (elimIdTables)
import qualified KMC.RangeSet as RS
import           KMC.SSTCompiler (compile)
import qualified KMC.SymbolicFST as FST
import           KMC.SymbolicFST.ActionMachine (action)
import           KMC.SymbolicFST.OracleMachine (CodeFunc, oracle)
import           KMC.SymbolicFST.Transducer (Transducer, constructTransducer)
import           KMC.SymbolicSST (SST)
import qualified KMC.SymbolicSST as SST
import           KMC.SymbolicSST.ActionSST
import           System.Exit
import           System.FilePath (takeExtension)
import           System.IO

data TransducerUnit =
  TransducerUnit { tuTransducers :: [Transducer Int Word8 RegAction]
                 , tuSourceName  :: String
                 , tuSourceHash  :: String
                 , tuDuration    :: NominalDiffTime
                 }

type OracleSST = SST Int (RS.RangeSet Word8) (CodeFunc (RS.RangeSet Word8) Word8 Word8) Int
data OracleSSTUnit =
  OracleSSTUnit { ouTransducers :: [OracleSST]
                , ouDuration    :: NominalDiffTime
                }

data ActionSSTUnit =
  ActionSSTUnit { auTransducers :: [ActionSST Int Word8 Word8 Int]
                , auDuration    :: NominalDiffTime
                }

data Flavor = CompilingKleenex | CompilingRegex deriving (Show, Eq)

getCompileFlavor :: [String] -> Flavor
getCompileFlavor args =
    let [arg] = args in
    case takeExtension arg of
    ".kex" -> CompilingKleenex
    ".re"  -> CompilingRegex
    ".rx"  -> CompilingRegex
    f      -> error $ unlines [ "Unknown extension: '" ++ f ++ "'."
                              , "Expects one of '.kex', '.re', or '.rx'."
                              ]

buildTransducers :: MainOptions -> [String] -> IO TransducerUnit
buildTransducers _mainOpts args = do
  let [arg] = args
  let flav = getCompileFlavor args
  timeFSTgen <- getCurrentTime
  -- Only Kleenex source code is supported at the moment
  when (not $ flav == CompilingKleenex) $ do
    hPutStrLn stderr $ "Unknown or unsupported source type: " ++ show flav
    exitWith $ ExitFailure 2
  kleenexSrc <- readFile arg
  rprog <- case parseKleenex kleenexSrc of
    Left e -> do
      hPutStrLn stderr $ show e
      exitWith $ ExitFailure 1
    Right ast -> return (desugarProg ast)
  let transducers = map (FST.enumerateStates . constructTransducer rprog) (rprogPipeline rprog)
  let sizes = sum $ [ FST.fstStateSize t + FST.fstTransSize t | t <- transducers ]
  -- Count all states and transitions to force the computation to finish before
  -- measuring the time duration
  timeFSTgen' <- sizes `seq` getCurrentTime
  return $ TransducerUnit
    { tuTransducers = transducers
    , tuSourceName  = arg
    , tuSourceHash  = md5s (Str kleenexSrc)
    , tuDuration    = diffUTCTime timeFSTgen' timeFSTgen
    }

compileOracles :: MainOptions -> TransducerUnit -> IO OracleSSTUnit
compileOracles mainOpts tu = do
  timeOracleGen <- getCurrentTime
  let ssts = [ SST.enumerateVariables
               $ SST.enumerateStates
               $ sstFromFST (oracle t) (not $ optLookahead mainOpts)
             | t <- tuTransducers tu ]
  let (sstopts, i) = unzip $ map (SST.optimize (optOptimizeSST mainOpts)) ssts
  timeOracleGen' <- i `seq` getCurrentTime
  return $ OracleSSTUnit
    { ouTransducers = sstopts
    , ouDuration    = diffUTCTime timeOracleGen' timeOracleGen
    }

compileActionSSTs :: MainOptions -> TransducerUnit -> IO ActionSSTUnit
compileActionSSTs mainOpts tu = do
  timeActionGen <- getCurrentTime
  let ssts = [ SST.enumerateVariables
               $ SST.enumerateStates
               $ actionToSST (action t) | t <- tuTransducers tu ]
  let (sstopts, i) = unzip $ map (SST.optimize (optOptimizeSST mainOpts)) ssts
  timeActionGen' <- i `seq` getCurrentTime
  return $ ActionSSTUnit
    { auTransducers = sstopts
    , auDuration    = diffUTCTime timeActionGen' timeActionGen
    }

transducerToProgram :: MainOptions
                    -> CompileOptions
                    -> Bool
                    -> String
                    -> String
                    -> OracleSSTUnit
                    -> ActionSSTUnit
                    -> IO (ExitCode, NominalDiffTime)
transducerToProgram mainOpts compileOpts useWordAlignment srcFile srcMd5 ou au = do
  timeCompile <- getCurrentTime
  let optimizeTables = if optElimIdTables compileOpts then elimIdTables else id
  let oprogs = map (optimizeTables . compile) (ouTransducers ou)
  let aprogs = map (optimizeTables . compile) (auTransducers au)
  let progs = Right (zip oprogs aprogs)
  let envInfo =
        intercalate "\\n"
        [ "Options:"
        , prettyOptions mainOpts compileOpts
        , ""
        , "Time:        " ++ show timeCompile
        , "Source file: " ++ srcFile
        , "Source md5:  " ++ srcMd5
        , "Oracle SST states:  " ++ intercalate ", " (map (show . S.size . SST.sstS) $ ouTransducers ou)
        , "Action SST states:  " ++ intercalate ", " (map (show . S.size . SST.sstS) $ auTransducers au)
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
