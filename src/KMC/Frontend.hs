module KMC.Frontend where

import           KMC.Frontend.Options

import           Control.Monad
import           Data.Hash.MD5 (md5s, Str(..))
import           Data.Time (NominalDiffTime,getCurrentTime, diffUTCTime)
import           Data.Word (Word8)
import           KMC.Determinization (sstFromFST)
import           KMC.Kleenex.Actions
import           KMC.Kleenex.Desugaring
import           KMC.Kleenex.Parser
import           KMC.Kleenex.Syntax
import qualified KMC.RangeSet as RS
import qualified KMC.SymbolicFST as FST
import           KMC.SymbolicFST.ActionMachine (ActionMachine, action)
import           KMC.SymbolicFST.OracleMachine (OracleMachine, CodeFunc, oracle)
import           KMC.SymbolicFST.Transducer (Transducer, constructTransducer)
import           KMC.SymbolicSST (SST)
import qualified KMC.SymbolicSST as SST
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

compileOracles :: MainOptions -> TransducerUnit -> IO (OracleSSTUnit)
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
