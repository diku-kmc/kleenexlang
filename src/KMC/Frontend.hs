module KMC.Frontend where

import           KMC.Frontend.Options

import           KMC.Determinization (SSTFunc)
import           KMC.Kleenex.Actions
import           KMC.Kleenex.Approximation
import qualified KMC.RangeSet as RS
import qualified KMC.SymbolicFST.ActionMachine as A
import           KMC.SymbolicFST.OracleMachine (CodeFunc)
import qualified KMC.SymbolicFST.OracleMachine as O
import           KMC.SymbolicFST.Transducer (CopyFunc)
import qualified KMC.SymbolicFST.Transducer as T
import           KMC.SymbolicSST (SST)
import qualified KMC.SymbolicSST.ActionSST as ASST

import           Control.Monad (when)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor.Identity (Identity)
import           Data.Time (NominalDiffTime,getCurrentTime, diffUTCTime)
import           Data.Word (Word8)
import           System.Exit (ExitCode(..),exitWith)
import           System.FilePath (takeExtension)
import           System.IO (hPutStr,hPutStrLn,hFlush,stdout,stderr)

-----------------
-- Frontend monad
-----------------

data Phase = Phase { pDuration  :: NominalDiffTime
                   , pName      :: String
                   , pSubPhases :: [Phase]
                   }
  deriving (Show)

data FrontendState =
  FrontendState { fsPhases :: [Phase], fsIndentLevel :: Int, fsHangingLine :: Bool }

type Frontend = ReaderT MainOptions (StateT FrontendState IO)

runFrontend :: MainOptions -> Frontend a -> IO (a, [Phase])
runFrontend mainOpts m = do
  (a, st) <- runStateT (runReaderT m mainOpts) (FrontendState [] 0 False)
  return (a, fsPhases st)

measure :: String -> Frontend a -> Frontend a
measure phaseName m = do
  phases <- gets fsPhases
  info' True True $ phaseName ++ " ... "
  indent
  modify $ \s -> s { fsPhases = [] }
  t0 <- liftIO getCurrentTime
  y <- m
  t1 <- y `seq` liftIO getCurrentTime
  phases' <- gets fsPhases
  outdent
  info' False False $ "Done (" ++ phaseName ++ ")"
  let phase = Phase (diffUTCTime t1 t0) phaseName phases'
  modify $ \s -> s { fsPhases = phase:phases }
  return y

indent :: Frontend ()
indent = modify $ \s -> s { fsIndentLevel = fsIndentLevel s + 1 }

outdent :: Frontend ()
outdent = modify $ \s -> s { fsIndentLevel = fsIndentLevel s - 1 }

info' :: Bool -> Bool -> String -> Frontend ()
info' breakLine hang msg = do
  quiet <- asks optQuiet
  hanging <- gets fsHangingLine
  ind <- gets fsIndentLevel
  let spaces = replicate (2*ind) ' '
  when (not quiet) $ do
    when (hanging && breakLine) $ liftIO $ hPutStrLn stdout ""
    when (not hanging || breakLine) $ liftIO $ hPutStr stdout spaces
    if hang
      then liftIO $ hPutStr stdout msg >> hFlush stdout
      else liftIO $ hPutStrLn stdout msg
    modify $ \s -> s { fsHangingLine = hang }

info :: String -> Frontend ()
info = info' True False

warn :: String -> Frontend ()
warn msg = do
  quiet <- asks optQuiet
  when (not quiet) $
    liftIO $ hPutStrLn stderr msg

fatal :: String -> Frontend a
fatal msg = do
  hanging <- gets fsHangingLine
  liftIO $ do
    when hanging $ hPutStrLn stderr ""
    hPutStrLn stderr msg
    exitWith $ ExitFailure 1

--------
-- Types
--------

data ProgramUnit =
  ProgramUnit { puProgram :: RProgAct
              , puSourceName  :: String
              , puSourceHash  :: String
              }

type Transducer = T.Transducer Int Word8 (Either Word8 RegAction)
data TransducerUnit =
  TransducerUnit { tuTransducers :: [Transducer]
                 , tuLastPostDominators :: [Int -> Maybe Int]
                 , tuProgramUnit  :: ProgramUnit
                 }

type OracleSST = SST Int (RS.RangeSet Word8) (SSTFunc (CodeFunc (RS.RangeSet Word8) Word8 Word8)) Int
data OracleSSTUnit =
  OracleSSTUnit { ouTransducers :: [OracleSST]
                }

type ActionSST = ASST.ActionSST Int Word8 Word8 Int
data ActionSSTUnit =
  ActionSSTUnit { auTransducers :: [ActionSST]
                }

type DirectSST = SST Int (RS.RangeSet Word8) (SSTFunc (CopyFunc Word8 [Identity Word8])) Int
data DirectSSTUnit =
  DirectSSTUnit { duTransducers :: [DirectSST] }

-- | These determine the types of the machines used for visualization
type OracleMachine = O.OracleMachine Int Word8 Bool
type ActionMachine = A.ActionMachine Int Word8 RegAction Bool

data Flavor = KleenexFlavor | RegexFlavor deriving (Show, Eq)

--------------------
-- Utility functions
--------------------

getCompileFlavor :: MainOptions -> FilePath -> Flavor
getCompileFlavor mainOpts filename
  | optExpressionArg mainOpts = RegexFlavor
  | otherwise =
    case takeExtension filename of
    ".kex" -> KleenexFlavor
    ".re"  -> RegexFlavor
    ".rx"  -> RegexFlavor
    f      -> error $ unlines [ "Unknown extension: '" ++ f ++ "'."
                              , "Expects one of '.kex', '.re', or '.rx'."
                              ]

