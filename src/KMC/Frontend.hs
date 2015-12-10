module KMC.Frontend where

import           KMC.Frontend.Options

import           KMC.Determinization (SSTFunc, sstFromFST)
import           KMC.Kleenex.Actions
import           KMC.Kleenex.Desugaring as DS
import           KMC.Kleenex.Parser
import           KMC.Kleenex.Syntax
import           KMC.Program.Backends.C (compileProgram)
import           KMC.Program.IL (elimIdTables)
import qualified KMC.RangeSet as RS
import           KMC.SSTCompiler (compile)
import qualified KMC.SymbolicFST as FST
import           KMC.SymbolicFST.ActionMachine (action)
import qualified KMC.SymbolicFST.ActionMachine as A
import           KMC.SymbolicFST.OracleMachine (CodeFunc, oracle)
import qualified KMC.SymbolicFST.OracleMachine as O
import           KMC.SymbolicFST.Transducer (CopyFunc, constructTransducer, projectTransducer)
import qualified KMC.SymbolicFST.Transducer as T
import           KMC.SymbolicSST (SST)
import qualified KMC.SymbolicSST as SST
import qualified KMC.SymbolicSST.ActionSST as ASST
import           KMC.Visualization (fstToDot, sstToDot, graphSize)

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char (toLower)
import           Data.Functor.Identity (Identity)
import qualified Data.GraphViz.Commands as GC
import           Data.Hash.MD5 (md5s, Str(..))
import           Data.List (intercalate)
import qualified Data.Set as S
import           Data.Time (NominalDiffTime,getCurrentTime, diffUTCTime)
import           Data.Word (Word8)
import           System.Exit
import           System.FilePath (takeExtension)
import           System.IO

-----------------
-- Frontend monad
-----------------

data Phase = Phase { pDuration  :: NominalDiffTime
                   , pName      :: String
                   , pSubPhases :: [Phase]
                   }
  deriving (Show)

data FrontendState =
  FrontendState { fsPhases :: [Phase], fsPhaseDepth :: Int }

type Frontend = ReaderT MainOptions (StateT FrontendState IO)

runFrontend :: MainOptions -> Frontend a -> IO (a, [Phase])
runFrontend mainOpts m = do
  (a, st) <- runStateT (runReaderT m mainOpts) (FrontendState [] 0)
  return (a, fsPhases st)

measure :: String -> Frontend a -> Frontend a
measure phaseName m = do
  quiet <- asks optQuiet
  phases <- gets fsPhases
  depth <- gets fsPhaseDepth
  modify $ \s -> s { fsPhases = [], fsPhaseDepth = depth + 1 }
  when (not quiet) $ liftIO $ do
    hPutStrLn stdout ""
    hPutStr stdout $ replicate (2*depth) ' ' ++ phaseName ++ " ..."
    hFlush stdout
  t0 <- liftIO getCurrentTime
  y <- m
  t1 <- y `seq` liftIO getCurrentTime
  phases' <- gets fsPhases
  when (not quiet) $ liftIO $
    if null phases' then
      hPutStr stdout " Done"
    else do
      hPutStrLn stdout ""
      hPutStr stdout $ replicate (2*depth) ' ' ++ "Done: " ++ phaseName
      hFlush stdout
  let phase = Phase (diffUTCTime t1 t0) phaseName phases'
  modify $ \s -> s { fsPhases = phase:phases, fsPhaseDepth = depth }
  return y


--------
-- Types
--------

data ProgramUnit =
  ProgramUnit { puProgram :: DS.RProg
              , puSourceName  :: String
              , puSourceHash  :: String
              }

type Transducer = T.Transducer Int Word8 (Either Word8 RegAction)
data TransducerUnit =
  TransducerUnit { tuTransducers :: [Transducer]
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

info :: String -> Frontend ()
info msg = do
  quiet <- asks optQuiet
  when (not quiet) $
    liftIO $ hPutStrLn stdout msg

warn :: String -> Frontend ()
warn msg = do
  quiet <- asks optQuiet
  when (not quiet) $
    liftIO $ hPutStrLn stderr msg

fatal :: String -> Frontend a
fatal msg = liftIO $ do
  hPutStrLn stderr msg
  exitWith $ ExitFailure 1

--------------------
-- Frontend commands
--------------------

createProgram :: String -> Frontend ProgramUnit
createProgram arg = do
  mainOpts <- ask
  case getCompileFlavor mainOpts arg of
    KleenexFlavor -> do
      kleenexSrc <- liftIO $ readFile arg
      case parseKleenex kleenexSrc of
        Left e -> liftIO $ do
          hPutStrLn stderr $ show e
          exitWith $ ExitFailure 1
        Right ast -> return ProgramUnit { puProgram = desugarProg ast
                                        , puSourceName = arg
                                        , puSourceHash = md5s (Str kleenexSrc)
                                        }
    RegexFlavor -> do
      (reSrc, sn) <- if optExpressionArg mainOpts then
                       return (arg, "<command line>")
                     else liftIO $ readFile arg >>= \src -> return (src, arg)
      case parseRegex reSrc of
        Left e -> liftIO $ do
          hPutStrLn stderr $ show e
          exitWith $ ExitFailure 1
        Right re -> return ProgramUnit { puProgram = desugarRegex re
                                       , puSourceName = sn
                                       , puSourceHash = md5s (Str reSrc)
                                       }

buildTransducers :: ProgramUnit -> Frontend TransducerUnit
buildTransducers pu = measure "Transducer generation" $ do
  let rp = puProgram pu
  transducers <- forM (zip (rprogPipeline rp) ([0..]::[Int])) $ \(ident, i) ->
                   measure (unwords ["Transducer",show i]) $
                     let t = FST.enumerateStates $ constructTransducer rp ident
                         n = FST.fstStateSize t + FST.fstTransSize t
                      in n `seq` return t
  return $ TransducerUnit
    { tuTransducers = transducers
    , tuProgramUnit = pu
    }

generateOracleSSTs :: TransducerUnit -> Frontend OracleSSTUnit
generateOracleSSTs tu = do
  mainOpts <- ask
  ssts <- measure "Oracle determinization" $
      forM (zip (tuTransducers tu) ([0..]::[Int])) $ \(t,i) ->
          measure (unwords ["Oracle",show i]) $
              let sst = SST.enumerateVariables
                      $ SST.enumerateStates
                      $ sstFromFST (oracle t) (not $ optLookahead mainOpts)
                  n   = S.size $ SST.sstS $ sst
              in n `seq` return sst
  sstopts <- measure "Oracle optimization" $
      forM (zip ssts ([0..]::[Int])) $ \(sst,i) ->
          measure (unwords ["Oracle",show i]) $
              let (sst', j) = SST.optimize (optOptimizeSST mainOpts) sst
               in j `seq` return sst'
  return $ OracleSSTUnit
    { ouTransducers = sstopts
    }

generateActionSSTs :: TransducerUnit -> Frontend ActionSSTUnit
generateActionSSTs tu = do
  mainOpts <- ask
  ssts <- measure "Action SST generation" $
      forM (zip (tuTransducers tu) ([0..]::[Int])) $ \(t,i) ->
          measure (unwords ["Action SST",show i]) $
              let sst = SST.enumerateVariables
                      $ SST.enumerateStates
                      $ ASST.actionToSST (action t)
                  n   = S.size $ SST.sstS sst
               in n `seq` return sst
  sstopts <- measure "Action SST optimization" $
      forM (zip ssts ([0..]::[Int])) $ \(sst,i) ->
          measure (unwords ["Action SST",show i]) $
              let (sst', j) = SST.optimize (optOptimizeSST mainOpts) sst
               in j `seq` return sst'
  return $ ActionSSTUnit
    { auTransducers = sstopts
    }

generateDirectSSTs :: TransducerUnit -> Frontend DirectSSTUnit
generateDirectSSTs tu = do
  mainOpts <- ask
  ssts <- measure "Direct SST generation" $
    forM (zip (tuTransducers tu) ([0..]::[Int])) $ \(t,i) ->
      measure (unwords ["SST",show i]) $
        case projectTransducer t of
          Nothing -> liftIO $ do
            hPutStrLn stderr $ "Transducer contains action symbols - direct SST generation not supported"
            exitWith $ ExitFailure 1
          Just t' -> let sst = SST.enumerateVariables
                             $ SST.enumerateStates
                             $ sstFromFST t' (not $ optLookahead mainOpts)
                         n   = S.size $ SST.sstS sst
                      in n `seq` return sst
  sstopts <- measure "SST optimization" $
      forM (zip ssts ([0..]::[Int])) $ \(sst,i) ->
          measure (unwords ["SST",show i]) $
              let (sst', j) = SST.optimize (optOptimizeSST mainOpts) sst
               in j `seq` return sst'
  return $ DirectSSTUnit
    { duTransducers = sstopts }

compileDirect :: CompileOptions
              -> Bool
              -> String
              -> String
              -> DirectSSTUnit
              -> Frontend ExitCode
compileDirect compileOpts useWordAlignment srcFile srcMd5 du =
  measure "Code generation and compilation" $ do
    mainOpts <- ask
    let optimizeTables = if optElimIdTables compileOpts then elimIdTables else id
    let progs = Left $ map (optimizeTables . compile) (duTransducers du)
    let envInfo =
          intercalate "\\n"
          [ "Options:"
          , prettyOptions mainOpts compileOpts
          , ""
          , "Source file: " ++ srcFile
          , "Source md5:  " ++ srcMd5
          , "SST states:  " ++ intercalate ", " (map (show . S.size . SST.sstS) $ duTransducers du)
          ]
    liftIO $ compileProgram (optWordSize compileOpts)
                            (optOptimizeLevelCC compileOpts)
                            (optQuiet mainOpts)
                            progs
                            (Just envInfo)
                            (optAltCompiler compileOpts)
                            (optOutFile compileOpts)
                            (optCFile compileOpts)
                            useWordAlignment

compileOracleAction :: CompileOptions
                    -> Bool
                    -> String
                    -> String
                    -> OracleSSTUnit
                    -> ActionSSTUnit
                    -> Frontend ExitCode
compileOracleAction compileOpts useWordAlignment srcFile srcMd5 ou au =
  measure "Code generation and compilation" $ do
    mainOpts <- ask
    let optimizeTables = if optElimIdTables compileOpts then elimIdTables else id
    let oprogs = map (optimizeTables . compile) (ouTransducers ou)
    let aprogs = map (optimizeTables . compile) (auTransducers au)
    let progs = Right (zip oprogs aprogs)
    let envInfo =
          intercalate "\\n"
          [ "Options:"
          , prettyOptions mainOpts compileOpts
          , ""
          , "Source file: " ++ srcFile
          , "Source md5:  " ++ srcMd5
          , "Oracle SST states:  " ++ intercalate ", " (map (show . S.size . SST.sstS) $ ouTransducers ou)
          , "Action SST states:  " ++ intercalate ", " (map (show . S.size . SST.sstS) $ auTransducers au)
          ]
    liftIO $ compileProgram (optWordSize compileOpts)
                            (optOptimizeLevelCC compileOpts)
                            (optQuiet mainOpts)
                            progs
                            (Just envInfo)
                            (optAltCompiler compileOpts)
                            (optOutFile compileOpts)
                            (optCFile compileOpts)
                            useWordAlignment

compileCoder :: CompileOptions
             -> Bool
             -> String
             -> String
             -> OracleSSTUnit
             -> Frontend ExitCode
compileCoder compileOpts useWordAlignment srcFile srcMd5 ou =
  measure "Code generation and compilation" $ do
    mainOpts <- ask
    let optimizeTables = if optElimIdTables compileOpts then elimIdTables else id
    let oprogs = map (optimizeTables . compile) (ouTransducers ou)
    let progs = Left oprogs
    let envInfo =
          intercalate "\\n"
          [ "Options:"
          , prettyOptions mainOpts compileOpts
          , ""
          , "Source file: " ++ srcFile
          , "Source md5:  " ++ srcMd5
          , "Oracle SST states:  " ++ intercalate ", " (map (show . S.size . SST.sstS) $ ouTransducers ou)
          ]
    liftIO $ compileProgram (optWordSize compileOpts)
                            (optOptimizeLevelCC compileOpts)
                            (optQuiet mainOpts)
                            progs
                            (Just envInfo)
                            (optAltCompiler compileOpts)
                            (optOutFile compileOpts)
                            (optCFile compileOpts)
                            useWordAlignment

visualize :: VisualizeOptions -> ProgramUnit -> Frontend (IO ExitCode)
visualize visOpts pu = do
  quiet <- asks optQuiet
  let visPhase = optVisPhase visOpts
  when (visPhase < 0 || visPhase >= length (rprogPipeline $ puProgram pu)) $
    fatal "Invalid phase specified for visualization"
  let prog = puProgram pu
  let pu' = pu { puProgram = prog { rprogPipeline = [rprogPipeline prog !! visPhase] } }
  tu <- buildTransducers pu'
  dg <- case optVisStage visOpts of
    VisTransducer -> do
      let dg' = fstToDot $ head $ tuTransducers tu
      measure "DOT code" $ graphSize dg' `seq` return dg'
    VisOracle     -> do
      let dg' = fstToDot $ (oracle $ head $ tuTransducers tu :: OracleMachine)
      measure "DOT code" $ graphSize dg' `seq` return dg'
    VisAction     -> do
      let dg' = fstToDot $ (action $ head $ tuTransducers tu :: ActionMachine)
      measure "DOT code" $ graphSize dg' `seq` return dg'
    VisSST        -> do
      du <- generateDirectSSTs tu
      let dg' = sstToDot $ head $ duTransducers du
      measure "DOT code" $ graphSize dg' `seq` return dg'
    VisOracleSST  -> do
      ou <- generateOracleSSTs tu
      let dg' = sstToDot $ head $ ouTransducers ou
      measure "DOT code" $ graphSize dg' `seq` return dg'
    VisActionSST  -> do
      au <- generateActionSSTs tu
      let dg' = sstToDot $ head $ auTransducers au
      measure "DOT code" $ graphSize dg' `seq` return dg'
  case optVisOut visOpts of
    Nothing ->
      return $ do when (not quiet) $
                    hPutStrLn stdout "Starting Graphviz in canvas mode ..."
                  GC.runGraphvizCanvas GC.Dot dg GC.Xlib
                  return ExitSuccess
    Just f -> do
      let ext = map toLower (takeExtension f)
      case output ext of
        Nothing -> return (putStrLn ("Unknown extension \"" ++ ext ++ "\"")
                           >> return (ExitFailure 1))
        Just out -> do
          _ <- measure "Graphviz" $ liftIO $ GC.runGraphvizCommand GC.Dot dg out f
          return (return ExitSuccess)
  where
    output e = case e of
                 ".pdf"  -> Just GC.Pdf
                 ".png"  -> Just GC.Png
                 ".eps"  -> Just GC.Eps
                 ".dot"  -> Just GC.DotOutput
                 ".svg"  -> Just GC.Svg
                 ".svgz" -> Just GC.SvgZ
                 _ -> Nothing
