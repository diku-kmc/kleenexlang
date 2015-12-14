module KMC.Frontend.Commands where

import           KMC.Frontend
import           KMC.Frontend.Options

import           KMC.Determinization (sstFromFST)
import           KMC.Kleenex.Desugaring as DS
import           KMC.Kleenex.Parser
import           KMC.Kleenex.Syntax
import           KMC.Program.Backends.C (compileProgram)
import           KMC.Program.IL (elimIdTables)
import           KMC.SSTCompiler (compile)
import qualified KMC.SymbolicFST as FST
import           KMC.SymbolicFST.ActionMachine (action)
import           KMC.SymbolicFST.OracleMachine (oracle)
import qualified KMC.SymbolicFST.OutputEquivalence as OutEq
import           KMC.SymbolicFST.Transducer (constructTransducer, projectTransducer)
import qualified KMC.SymbolicSST as SST
import qualified KMC.SymbolicSST.ActionSST as ASST
import           KMC.Visualization (fstToDot, sstToDot, graphSize)

import           Control.Monad.Reader
import           Data.Char (toLower)
import qualified Data.GraphViz.Commands as GC
import           Data.Hash.MD5 (md5s, Str(..))
import           Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import           System.Exit (ExitCode(..),exitWith)
import           System.FilePath (takeExtension)
import           System.IO (hPutStrLn,stdout,stderr)

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
buildTransducers pu = do
  mainOpts <- ask
  let rp = puProgram pu
  transducers <- measure "Transducer generation" $
    forM (zip (rprogPipeline rp) ([0..]::[Int])) $ \(ident, i) ->
      measure (unwords ["Transducer",show i]) $
        let t = FST.enumerateStates $ constructTransducer rp ident
            n = FST.fstStateSize t + FST.fstTransSize t
        in n `seq` return t
  lpdoms <-
    if optSuppressBits mainOpts then
      measure "Post-dominators" $
        forM (zip transducers ([0..]::[Int])) $ \(t, i) ->
          measure (unwords ["Transducer",show i]) $
            let pd = OutEq.postDominators t
                n = sum $ map S.size $ M.elems pd
            in n `seq` return (OutEq.lastPostDominator pd)
    else
      return $ replicate (length transducers) $ error "post-dominators not computed"
  return $ TransducerUnit
    { tuTransducers = transducers
    , tuLastPostDominators = lpdoms
    , tuProgramUnit = pu
    }

generateOracleSSTs :: TransducerUnit -> Frontend OracleSSTUnit
generateOracleSSTs tu = do
  mainOpts <- ask
  ssts <- measure "Oracle determinization" $
      forM (zip3 (tuTransducers tu) (tuLastPostDominators tu) ([0..]::[Int])) $ \(t,lpd,i) ->
          measure (unwords ["Oracle",show i]) $
              let preopt = if optSuppressBits mainOpts then OutEq.optOracle lpd else id
                  sst = SST.enumerateVariables
                      $ SST.enumerateStates
                      $ sstFromFST (preopt $ oracle t) (not $ optLookahead mainOpts)
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
      forM (zip3 (tuTransducers tu) (tuLastPostDominators tu) ([0..]::[Int])) $ \(t,lpd,i) ->
          measure (unwords ["Action SST",show i]) $
              let preopt = if optSuppressBits mainOpts then OutEq.optAction lpd else id
                  sst = SST.enumerateVariables
                      $ SST.enumerateStates
                      $ ASST.actionToSST (preopt $ action t)
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
    compileProgram (optWordSize compileOpts)
                   (optOptimizeLevelCC compileOpts)
                   info
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
    compileProgram (optWordSize compileOpts)
                   (optOptimizeLevelCC compileOpts)
                   info
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
    compileProgram (optWordSize compileOpts)
                   (optOptimizeLevelCC compileOpts)
                   info
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
