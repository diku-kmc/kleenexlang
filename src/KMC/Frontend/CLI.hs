module Main where

import Control.Monad
import KMC.Frontend
import KMC.Frontend.Options
import Options
import System.Environment
import System.Exit

main :: IO ExitCode
main = runSubcommand
       [ subcommand "compile" compileCmd
       , subcommand "visualize" visualizeCmd
       ]

checkArgs :: [String] -> IO ()
checkArgs args = do
  when (length args /= 1) $ do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " <compile|visualize> [options] <kleenex_file>"
    exitWith $ ExitFailure 1

compileCmd :: MainOptions -> CompileOptions -> [String] -> IO ExitCode
compileCmd mainOpts compileOpts args = do
  -- Sanity checking
  checkArgs args

  -- Parse source and generate transducers
  tu <- buildTransducers mainOpts args

  -- Generate Oracle SSTs
  ou <- compileOracles mainOpts tu

  -- Generate Action machines
  assts <- buildActionSSTs mainOpts args

  -- Translate to C program
  let useWordAlignment = getCompileFlavor args == CompilingKleenex
  (ret, compileDuration) <- transducerToProgram mainOpts compileOpts
                                                useWordAlignment srcName
                                                srcMd5 ssts assts

  -- Output stats
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


visualizeCmd :: MainOptions -> VisualizeOptions -> [String] -> IO ExitCode
visualizeCmd mainOpts visOpts args = do undefined
  {-
  let phase = optVisPhase visOpts
  checkArgs args
  (Transducers fsts, _, _, _) <- buildTransducers mainOpts args
  when (phase < 1 || phase > length fsts) $ error "Invalid phase specified for visualization."
  when (length fsts > 1) $ hPutStrLn stderr $ "WARNING: Multiple stages, only phase " ++ show phase ++ " is visualized"
  let fst' = fsts !! (phase-1)
  dg <- case optVisStage visOpts of
          VisFST -> return $ fstToDot fst'
          VisSST -> do
            (DetTransducers (sst:_),_,_) <- compileTransducers mainOpts (Transducers [fst'])
            return $ sstToDot sst
          VisASST -> do
            (DetTransducers ssts)  <- buildActionSSTs mainOpts args
            return $ sstToDot $ ssts !! (phase-1)

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
-}
