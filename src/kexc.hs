module Main where

import Control.Monad.Reader
import KMC.Frontend
import KMC.Frontend.Commands
import KMC.Frontend.Options
import Options
import System.Environment
import System.Exit

main :: IO ExitCode
main = runSubcommand
       [ subcommand "compile" compileCmd
       , subcommand "simulate" simulateCmd
       , subcommand "visualize" visualizeCmd
       ]

checkArgs :: [String] -> IO FilePath
checkArgs [fp] = return fp
checkArgs _ = do
  prog <- getProgName
  putStrLn $ "Usage: " ++ prog ++ " <compile|simulate|visualize> [options] <kleenex_file>"
  exitWith $ ExitFailure 1

compileCmd :: MainOptions -> CompileOptions -> [String] -> IO ExitCode
compileCmd mainOpts compileOpts args = do
  -- Sanity checking
  arg <- checkArgs args

  (res, phases) <- runFrontend mainOpts $ measure "Compile" $ do
    pu <- createProgram arg
    tu <- buildTransducers pu
    actionDecomp <- asks optActionEnabled
    case getCompileFlavor mainOpts arg of
      KleenexFlavor | actionDecomp -> do
        ou <- generateOracleSSTs tu
        au <- generateActionSSTs tu
        compileOracleAction compileOpts True (puSourceName pu) (puSourceHash pu) ou au
      KleenexFlavor | otherwise -> do
        du <- generateDirectSSTs tu
        compileDirect compileOpts True (puSourceName pu) (puSourceHash pu) du
      RegexFlavor -> do
        ou <- generateOracleSSTs tu
        compileCoder compileOpts True (puSourceName pu) (puSourceHash pu) ou
  when (optReport mainOpts) $ printPhases phases
  return res

simulateCmd :: MainOptions -> SimulateOptions -> [String] -> IO ExitCode
simulateCmd mainOpts simOpts args = do
  arg <- checkArgs args
  (res, phases) <- runFrontend mainOpts $ measure "Simulate" $ do
    pu <- createProgram arg
    tu <- buildTransducers pu
    case (optSimulationType simOpts) of
      SimLockstepFST -> simulateLockstep simOpts tu
      SimLinearBacktrackingFST -> do
        fatal "Not yet implemented"
      SimSST -> do
        actionDecomp <- asks optActionEnabled
        case getCompileFlavor mainOpts arg of
          KleenexFlavor | actionDecomp -> do
            ou <- generateOracleSSTs tu
            au <- generateActionSSTs tu
            simulateOracleAction simOpts ou au
          KleenexFlavor | otherwise -> do
            du <- generateDirectSSTs tu
            simulateDirect simOpts du
          RegexFlavor -> do
            ou <- generateOracleSSTs tu
            simulateCoder simOpts ou
  when (optReport mainOpts) $ printPhases phases
  return res

visualizeCmd :: MainOptions -> VisualizeOptions -> [String] -> IO ExitCode
visualizeCmd mainOpts visOpts args = do
  arg <- checkArgs args
  (act, phases) <- runFrontend mainOpts $ measure "Visualize" $ do
    pu <- createProgram arg
    visualize visOpts pu
  when (optReport mainOpts) $ printPhases phases
  act

printPhases :: [Phase] -> IO ()
printPhases phases = do
  putStrLn $ replicate 80 '-'
  putStrLn "Compilation report"
  putStrLn $ replicate 80 '-'
  go' 0 phases
  where
    padLeft n c s = replicate (n - length s) c ++ s
    padRight n c s = s ++ replicate (n - length s) c
    fmtD t = let s = show (round . toRational $ 1000 * t :: Integer)
             in s

    go' i       = go i . reverse
    go _ []     = return ()
    go i (p:ps) = do
      let s = concat [padRight 68 '.' $ replicate (2*i) ' ' ++ (pName p)
                     ,padLeft 12 '.' $ fmtD (pDuration p) ++ " ms"]
      putStrLn s
      go' (i+1) (pSubPhases p)
      go i ps
