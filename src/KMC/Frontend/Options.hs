module KMC.Frontend.Options
       (MainOptions(..)
       ,CompileOptions(..)
       ,SimulateOptions(..)
       ,VisualizeOptions(..)
       ,SimulationType(..)
       ,VisStage(..)
       ,prettyOptions
       )
       where

import Control.Applicative
import Data.List (intercalate)
import KMC.Program.Backends.C (CType(..))
import KMC.Kleenex.ApproximationMetrics
import Options
import Prelude

data MainOptions =
    MainOptions
    { optQuiet            :: Bool -- ^ Do not generate console output
    , optReport           :: Bool -- ^ Generate compilation report on standard output
    , optOptimizeSST      :: Int  -- ^ SST optimization level (0-3)
    , optPreFunctionalize :: Bool -- ^ Functionalize oracle before SST gen
    , optLookahead        :: Bool -- ^ Use finite lookahead in SSTs
    , optExpressionArg    :: Bool -- ^ Treat input as RE, generate bit-coder
    , optActionEnabled    :: Bool -- ^ Decompose into oracle/action
    , optSuppressBits     :: Bool -- ^ Don't generate bitcodes that can be safely suppressed
    , optIte              :: Bool -- ^ Use iterative approximation
    , optApproxMetric     :: ApproxMetric -- ^ The metric used for approximation
    , optApproxMode       :: ApproxMode -- ^ Output mode for approximate matching
    }

data CompileOptions =
    CompileOptions
    { optOptimizeLevelCC :: Int            -- ^ CC optimization level
    , optOutFile         :: Maybe FilePath -- ^ Binary output file
    , optCFile           :: Maybe FilePath -- ^ Intermediate source file
    , optWordSize        :: CType          -- ^ Word size in run-time buffer
    , optAltCompiler     :: FilePath       -- ^ Alternative compiler
    , optElimIdTables    :: Bool           -- ^ Eliminate identity tables in generated code
    }

data SimulateOptions =
    SimulateOptions
    { optSimulationType :: SimulationType -- ^ Simulation type (streaming FST, backtracking FST, SST sim)
    }

data SimulationType = SimLockstepFST           -- ^ Straightforward lockstepped FST simulation
                    | SimLinearBacktrackingFST -- ^ Linear-time backtracking FST simulation
                    | SimSST                   -- ^ Streaming SST simulation
                    deriving (Eq, Ord)

data VisualizeOptions =
   VisualizeOptions
   { optVisStage :: VisStage
   , optVisPhase :: Int
   , optVisOut   :: Maybe FilePath
   }

data VisStage = VisTransducer -- ^ Visualize the generated transducer
              | VisOracle     -- ^ Visualize the oracle transducer
              | VisAction     -- ^ Visualize the action machine
              | VisSST        -- ^ Visualize the SST (composed)
              | VisOracleSST  -- ^ Visualize the oracle SST
              | VisActionSST  -- ^ Visualize the action SST
              deriving (Eq, Ord)

simulationTypeOptionType :: OptionType SimulationType
simulationTypeOptionType =
  optionType "lockstep|backtrack|sst"
             SimLockstepFST
             (\s -> case s of
                 "lockstep"  -> Right SimLockstepFST
                 "backtrack" -> Right SimLinearBacktrackingFST
                 "sst"       -> Right SimSST
                 _           -> Left $ concat ["\"", s, "\" is not a valid simulation type"])
             (\t -> case t of
                 SimLockstepFST           -> "lockstep"
                 SimLinearBacktrackingFST -> "backtrack"
                 SimSST                   -> "sst")

visStageOptionType :: OptionType VisStage
visStageOptionType =
  optionType "transducer|oracle|action|sst|oraclesst|actionsst"
             VisTransducer
             (\s -> case s of
                      "transducer" -> Right VisTransducer
                      "oracle"     -> Right VisOracle
                      "action"     -> Right VisAction
                      "sst"        -> Right VisSST
                      "oraclesst"  -> Right VisOracleSST
                      "actionsst"  -> Right VisActionSST
                      _ -> Left $ "\"" ++ s ++ "\" is not a valid automaton type")
             (\t -> case t of
                      VisTransducer -> "transducer"
                      VisOracle     -> "oracle"
                      VisAction     -> "action"
                      VisSST        -> "sst"
                      VisOracleSST  -> "oraclesst"
                      VisActionSST  -> "actionsst")

approxMetricOptionType :: OptionType ApproxMetric
approxMetricOptionType =
  optionType "LCS|Hamming|Levenshtein"
             LCS (\s -> case s of
                          "LCS" -> Right LCS
                          "Hamming" -> Right Hamming
                          "Levenshtein" -> Right Levenshtein
                          _ -> Left $ "\"" ++ s ++ "\" is not a valid approximation type")
                  (\t -> case t of
                          LCS -> "LCS"
                          Hamming -> "Hamming"
                          Levenshtein -> "Levenshtein")

approxModeOptionType :: OptionType ApproxMode
approxModeOptionType =
  optionType "correction|matching|explicit"
    Correction
    (\s -> case s of
             "correction" -> Right Correction
             "matching"   -> Right Matching
             "explicit"   -> Right Explicit
             _ -> Left $ "\"" ++ s ++ "\" is not a valid approximation mode")
    (\t -> case t of
             Correction   -> "correction"
             Matching     -> "matching"
             Explicit     -> "explicit")

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
      <*> simpleOption "report" False "Write compilation report to standard out (NOTE: ignores quiet option)"
      <*> simpleOption "opt" 3 "SST optimization level (1-3)"
      <*> simpleOption "func" False "Functionalize FST before SST construction (EXPERIMENTAL)"
      <*> simpleOption "la" True "Enable lookahead"
      <*> simpleOption "re" False "Treat argument as a verbatim regular expression (generate bit-coder)"
      <*> simpleOption "act" True "Enable actions in the language"
      <*> simpleOption "sb"  True "Avoid generating bits for suppressed terms whenever safe"
      <*> simpleOption "ite" False "Use iterative approximation"
      <*> defineOption approxMetricOptionType
                (\o -> o { optionLongFlags = ["metric"]
                         , optionDefault = LCS
                         , optionDescription = "Error metric used for approximation"
                         })
      <*> defineOption approxModeOptionType
                (\o -> o { optionLongFlags = ["approxmode"]
                         , optionDefault = Correction
                         , optionDescription = "Output mode when doing approximate matching"
                         })

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
      <*> simpleOption "rmidtbls" False "Eliminate C-tables that implement the identity function."

instance Options SimulateOptions where
    defineOptions =
      SimulateOptions <$> defineOption simulationTypeOptionType
                              (\o -> o { optionLongFlags = ["sim"]
                                       , optionDefault = SimLockstepFST
                                       , optionDescription = "What type of simulator to use"
                                       })

instance Options VisualizeOptions where
    defineOptions =
        VisualizeOptions
        <$> defineOption visStageOptionType
                (\o -> o { optionLongFlags = ["visstage"]
                         , optionDefault = VisTransducer
                         , optionDescription = "Automaton to visualize"
                         })
        <*> simpleOption "visphase" 0 "Which stage in the pipeline to visualize (starting from 0)"
        <*> simpleOption "visout" Nothing ("Save visualization to file (determine type from extension). "
                                           ++ "If not set, attempt to show visualization in window.")

prettyOptions :: MainOptions -> CompileOptions -> String
prettyOptions mainOpts compileOpts = intercalate "\\n"
                     [ "SST optimization level:  " ++ show (optOptimizeSST mainOpts)
                     , "Word size:               " ++ show (optWordSize compileOpts)
                     , "Identity tables removed: " ++ show (optElimIdTables compileOpts)
                     ]

