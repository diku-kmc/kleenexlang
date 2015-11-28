module KMC.Frontend.Options
       (MainOptions(..)
       ,CompileOptions(..)
       ,VisualizeOptions(..)
       ,VisStage(..)
       ,prettyOptions
       )
       where

import Data.List (intercalate)
import KMC.Program.Backends.C (CType(..))
import Options

data MainOptions =
    MainOptions
    { optQuiet            :: Bool
    , optOptimizeSST      :: Int
    , optPreFunctionalize :: Bool
    , optLookahead        :: Bool
    , optExpressionArg    :: Bool
    , optActionEnabled    :: Bool
    , optConstructDFA     :: Bool -- ^ Enable DFA optimization
    , optSuppressBits     :: Bool -- ^ Don't generate bitcodes that can be safely suppressed
    }

data CompileOptions =
    CompileOptions
    { optOptimizeLevelCC :: Int
    , optOutFile         :: Maybe FilePath
    , optCFile           :: Maybe FilePath
    , optWordSize        :: CType
    , optAltCompiler     :: FilePath
    , optElimIdTables    :: Bool
    }

data VisualizeOptions =
   VisualizeOptions
   { optVisStage :: VisStage
   , optVisPhase :: Int
   , optVisOut   :: Maybe FilePath
   }

data VisStage = VisFST | VisSST | VisASST

visStageOptionType :: OptionType VisStage
visStageOptionType =
  optionType "fst|sst|asst"
             VisFST
             (\s -> case s of
                      "fst"  -> Right VisFST
                      "sst"  -> Right VisSST
                      "asst" -> Right VisASST
                      _ -> Left $ "\"" ++ s ++ "\" is not a valid automaton type")
             (\t -> case t of
                      VisFST  -> "fst"
                      VisSST  -> "sst"
                      VisASST -> "asst")

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
      <*> simpleOption "act" True "Enable actions in the language"
      <*> simpleOption "dfa" False "Treat ignored Kleenex-subterms as DFAs"
      <*> simpleOption "sb"  True "Avoid generating bits for suppressed terms whenever safe"

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

instance Options VisualizeOptions where
    defineOptions =
        VisualizeOptions
        <$> defineOption visStageOptionType
                (\o -> o { optionLongFlags = ["visstage"]
                         , optionDefault = VisFST
                         , optionDescription = "Automaton to visualize"
                         })
        <*> simpleOption "visphase" 1 "Which stage in the pipeline to visualize."
        <*> simpleOption "visout" Nothing ("Save visualization to file (determine type from extension). "
                                           ++ "If not set, attempt to show visualization in window.")

prettyOptions :: MainOptions -> CompileOptions -> String
prettyOptions mainOpts compileOpts = intercalate "\\n"
                     [ "SST optimization level:  " ++ show (optOptimizeSST mainOpts)
                     , "Word size:               " ++ show (optWordSize compileOpts)
                     , "Identity tables removed: " ++ show (optElimIdTables compileOpts)
                     , "DFA optimization:        " ++ show (optConstructDFA mainOpts)
                     ]
