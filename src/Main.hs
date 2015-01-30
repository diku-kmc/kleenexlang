module Main where

import Data.Word (Word8)

import Expression
import FSTConstruction
import KMC.Syntax.Config
import KMC.Syntax.Parser
import OutputTerm
import RangeSet
import SSTConstruction
import SymbolicSST

sstFromFancy :: String
             -> SST (PathTree Var Int)
                    (RangeSet Word8)
                    (OutputTerm (RangeSet Word8) (Either Var Bool))
                    Var
                    Bool
sstFromFancy str =
  case parseRegex fancyRegexParser str of
    Left e -> error e
    Right (_, re) -> sstFromFST (fromMu (fromRegex re))

main :: IO ()
main = return ()
