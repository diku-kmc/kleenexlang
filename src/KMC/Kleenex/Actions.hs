module KMC.Kleenex.Actions(RegAction(..)) where

import KMC.Kleenex.Syntax

data RegAction = Push
               | Pop RegIdent
               | Write RegIdent
               deriving (Eq, Ord, Show)
