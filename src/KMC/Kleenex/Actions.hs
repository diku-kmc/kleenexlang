module KMC.Kleenex.Actions(RegAction(..)
                          ,Action(..)
                          ,actionSem
                          ,adjActionSem
                          ,runAction) where

import           Data.ByteString.Builder
import           Data.Map (Map, (!))
import qualified Data.Map as M
import           Data.Word (Word8)

import           KMC.Kleenex.Syntax

data RegAction = Push
               | Pop RegIdent
               | Write RegIdent
               deriving (Eq, Ord, Show)

---------------------------------------------
-- Action semantics (used for interpretation)
---------------------------------------------

-- | Semantically, an action denotes a state transformation of a stack together
-- with a string register bank.
newtype Action = Action { runAction' :: (Map RegIdent Builder, [Builder])
                                     -> (Map RegIdent Builder, [Builder]) }

inj :: Word8 -> Action
inj w = Action $ \(store, h:stck) -> (store, (h <> word8 w):stck)

psh :: Action
psh = Action $ \(store, stck) -> (store, mempty:stck)

pop :: RegIdent -> Action
pop r = Action $ \(store, h:stck) -> (M.insert r h store, stck)

wr :: RegIdent -> Action
wr r = Action $ \(store, h:stck) -> (M.insert r mempty store, (h <> store!r):stck)

instance Semigroup Action where
  (Action is1) <> (Action is2) = Action $ is2 . is1

instance Monoid Action where
  mempty = Action id

-- | Interprets an action as a state transformation
actionSem :: RegAction -> Action
actionSem act = case act of
  Push   -> psh
  Pop r  -> pop r
  Write r -> wr r

-- | Interprets a byte alphabet adjoined with actions as a semantic action
adjActionSem :: Either Word8 RegAction -> Action
adjActionSem = either inj actionSem

runAction :: Action -> (Map RegIdent Builder, [Builder])
runAction act = runAction' act (M.empty, [mempty])
