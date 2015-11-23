{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module KMC.Expression where

import qualified Data.Set as S

import           KMC.Theories

-- | Bit-code value for true (top)
bTrue :: (Bounded bit) => bit
bTrue = maxBound

-- | Bit-code value for false (bottom)
bFalse :: (Bounded bit) => bit
bFalse = minBound

-- | Symbolic mu-recursive expressions with output.
data Mu pred func a =
        Var a -- ^ Recursion variable.
   -- | Loop (forall b. b -> Mu pred func delta b)
      | Loop (a -> Mu pred func a) -- ^ Fixed point.
      | Alt (Mu pred func a) (Mu pred func a) -- ^ Alternation.
      | Action func (Mu pred func a) -- Perform a manual register action
      | RW pred func (Mu pred func a) -- ^ Read a symbol matching the given
                                      -- predicate and write an output indexed
                                      -- by the concrete input symbol.
      | W (Rng func) (Mu pred func a) -- ^ Write a constant.
      | Seq (Mu pred func a) (Mu pred func a) -- ^ Sequence
      | Accept -- ^ Accept

showMu :: (Show pred, Show func, Show (Rng func)) => Mu pred func a -> String
showMu (Var _) = "Var!"
showMu (Loop f) = "Loop (" ++ showMu (f undefined) ++ ")"
showMu (Action a m) = "Action (" ++ show a ++ ") (" ++ showMu m ++ ")"
showMu (Alt l r) = "Alt (" ++ showMu l ++ ") (" ++ showMu r ++ ")"
showMu (Seq l r) = "Seq (" ++ showMu l ++ ") (" ++ showMu r ++ ")"
showMu (RW p f m) = "RW (" ++ show p ++ ") (" ++ show f ++ ") (" ++ showMu m ++ ")"
showMu (W r m) = "W (" ++ show r ++ ") (" ++ showMu m ++ ")"
showMu Accept = "Accept"

-- | Denote positions in mu expressions
data Dir = L | R deriving (Eq, Ord, Show)
type Pos = [Dir]
type Marks = S.Set Pos
