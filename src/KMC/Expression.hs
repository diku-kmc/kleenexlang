{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module KMC.Expression
(Mu(..))
where

import KMC.Theories

-- | Symbolic mu-recursive expressions with output.
data Mu pred func a =
        Var a -- ^ Recursion variable.
   -- | Loop (forall b. b -> Mu pred func delta b)
      | Loop (a -> Mu pred func a) -- ^ Fixed point.
      | Alt (Mu pred func a) (Mu pred func a) -- ^ Alternation.
      | RW pred func (Mu pred func a) -- ^ Read a symbol matching the given
                                      -- predicate and write an output indexed
                                      -- by the concrete input symbol.
      | W (Rng func) (Mu pred func a) -- ^ Write a constant.
      | Seq (Mu pred func a) (Mu pred func a) -- ^ Sequence
      | Accept -- ^ Accept
