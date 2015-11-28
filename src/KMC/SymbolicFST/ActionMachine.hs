{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ParallelListComp #-}
module KMC.SymbolicFST.ActionMachine
       (ActionMachine,CodeInputLab(..),  DecodeFunc(..), action)
       where

import Data.Functor.Identity
import KMC.RangeSet (RangeSet)
import KMC.SymbolicFST (FST, mapEdges)
import KMC.SymbolicFST.Transducer (Transducer, CopyFunc(..))
import KMC.Theories (Function(..), Enumerable(..))
import KMC.Util.Coding

type ActionMachine st sigma act digit
  = FST st (CodeInputLab digit) (DecodeFunc (RangeSet sigma) digit (Either sigma act))

-----------------------------------------------------------
-- Predicate and function types for symbolic representation
-----------------------------------------------------------

-- | Data type representing input labels for action machines.
data CodeInputLab b = InputConst [b]  -- ^ Read exactly this constant
                    | InputAny Int    -- ^ Read this number of symbols

-- | Represents functions which decode fixed-width codes into sequences of
-- elements. That is, the code of one element may be followed by the code of
-- another, or the code sequence can be empty. The functions may also ignore
-- their arguments and return a constant result sequence.
data DecodeFunc enum digit c = DecodeArg [enum] | DecodeConst [c]
  deriving (Show)

-- | Instance which injects the decoded result into a sum type (Either)
instance (Enum digit, Bounded digit, Enumerable enum c)
         => Function (DecodeFunc enum digit (Either c x)) where
  type Dom (DecodeFunc enum digit (Either c x)) = [digit]
  type Rng (DecodeFunc enum digit (Either c x)) = [Either c x]
  eval (DecodeArg []) code | null code = []
                           | otherwise = error "non-empty code"
  eval (DecodeArg (enum:enums) :: DecodeFunc enum digit (Either c x)) code =
    let w = bitWidth (boundedSize (undefined :: digit)) (size enum)
        (pre,rest) = splitAt w code
    in (Left (lookupIndex (decodeEnum pre) enum))
       :(eval (DecodeArg enums :: DecodeFunc enum digit (Either c x)) rest)
  eval (DecodeConst y) _ = y
  isConst (DecodeArg _) = Nothing
  isConst (DecodeConst y) = Just y
  inDom code (DecodeArg []) = null code
  inDom code (DecodeArg (enum:enums) :: DecodeFunc enum digit (Either c x)) =
    let w = bitWidth (boundedSize (undefined :: digit)) (size enum)
        (pre,rest) = splitAt w code
    in (decodeEnum pre < size enum) && inDom rest (DecodeArg enums :: DecodeFunc enum digit (Either c x))
  inDom _ (DecodeConst _) = True

-- | Function instance for DecodeFunc which just returns its result without
-- injecting it into a larger type. The identity functor is necessary in the
-- range type to avoid overlapping instances.
instance (Enum digit, Bounded digit, Enumerable enum c)
         => Function (DecodeFunc enum digit (Identity c)) where
  type Dom (DecodeFunc enum digit (Identity c)) = [digit]
  type Rng (DecodeFunc enum digit (Identity c)) = [c]
  eval (DecodeArg []) code | null code = []
                           | otherwise = error "non-empty code"
  eval (DecodeArg (enum:enums) :: DecodeFunc enum digit (Identity c)) code =
    let w = bitWidth (boundedSize (undefined :: digit)) (size enum)
        (pre,rest) = splitAt w code
    in (lookupIndex (decodeEnum pre) enum)
       :(eval (DecodeArg enums :: DecodeFunc enum digit (Identity c)) rest)
  eval (DecodeConst y) _ = map runIdentity y
  isConst (DecodeArg _) = Nothing
  isConst (DecodeConst y) = Just (map runIdentity y)
  inDom code (DecodeArg []) = null code
  inDom code (DecodeArg (enum:enums) :: DecodeFunc enum digit (Identity c)) =
    let w = bitWidth (boundedSize (undefined :: digit)) (size enum)
        (pre,rest) = splitAt w code
    in (decodeEnum pre < size enum) && inDom rest (DecodeArg enums :: DecodeFunc enum digit (Identity c))
  inDom _ (DecodeConst _) = True

------------------------------
-- Action machine construction
------------------------------

-- | Get the underlying action machine for a transducer. This is obtained by
-- removing input labels on symbol transitions; and adding bit code inputs to
-- epsilon transitions.
action :: forall st sigma act digit.
          (Ord st, Enum sigma, Bounded sigma, Ord sigma, Enum digit, Bounded digit)
       => Transducer st sigma act -> ActionMachine st sigma act digit
action = mapEdges symsym symeps epssym epseps
  where
    digitSize = boundedSize (undefined :: digit)

    symsym _q ts = [ (InputAny w, DecodeArg [p],q') | (p,CopyArg,q') <- ts
                                                    , size p > 1
                                                    , let w = bitWidth digitSize (size p) ]
    symeps _q ts = [ (y, q') | (_p,CopyConst y,q') <- ts ]
                   ++ [ ([], q') | (p, CopyArg, q') <- ts, size p <= 1 ]
    epssym _q [_t] = []
    epssym _q ts   = let n = length ts
                     in [ (InputConst (codeFixedWidthEnumSized n ix), DecodeConst y, q')
                        | (y,q') <- ts | ix <- [0..] ]
    epseps _q [t]  = [t]
    epseps _q _    = []
