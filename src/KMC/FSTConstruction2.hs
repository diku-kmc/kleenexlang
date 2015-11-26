{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module KMC.FSTConstruction2 where

import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           KMC.Coding
import           KMC.Kleenex.Syntax hiding (Ident)
import           KMC.RangeSet (RangeSet)
import           KMC.SymbolicFST
import           KMC.Theories

type FSTState = [RIdent]

type Transducer sigma act
  = FST FSTState (RangeSet sigma) (CopyFunc sigma [Either sigma act])

type ActionMachine sigma act digit
  = FST FSTState (CodeInputLab digit) (DecodeFunc (RangeSet sigma) digit (Either sigma act))

type OracleMachine sigma digit
  = FST FSTState (RangeSet sigma) (CodeFunc (RangeSet sigma) sigma digit)

-----------------------------------------------------------
-- Predicate and function types for symbolic representation
-----------------------------------------------------------

-- | Data type representing input labels for action machines.
data CodeInputLab b = InputConst [b]  -- ^ Read exactly this constant
                    | InputAny Int    -- ^ Read this number of symbols

-- | Represents functions which code values as fixed-width codes in an arbitrary
-- base. The functions may also ignore their arguments and return a constant
-- code.
data CodeFunc enum dom digit = CodeArg enum | CodeConst [digit]

-- | Represents functions which decode fixed-width codes into sequences of
-- elements. That is, the code of one element may be followed by the code of
-- another, or the code sequence can be empty. The functions may also ignore
-- their arguments and return a constant result sequence.
data DecodeFunc enum digit c = DecodeArg [enum] | DecodeConst [c]

-- | Represents functions which inject their argument into the range type. The
-- functions may also ignore their argument and return a constant.
data CopyFunc a c = CopyArg | CopyConst c

instance (Enum dom, Bounded dom, Enum digit, Bounded digit, Enumerable enum dom)
         => Function (CodeFunc enum dom digit) where
  type Dom (CodeFunc enum dom digit) = dom
  type Rng (CodeFunc enum dom digit) = [digit]
  eval (CodeArg p) x = codeFixedWidthEnumSized (size p) (indexOf x p)
  eval (CodeConst y) _ = y
  isConst (CodeArg p) = if size p == 1 then
                          Just $ eval (CodeArg p) (lookupIndex 0 p)
                        else
                          Nothing
  isConst (CodeConst y) = Just y
  inDom x (CodeArg p) = member x p
  inDom _ (CodeConst _) = True
--  domain (CodeArg p) = [lookupIndex i p | i <- [0 .. size p - 1]]
--  domain (CodeConst _) = [minBound .. maxBound]

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
--  domain (DecodeArg []) = [[]]
--  domain (DecodeArg (enum:enums)) =
--    [ codeFixedWidthEnumSized (size enum) i ++ rest | i <- [0 .. size enum - 1]
--                                                    , rest <- domain (DecodeArg enums) ]
--  domain (DecodeConst _) = error "cannot compute infinite domain"

instance Function (CopyFunc a [Either a x]) where
  type Dom (CopyFunc a [Either a x]) = a
  type Rng (CopyFunc a [Either a x]) = [Either a x]
  eval CopyArg x = [Left x]
  eval (CopyConst y) _ = y
  isConst CopyArg = Nothing
  isConst (CopyConst y) = Just y
  inDom _ _ = True

---------------------------------------------
-- Construction of action and oracle machines
---------------------------------------------

-- | Converts a Kleenex program with outputs in the input alphabet adjoined with
-- extra effect symbols to a transducer.
constructTransducer :: RProg sigma (Either sigma act) -> RIdent -> Transducer sigma act
constructTransducer rprog initial =
  FST { fstS = allStates
      , fstE = edgesFromList allTransitions
      , fstI = [initial]
      , fstF = S.singleton []
      }
  where
    (allStates, allTransitions) = go (S.singleton [initial]) S.empty []
    go ws states trans
      | S.null ws                         = (states, trans)
      | (q, ws') <- S.deleteFindMin ws, S.member q states
                                          = go ws' states trans
      | ([], ws') <- S.deleteFindMin ws   = go ws' (S.insert [] states) trans
      | (i:is, ws') <- S.deleteFindMin ws =
          let states' = S.insert (i:is) states in
          case getDecl i of
          RConst y -> go (S.insert is ws') states' ((i:is, Right [y], is):trans)
          RRead p False ->
            go (S.insert is ws') states' ((i:is, Left (p, CopyConst []), is):trans)
          RRead p True ->
            go (S.insert is ws') states'((i:is, Left (p, CopyArg), is):trans)
          RSeq js ->
            let q' = js ++ is
            in go (S.insert q' ws') states' ((i:is, Right [], q'):trans)
          RSum js ->
            let qs' = [j:is | j <- js]
                -- Indexed epsilons are implicitly represented by the transition order
                trans' = [(i:is, Right [], q') | q' <- qs']
            in go (S.union (S.fromList qs') ws') states' (trans' ++ trans)
      | otherwise = error "impossible"

    getDecl i =
      fromMaybe (error $ "internal error: identifier without declaration: " ++ show i)
        $ M.lookup i (rprogDecls rprog)

-- | Get the underlying action machine for a transducer. This is obtained by
-- removing input labels on symbol transitions; and adding bit code inputs to
-- epsilon transitions.
action :: forall sigma act digit.
          (Enum sigma, Bounded sigma, Ord sigma, Enum digit, Bounded digit)
       => Transducer sigma act -> ActionMachine sigma act digit
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

-- | Get the underlying oracle for a transducer. This is obtained by removing
-- output labels on symbol transitions; and adding bit code outputs to every
-- non-deterministic transition.
oracle :: forall sigma act digit. (Ord sigma, Enum sigma, Bounded sigma, Enum digit, Bounded digit)
       => Transducer sigma act -> OracleMachine sigma digit
oracle = mapEdges symsym symeps epssym epseps
  where
    symsym _q ts = [ (p, f, q') | (p, CopyArg, q') <- ts
                                , let f = if size p > 1 then CodeArg p else CodeConst [] ]
    symeps _q _ts = []
    epssym _q _ts = []
    epseps _q [(_y, q')] = ([([], q')])
    epseps _q ts = let n = length ts
                   in [ (codeFixedWidthEnumSized n ix, q') | ([], q') <- ts | ix <- [0..] ]
