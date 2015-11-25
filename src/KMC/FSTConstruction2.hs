{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KMC.FSTConstruction2 where

import           KMC.Theories
--import           Control.Monad.State
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
--import           Data.Word
--import           KMC.Kleenex.Actions
import           KMC.Kleenex.Syntax hiding (Ident)
import           KMC.Kleenex.Actions
import           KMC.Coding
import           KMC.RangeSet (RangeSet)
import           KMC.SymbolicFST
--import           KMC.Theories

type FSTState = [RIdent]

type ActionMachine a digit
  = FST FSTState (CodeInputLab digit) (DecodeFunc (RangeSet a) digit a)

type OracleMachine a b
  = FST FSTState (RangeSet a) (CodeFunc (RangeSet a) a b)

-----------------------------------------------------------
-- Predicate and function types for symbolic representation
-----------------------------------------------------------

-- | Data type representing input labels for action machines.
data CodeInputLab b = InputConst [b]  -- ^ Read exactly this constant
                    | InputAny Int    -- ^ Read this number of symbols
  deriving (Eq, Ord, Show)

-- | Represents functions which code values as fixed-width codes in an arbitrary
-- base. The functions may also ignore their arguments and return a constant
-- code.
data CodeFunc enum dom digit = CodeArg enum | CodeConst [digit]
  deriving (Show)

-- | Represents functions which decode fixed-width codes into sequences of
-- elements. That is, the code of one element may be followed by the code of
-- another, or the code sequence can be empty. The functions may also ignore
-- their arguments and return a constant result sequence.
data DecodeFunc enum digit rng = DecodeArg [enum] | DecodeConst [Either rng RegAction]
  deriving (Show)

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
  domain (CodeArg p) = [lookupIndex i p | i <- [0 .. size p - 1]]
  domain (CodeConst _) = [minBound .. maxBound]

instance (Enum digit, Bounded digit, Enumerable enum rng) => Function (DecodeFunc enum digit rng) where
  type Dom (DecodeFunc enum digit rng) = [digit]
  type Rng (DecodeFunc enum digit rng) = [Either rng RegAction]
  eval (DecodeArg []) code | null code = []
                           | otherwise = error "non-empty code"
  eval (DecodeArg (enum:enums)) code =
    let w = bitWidth (boundedSize (undefined :: digit)) (size enum)
        (pre,rest) = splitAt w code
    in (Left (lookupIndex (decodeEnum pre) enum)):(eval (DecodeArg enums) rest)
  eval (DecodeConst y) _ = y
  isConst (DecodeArg _) = Nothing
  isConst (DecodeConst y) = Just y
  inDom code (DecodeArg []) = null code
  inDom code (DecodeArg (enum:enums)) =
    let w = bitWidth (boundedSize (undefined :: digit)) (size enum)
        (pre,rest) = splitAt w code
    in (decodeEnum pre < size enum) && inDom rest (DecodeArg enums)
  inDom _ (DecodeConst _) = True
  domain (DecodeArg []) = [[]]
  domain (DecodeArg (enum:enums)) =
    [ codeFixedWidthEnumSized (size enum) i ++ rest | i <- [0 .. size enum - 1]
                                                    , rest <- domain (DecodeArg enums) ]
  domain (DecodeConst _) = error "cannot compute infinite domain"


---------------------------------------------
-- Construction of action and oracle machines
---------------------------------------------

constructAction :: forall a digit. (Enum a, Bounded a, Ord a, Enum digit, Bounded digit)
                => RProg a (Either a RegAction)
                -> RIdent
                -> ActionMachine a digit
constructAction rprog initial =
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
            RRead _ False ->
              go (S.insert is ws') states' ((i:is, Right [], is):trans)
            RRead p True ->
              let w = bitWidth (boundedSize (undefined :: digit)) (size p) in
              go (S.insert is ws') states'((i:is, Left (InputAny w, DecodeArg [p]), is):trans)
            RSeq js ->
              let q' = js ++ is
              in go (S.insert q' ws') states' ((i:is, Right [], q'):trans)
            RSum js ->
              let qs' = [j:is | j <- js]
                  n = length qs'
                  trans' = [(i:is, Left (InputConst (codeFixedWidthEnumSized n ix), DecodeConst []), q')
                           | q' <- qs' | ix <- [0..]]
              in go (S.union (S.fromList qs') ws') states' (trans' ++ trans)
      | otherwise = error "impossible"

    getDecl i =
      fromMaybe (error "internal error: identifier without declaration")
        $ M.lookup i (rprogDecls rprog)

constructOracle :: (Enum b, Bounded b) => RProg a c -> RIdent -> OracleMachine a b
constructOracle rprog initial =
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
          RConst _ -> go (S.insert is ws') states' ((i:is, Right [], is):trans)
          RRead p False ->
            go (S.insert is ws') states' ((i:is, Left (p, CodeConst []), is):trans)
          RRead p True ->
            go (S.insert is ws') states'((i:is, Left (p, CodeArg p), is):trans)
          RSeq js ->
            let q' = js ++ is
            in go (S.insert q' ws') states' ((i:is, Right [], q'):trans)
          RSum js ->
            let qs' = [j:is | j <- js]
                n = length qs'
                trans' = [(i:is, Right (codeFixedWidthEnumSized n ix), q') | q' <- qs' | ix <- [0..]]
            in go (S.union (S.fromList qs') ws') states' (trans' ++ trans)
      | otherwise = error "impossible"

    getDecl i =
      fromMaybe (error $ "internal error: identifier without declaration: " ++ show i)
        $ M.lookup i (rprogDecls rprog)
