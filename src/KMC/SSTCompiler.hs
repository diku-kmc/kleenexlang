{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KMC.SSTCompiler where

import           Data.List (sortBy)
import qualified Data.Map as M
import qualified Data.Set as S

import           KMC.OutputTerm
import           KMC.Program.IL
import qualified KMC.RangeSet as RS
import           KMC.SymbolicSST
import           KMC.Theories

class PredicateToExpr p where
    predToExpr :: p -> Expr

instance (Enum a) => PredicateToExpr (RS.RangeSet a) where
  predToExpr rs = case map rangeTest (RS.ranges rs) of
                    [] -> FalseE
                    xs -> foldr1 OrE xs
      where
        rangeTest (l, h) = AndE (LtE (ConstE $ fromEnum l) SymE)
                                (LtE SymE (ConstE $ fromEnum h))

-- | The output functions of FSTs generated from Mu-expressions are a strict
-- subset of the update string functions of SSTs. We can therefore "join" any
-- UpdateStringFunc over an FST function over some generic function to an
-- UpdateStringFunc over just the generic function.
flattenFSTFunc :: Rng func ~ [delta] =>
                  UpdateStringFunc var (Join (Const t [delta] :+: func) [delta])
                  -> UpdateStringFunc var func
flattenFSTFunc = normalizeUpdateStringFunc . concatMap norm
    where
      norm (VarA v) = [VarA v]
      norm (ConstA c) = [ConstA c]
      norm (FuncA (Join xs)) = map normJoin xs
      normJoin (Inl (Const xs)) = ConstA xs
      normJoin (Inr e) = FuncA e

-- | List of function occurrences in an UpdateStringFunc
usFunctions :: UpdateStringFunc var func -> [func]
usFunctions [] = []
usFunctions (FuncA f:xs) = f:usFunctions xs
usFunctions (_:xs) = usFunctions xs

-- | Tabulate a function. It is assumed that the codomain is a set of
-- bit-vectors with pairwise equal length.
tabulate :: (Function t,Enum (Dom t),Bounded (Dom t)
            ,Rng t ~ [delta])
         => t -> Table delta
tabulate f = Table bitTable bitSize
  where
    bitTable = map (eval f) [minBound .. maxBound]
    bitSize = case bitTable of
                [] -> 0
                (x:_) -> length x

-- | Compile a single variable update into a sequence of instructions.
-- The following is assumed:
--   (1) Variable updates are linear (non-copying)
--   (2) Within the block of updates, the updated buffer is not live. (I.e., it is safe to modify it)
compileAssignment :: (Ord var, Ord func, Rng func ~ [delta]) =>
                     M.Map var BufferId        -- ^ Variable to buffer map
                  -> M.Map func TableId        -- ^ Function to table map
                  -> var                       -- ^ Variable to be updated
                  -> UpdateStringFunc var func -- ^ Update function
                  -> Block delta
compileAssignment bmap tmap var atoms =
  case atoms of
    VarA var':atoms' | var == var' -> go atoms'
    _ -> ResetI bid
    -- : AlignI bid parentbid
       : go atoms
  where
    bid = bmap M.! var
    go [] = []
    go (VarA var':atoms') = ConcatI bid (bmap M.! var')
                            : go atoms'
    go (ConstA bits:atoms') = AppendI bid bits
                              : go atoms'
    go (FuncA f:atoms') = AppendTblI bid (tmap M.! f)
                          : go atoms'

-- | Order assignments in a register update based on data dependencies. An
-- assignment `a' should come before an assignment `b' if `a' uses the variable
-- that `b' assigns to.
orderAssignments :: (Eq var) => [(var, UpdateStringFunc var func)] -> [(var, UpdateStringFunc var func)]
orderAssignments = sortBy dataDependent
    where
      dataDependent (v, as) (v', as')
        | v' `elem` fv as = LT
        | v `elem` fv as' = GT
        | otherwise = EQ

      fv [] = []
      fv (VarA v:xs) = v:fv xs
      fv (_:xs) = fv xs

compileRegisterUpdate :: (Ord var, Ord func, Rng func ~ [delta]) =>
                         M.Map var BufferId        -- ^ Variable to buffer map
                      -> M.Map func TableId        -- ^ Function to table map
                      -> RegisterUpdate var func   -- ^ Register update
                      -> Block delta
compileRegisterUpdate bmap tmap =
  concatMap (uncurry $ compileAssignment bmap tmap) . orderAssignments . M.toList

compileTransition :: (Ord var, Ord func, Rng func ~ [delta], PredicateToExpr pred) =>
                     M.Map var BufferId        -- ^ Variable to buffer map
                  -> M.Map func TableId        -- ^ Function to table map
                  -> pred                      -- ^ Transition predicate
                  -> RegisterUpdate var func   -- ^ Transition action
                  -> BlockId                   -- ^ Next block id
                  -> Block delta
compileTransition bmap tmap pred' update blockId =
  [IfI (predToExpr pred') (compileRegisterUpdate bmap tmap update ++ [GotoI blockId])]

compileState :: (Ord st, Ord var, Ord func, Rng func ~ [delta], PredicateToExpr pred) =>
                     M.Map var BufferId        -- ^ Variable to buffer map
                  -> M.Map func TableId        -- ^ Function to table map
                  -> M.Map st BlockId          -- ^ State to block id map
                  -> var                       -- ^ Designated output variable
                  -> [(pred, RegisterUpdate var func, st)] -- ^ Transitions
                  -> Maybe (UpdateString var [delta]) -- ^ Final action
                  -> Block delta
compileState bmap tmap smap outvar trans fin =
  [NextI $
     case fin of
       Nothing -> [FailI]
       Just upd -> compileAssignment bmap tmap outvar (constUpdateStringFunc upd)
                   ++ [AcceptI]
  ]
  ++ concat [ compileTransition bmap tmap p upd (smap M.! st) | (p, upd, st) <- trans ]
  ++ [FailI]

compileAutomaton :: forall st var func pred delta.
    (Ord st, Ord var, Ord func
    ,Function func, Enum (Dom func), Bounded (Dom func), Rng func ~ [delta]
    ,PredicateToExpr pred) =>
    SST st pred func var
    -> Program delta
compileAutomaton sst =
  Program
  { progTables       = M.fromList [ (tid, tbl) | (_,tid,tbl) <- funcRel ]
  , progStreamBuffer = bmap M.! sstOut sst
  , progBuffers      = M.elems bmap
  , progInitBlock    = smap M.! sstI sst
  , progBlocks       =
      M.fromList [ (blck
                   ,compileState bmap tmap smap (sstOut sst)
                                 (eForward (sstE sst) M.! st)
                                 (M.lookup st (sstF sst)))
                       | (st, blck) <- M.toList smap ]
  }
  where
    -- List of all functions occurring in the SST, obtained by visiting every
    -- update string function in every transition.
    allFunctions =
      S.toList $ S.unions
        [ S.fromList (usFunctions us) | (_, upd, _) <- concat $ M.elems $ eForward $ sstE sst
                                      , us <- M.elems upd ]

    funcRel :: [(func, TableId, Table delta)]
    funcRel = [ (t, tid, tabulate t) | t <- allFunctions
                                     | tid <- map TableId [0..] ]

    tmap = M.fromList [ (func, tid) | (func, tid, _) <- funcRel ]
    bmap = M.fromList $ zip (S.toList $ sstV sst) (map BufferId [0..])
    smap = M.fromList $ zip (S.toList $ sstS sst) (map BlockId [0..])
