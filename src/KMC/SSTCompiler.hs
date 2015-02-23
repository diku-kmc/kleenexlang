{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KMC.SSTCompiler where

import           Control.Monad.Reader
import           Control.Applicative ((<$>), (<*>), pure)
import           Data.List (sortBy)
import qualified Data.Map as M
import qualified Data.Set as S

import           KMC.OutputTerm
import           KMC.Program.IL
import qualified KMC.RangeSet as RS
import           KMC.SymbolicSST
import           KMC.Theories
import           KMC.Util.Map (swapMap)

class PredicateToExpr p where
    predToExpr :: p -> Int -> Expr

instance (Eq a, Enum a) => PredicateToExpr (RS.RangeSet a) where
  predToExpr rs i = case map rangeTest (RS.ranges rs) of
                      [] -> FalseE
                      xs -> foldr1 OrE xs
      where
        rangeTest (l, h)
          | l == h    = EqE (SymE i) (ConstE $ fromEnum l)
          | otherwise = AndE (LteE (ConstE $ fromEnum l) (SymE i))
                             (LteE (SymE i) (ConstE $ fromEnum h))

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
      norm (FuncA (Join xs) i) = map (normJoin i) xs
      normJoin _ (Inl (Const xs)) = ConstA xs
      normJoin i (Inr e) = FuncA e i

-- | List of function occurrences in an UpdateStringFunc
usFunctions :: UpdateStringFunc var func -> [func]
usFunctions [] = []
usFunctions (FuncA f _:xs) = f:usFunctions xs
usFunctions (_:xs) = usFunctions xs

-- | Tabulate a function. It is assumed that the codomain is a set of
-- bit-vectors with pairwise equal length.
tabulate :: (Function t,Enum (Dom t),Bounded (Dom t)
            ,Rng t ~ [delta])
         => t -> Table delta
tabulate f = Table bitTable bitSize
  where
    bitTable = map eval' [minBound .. maxBound]
    bitSize = foldr max 0 (map length bitTable)
    eval' x | inDom x f = eval f x
            | otherwise = []

-- | Compile a single variable update into a sequence of instructions.
-- The following is assumed:
--   (1) Variable updates are linear (non-copying)
--   (2) Within the block of updates, the updated buffer is not live. (I.e., it is safe to modify it)
compileAssignment :: (Ord var, Ord func, Rng func ~ [delta], Ord delta) =>
                     var                       -- ^ Variable to be updated
                  -> UpdateStringFunc var func -- ^ Update function
                  -> EnvReader st var func delta (Block delta)
compileAssignment var atoms = do
  case atoms of
    VarA var':atoms'
        | var == var' -> mapM conv atoms'
    _                 -> (:) <$> (ResetI <$> bid) <*> (mapM conv atoms)
  where
    bid = (M.!) <$> (asks bmap) <*> pure var
    conv (VarA var')     = ConcatI    <$> bid <*> ((M.!) <$> asks bmap <*> pure var')
    conv (ConstA deltas) = AppendI    <$> bid <*> ((M.!) <$> asks cmap <*> pure deltas)
    conv (FuncA f i)     = AppendTblI <$> bid <*> ((M.!) <$> asks tmap <*> pure f) <*> pure i

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

compileRegisterUpdate :: (Ord var, Ord func, Rng func ~ [delta], Ord delta) =>
                      RegisterUpdate var func   -- ^ Register update
                      -> EnvReader st var func delta (Block delta)
compileRegisterUpdate rup = 
  liftM concat $ mapM (uncurry compileAssignment) $ orderAssignments $ M.toList rup

compileTransitions :: (Ord st, Ord var, Ord pred, Ord func, Ord delta
                      ,Rng func ~ [delta], PredicateToExpr pred) =>
                      Int
                   -> [([pred], RegisterUpdate var func, st)] -- ^ Transitions
                   -> EnvReader st var func delta (Block delta)
compileTransitions i ts = do
  testBlock <- forM tests $ \(p, ts') ->
    do block <- compileTransitions (i+1) ts'
       return [IfI ((AvailableSymbolsE `GteE` (ConstE (i+1)))
                    `AndE` (predToExpr p i))
                   block]
  actionBlock <- case [ (upd, st') | ([], upd, st') <- ts ] of
                   [] -> return []
                   [(upd, st')] -> do
                               bid <- (M.! st') <$> asks smap
                               block <- compileRegisterUpdate upd
                               return $ block ++ [ConsumeI i, GotoI bid]
                   _ -> error "ambiguous transition"
  return $ concat testBlock ++ actionBlock
  where
    tests = M.toList $ M.fromListWith (++) [ (p, [(ps, upd, st')]) | (p:ps, upd, st') <- ts ]

compileState :: (Ord st, Ord var, Ord pred, Ord func, Ord delta
                ,Rng func ~ [delta], PredicateToExpr pred) =>
                [([pred], RegisterUpdate var func, st)] -- ^ Transitions
             -> Maybe (UpdateString var [delta])        -- ^ Final action
             -> EnvReader st var func delta (Block delta)
compileState trans fin = do
  let (minL, maxL) =
        if null trans then
            (1, 1)
        else
            (foldr1 min [ length ps | (ps, _, _) <- trans ]
            ,foldr1 max [ length ps | (ps, _, _) <- trans ])
  -- Look ahead at least minL symbols, up to maxL symbols. If minL symbols are
  -- not available, execute fallback action.
  assignments <- liftM ((:[]) . NextI minL maxL) $ case fin of
                   Nothing  -> return [FailI]
                   Just upd -> do
                           var <- asks outvar
                           ass <- compileAssignment var (constUpdateStringFunc upd)
                           return $ ass ++ [AcceptI]
  transitions <- compileTransitions 0 trans
  return $ assignments ++ transitions ++ [FailI]

constants :: (Ord delta, Rng func ~ [delta]) =>
             [(st, pred, RegisterUpdate var func, st)]
          -> [UpdateString var [delta]]
          -> S.Set [delta]
constants trans fins = S.union tconsts fconsts
    where tconsts = S.fromList $ do
                      (_, _, ru, _) <- trans
                      usf <- M.elems ru
                      ConstA c <- usf
                      return c

          fconsts = S.fromList $ do
                      us <- fins
                      Right c <- us
                      return c

data Env st var func delta = Env
    { bmap   :: M.Map var BufferId      -- ^ Variable to buffer map
    , tmap   :: M.Map func TableId      -- ^ Function to table map
    , cmap   :: M.Map [delta] ConstId   -- ^ Constant to const id map
    , smap   :: M.Map st BlockId        -- ^ State to block id map
    , outvar :: var                     -- ^ Designated output variable
    }
type EnvReader st var func delta = Reader (Env st var func delta)

compileAutomaton :: forall st var func pred delta.
    ( Ord st, Ord var, Ord func, Ord pred, Ord delta
    , Function func, Enum (Dom func), Bounded (Dom func), Rng func ~ [delta]
    , PredicateToExpr pred) =>
    SST st pred func var
    -> Program delta
compileAutomaton sst =
  Program
  { progTables       = M.fromList [ (tid, tbl) | (_,tid,tbl) <- funcRel ]
  , progConstants    = swapMap (cmap env)
  , progStreamBuffer = (bmap env) M.! (outvar env)
  , progBuffers      = M.elems (bmap env)
  , progInitBlock    = (smap env) M.! sstI sst
  , progBlocks       =
      M.fromList [ (blck
                   ,flip runReader env
                    $ compileState (eForwardLookup (sstE sst) st)
                                   (M.lookup st (sstF sst))
                   )
                   | (st, blck) <- M.toList (smap env) ]
  }
  where
    env = Env { bmap = M.fromList $ zip (S.toList $ sstV sst) (map BufferId [0..])
              , tmap = M.fromList [ (func, tid) | (func, tid, _) <- funcRel ]
              , smap = M.fromList $ zip (S.toList $ sstS sst) (map BlockId [0..])
              , cmap = M.fromList $ zip (S.toList consts) (map ConstId [0..])
              , outvar = sstOut sst
              }
    -- List of all functions occurring in the SST, obtained by visiting every
    -- update string function in every transition.
    allFunctions =
      S.toList $ S.unions
        [ S.fromList (usFunctions us) | (_, upd, _) <- concat $ M.elems $ sstE sst
                                      , us <- M.elems upd ]

    funcRel :: [(func, TableId, Table delta)]
    funcRel = [ (t, tid, tabulate t) | t <- allFunctions
                                     | tid <- map TableId [0..] ]

    consts = constants (edgesToList $ sstE sst) (M.elems $ sstF sst)

