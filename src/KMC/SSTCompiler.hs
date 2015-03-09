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
import qualified Data.Map as M
import qualified Data.Set as S

import           KMC.OutputTerm
import           KMC.Program.IL
import qualified KMC.RangeSet as RS
import           KMC.SymbolicSST
import           KMC.Theories
import           KMC.Util.Map (swapMap)

class PredicateListToExpr p where
    predListToExpr :: [p] -> Int -> Expr

instance (Eq a, Enum a) => PredicateListToExpr (RS.RangeSet a) where
    predListToExpr [] _ = TrueE
    predListToExpr xs i =
      let (xsEq, xsRest') = span (\rs -> RS.size rs == 1) xs
          (xsComplex, xsRest) = span (\rs -> RS.size rs > 1) xsRest'
          eqExprs =
            case xsEq of
             []    -> []
             [sgl] -> [predToExpr sgl i]
             _     -> [CompareE i $ map (fromEnum . fromSingleton) xsEq]
          complexExprs = zipWith predToExpr xsComplex [i + length xsEq..]
          recExpr      = predListToExpr xsRest (i + length xsEq + length xsComplex)
          allExprs     = eqExprs
                         ++ complexExprs
                         ++ [recExpr]
      in foldr1 AndE allExprs
         where
          fromSingleton rs | [(l,h)] <- RS.ranges rs, l == h = l
                           | otherwise = error "not a singleton rangeset"
          
          predToExpr rs j = case map rangeTest (RS.ranges rs) of
                              []     -> FalseE
                              ranges -> foldr1 OrE ranges
              where
                rangeTest (l, h)
                  | l == h    = EqE (SymE j) (ConstE $ fromEnum l)
                  | otherwise = AndE (LteE (ConstE $ fromEnum l) (SymE j))
                                     (LteE (SymE j) (ConstE $ fromEnum h))

-- | Tree of tests and actions.
data KVTree a b =
  BranchT (Maybe b) [([a], KVTree a b)]
  deriving (Eq, Ord, Show)

-- | Converts a list of test/action pairs to a KVTree such that tests with
-- common prefixes share roots.
kvtree :: (Ord a) => [([a], b)] -> KVTree a b
kvtree xs = BranchT b [ (k, kvtree ts') | (k, ts') <- M.elems ts ]
    where
      b = case [ b' | ([], b') <- xs ] of
            []   -> Nothing
            [b'] -> Just b'
            _    -> error "Ambiguous transition map"
      ts = M.map lcp $ M.fromListWith (++) [ (a, [(a:as, b')]) | (a:as, b') <- xs ]

lcp :: (Eq a) => [([a], b)] -> ([a], [([a], b)])
lcp xs | null xs || any (null . fst) xs = ([], xs)
       | otherwise =
           let h = head (fst (head xs))
               (p, xs') = lcp [ (as, b) | (_:as, b) <- xs ]
           in
           if all (h ==) [ a | (a:_, _) <- xs ] then
               (h:p, xs')
           else
               ([], xs)

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
orderAssignments :: (Ord var) => RegisterUpdate var func -> [(var, UpdateStringFunc var func)]
orderAssignments ru
  | M.null ru = []
  | otherwise = snd $ foldl (visit S.empty) (S.empty, []) (M.keys ru)
    where
      visit temp (mark, acc) v
          | S.member v temp = error "Not a DAG"
          | S.member v mark || not (M.member v ru) = (S.insert v mark, acc)
          | otherwise =
              let (mark', acc') =
                    foldl (visit (S.insert v temp))
                          (mark, acc)
                          (maybe [] id (M.lookup v dag))
              in (S.insert v mark', (v, ru M.! v):acc')

      dag = M.mapWithKey (\k us -> filter (/=k) $ fv us) ru

      fv [] = []
      fv (VarA v:xs) = v:fv xs
      fv (_:xs) = fv xs

compileRegisterUpdate :: (Ord var, Ord func, Rng func ~ [delta], Ord delta) =>
                      RegisterUpdate var func   -- ^ Register update
                      -> EnvReader st var func delta (Block delta)
compileRegisterUpdate rup = 
  liftM concat $ mapM (uncurry compileAssignment) $ orderAssignments rup

compileTransitions :: (Ord st, Ord var, Ord pred, Ord func, Ord delta
                      ,Rng func ~ [delta], PredicateListToExpr pred) =>
                      Int
                   -> KVTree pred (RegisterUpdate var func, st) -- ^ Transitions
                   -> EnvReader st var func delta (Block delta)
compileTransitions i (BranchT action tests) = do
  testBlock <- forM tests $ \(ps, ts') ->
    do block <- compileTransitions (i+length ps) ts'
       return [IfI ((AvailableSymbolsE `GteE` (ConstE (i+length ps)))
                    `AndE` (predListToExpr ps i))
                   block]
  actionBlock <- case action of
                   Nothing -> return []
                   Just (upd, st') -> do
                               bid <- (M.! st') <$> asks smap
                               block <- compileRegisterUpdate upd
                               return $ block ++ [ConsumeI i, GotoI bid]
  return $ concat testBlock ++ actionBlock

compileState :: (Ord st, Ord var, Ord pred, Ord func, Ord delta
                ,Rng func ~ [delta], PredicateListToExpr pred) =>
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
  transitions <- compileTransitions 0 (kvtree [ (ps, (upd, st')) | (ps, upd, st') <- trans ])
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
    , PredicateListToExpr pred) =>
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

