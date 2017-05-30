{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KMC.SSTCompiler
       (compile
       ,module KMC.SSTCompiler.Classes)
       where

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Set as S

import           KMC.Program.IL
import           KMC.SymbolicSST
import           KMC.Theories
import           KMC.Util.Coding (bitWidth, boundedSize)
import           KMC.Util.Map (swapMap)

import           KMC.SSTCompiler.Classes
import           KMC.SSTCompiler.Env

import           Prelude

------------------
-- Lookahead phase
------------------

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


--------------
-- Compilation
--------------

-- | Compile a single variable update into a sequence of instructions.
-- The following is assumed:
--   (1) Variable updates are linear (non-copying)
--   (2) Within the block of updates, the updated buffer is not live.
--       (I.e., no later update depends on it, so it is safe to modify it)
compileAssignment :: (Ord st, Ord var, Ord tid, Rng func ~ [delta], Enum delta, CompilableFunction func tid)
                  => var                       -- ^ Variable to be updated
                  -> UpdateStringFunc var func -- ^ Update function
                  -> EnvReader st var tid Block
compileAssignment var atoms = do
  case atoms of
    VarA var':atoms'
        | var == var' -> concat <$> mapM conv atoms'
    _                 -> (:) <$> (ResetI <$> bid) <*> (concat <$> mapM conv atoms)
  where
    bid = (M.!) <$> (asks bmap) <*> pure var
    conv (VarA var')     = (:[]) <$> (ConcatI    <$> bid <*> ((M.!) <$> asks bmap <*> pure var'))
    conv (ConstA deltas) = (:[]) <$> (AppendI    <$> bid <*> ((M.!) <$> asks cmap <*> pure (map fromEnum deltas)))
    conv (FuncA f)       = do { bid' <- bid; compileFuncAppend bid' f }

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

compileRegisterUpdate :: (Ord st, Ord var, Ord tid, Rng func ~ [delta], Enum delta
                         ,CompilableFunction func tid)
                      => RegisterUpdate var func   -- ^ Register update
                      -> EnvReader st var tid Block
compileRegisterUpdate rup =
  liftM concat $ mapM (uncurry compileAssignment) $ orderAssignments rup

compileTransitions :: (Ord st, Ord var, Ord pred, Ord tid, Enum delta, Ord delta
                      ,Rng func ~ [delta], PredicateListToExpr pred, CompilableFunction func tid) =>
                      Int
                   -> KVTree pred (RegisterUpdate var func, st) -- ^ Transitions
                   -> EnvReader st var tid Block
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

compileState :: forall st var tid delta pred func.
                (Ord st, Ord var, Ord pred, Ord tid, Ord delta, Enum delta
                ,Rng func ~ [delta], PredicateListToExpr pred, CompilableFunction func tid) =>
                [([pred], RegisterUpdate var func, st)] -- ^ Transitions
             -> Maybe (UpdateString var [delta])        -- ^ Final action
             -> EnvReader st var tid Block
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
                           -- Type signature needed to resolve types
                           ass <- compileAssignment var
                                    (constUpdateStringFunc upd :: UpdateStringFunc var func)
                           return $ ass ++ [AcceptI]
  transitions <- compileTransitions 0 (kvtree [ (ps, (upd, st')) | (ps, upd, st') <- trans ])
  return $ assignments ++ transitions ++ [FailI]

compile :: forall st var func tid pred sigma delta.
    ( Bounded delta, Enum delta, Bounded sigma, Enum sigma
    , Ord st, Ord var, Ord tid, Ord pred, Ord delta
    , Function func, Dom func ~ [sigma], Rng func ~ [delta]
    , PredicateListToExpr pred, CompilableFunction func tid) =>
    SST st pred func var
    -> Program
compile sst =
  Program
  { progInBits       = bitWidth 2 (boundedSize (undefined :: sigma) :: Integer)
  , progOutBits      = bitWidth 2 (boundedSize (undefined :: delta) :: Integer)
  , progTables       = M.fromList [ (tid, tbl) | (_,tid,tbl) <- funcRel ]
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
    env = Env { bmap = M.fromList $ zip (S.elems $ sstV sst) (map BufferId [0..])
              , tmap = M.fromList [ (func, tid) | (func, tid, _) <- funcRel ]
              , smap = M.fromList $ zip (S.elems $ sstS sst) (map BlockId [0..])
              , cmap = M.fromList $ zip (S.elems consts) (map ConstId [0..])
              , outvar = sstOut sst
              }
    -- List of all functions occurring in the SST, obtained by visiting every
    -- update string function in every transition.
    allTables = M.unions
        [ tables f | (_, upd, _) <- concat $ M.elems $ sstE sst
                   , us <- M.elems upd
                   , f <- usFunctions us ]

    funcRel :: [(tid, TableId, Table)]
    funcRel = [ (tid, syntid, tbl) | (tid, tbl) <- M.toList allTables
                                   | syntid <- map TableId [0..] ]

    consts = constants (edgesToList $ sstE sst) (M.elems $ sstF sst)


--------------------
-- Utility functions
--------------------

-- | Set of all constants occurring in a list of update strings
constants :: (Enum delta, Rng func ~ [delta], CompilableFunction func tid) =>
             [(st, pred, RegisterUpdate var func, st)]
          -> [UpdateString var [delta]]
          -> S.Set [Int]
constants trans fins = S.unions [tconsts, fconsts]
    where tconsts = S.fromList $ do
                      (_, _, ru, _) <- trans
                      usf <- M.elems ru
                      (   [ map fromEnum c | ConstA c <- usf ]
                       ++ [ c              | FuncA f <- usf, c <- funcConstants f ])
          fconsts = S.fromList $ do
                      us <- fins
                      Right c <- us
                      return (map fromEnum c)

-- | List of function occurrences in an UpdateStringFunc
usFunctions :: UpdateStringFunc var func -> [func]
usFunctions [] = []
usFunctions (FuncA f:xs) = f:usFunctions xs
usFunctions (_:xs) = usFunctions xs
