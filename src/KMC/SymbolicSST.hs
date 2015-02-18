{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module KMC.SymbolicSST where

import           Control.Applicative
import           Control.Monad

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import           KMC.Theories

type Valuation var delta       = M.Map var [delta]
type Environment st var delta  = M.Map st (Valuation var delta)

data Atom var func             = VarA var | ConstA (Rng func) | FuncA func
type UpdateStringFunc var func = [Atom var func]
type UpdateString var rng      = [Either var rng]
type RegisterUpdate var func   = M.Map var (UpdateStringFunc var func)

data EdgeSet st pred func var =
  EdgeSet
  { eForward  :: M.Map st [(pred, RegisterUpdate var func, st)]
  , eBackward :: M.Map st [(pred, RegisterUpdate var func, st)]
  }

data SST st pred func var =
  SST
  { sstS :: S.Set st                               -- ^ State set
  , sstE :: EdgeSet st pred func var               -- ^ Symbolic transition relation
  , sstI :: st                                     -- ^ Initial state
  , sstF :: M.Map st (UpdateString var (Rng func)) -- ^ Final states with final output
  }

-- | Get the designated output variable of an SST.
sstOut :: (Ord var) => SST st pred func var -> var
sstOut = S.findMin . sstV

-- | Output variables. The minimal variable is the designated output variable.
sstV :: (Ord var) => SST st pred func var -> S.Set var
sstV sst = S.unions [ M.keysSet upd | (_,_,upd,_) <- edgesToList $ sstE sst ]

deriving instance (Show var, Show func, Show (Rng func)) => Show (Atom var func)
deriving instance (Show st, Show pred, Show func, Show var, Show (Rng func))
             => Show (EdgeSet st pred func var)
deriving instance (Show st, Show pred, Show func, Show var, Show (Rng func))
             => Show (SST st pred func var)

evalUpdateStringFunc :: (Function func, Rng func ~ [delta]) =>
                        Dom func -> UpdateStringFunc var func -> UpdateString var [delta]
evalUpdateStringFunc x = normalizeUpdateString . map subst
    where
      subst (VarA v)   = Left v
      subst (ConstA y) = Right y
      subst (FuncA f)  = Right $ eval f x

constUpdateStringFunc :: UpdateString var (Rng func) -> UpdateStringFunc var func
constUpdateStringFunc = map subst
    where
      subst (Left v) = VarA v
      subst (Right x) = ConstA x

normalizeUpdateStringFunc :: (Rng func ~ [delta]) => UpdateStringFunc var func -> UpdateStringFunc var func
normalizeUpdateStringFunc = go
    where
      go [] = []
      go (ConstA x:ConstA y:xs) = go (ConstA (x ++ y):xs)
      go (ConstA x:xs) = ConstA x:go xs
      go (VarA v:xs) = VarA v:go xs
      go (FuncA f:xs) = FuncA f:go xs

normalizeRegisterUpdate :: (Rng func ~ [delta]) => RegisterUpdate var func -> RegisterUpdate var func
normalizeRegisterUpdate = M.map normalizeUpdateStringFunc

normalizeUpdateString :: UpdateString var [delta] -> UpdateString var [delta]
normalizeUpdateString = go
    where
      go [] = []
      go (Right x:Right y:xs) = Right (x++y):xs
      go (Left v:xs) = Left v:go xs
      go (Right x:xs) = Right x:go xs

edgesFromList :: (Ord st) => [(st, pred, RegisterUpdate var func, st)] -> EdgeSet st pred func var
edgesFromList xs = EdgeSet { eForward  = M.fromListWith (++) [ (q,  [(p, u, q')]) | (q,p,u,q') <- xs ]
                           , eBackward = M.fromListWith (++) [ (q', [(p, u, q)])  | (q,p,u,q') <- xs ]
                           }

edgesToList :: EdgeSet st pred func var -> [(st, pred, RegisterUpdate var func, st)]
edgesToList es = [ (q,p,u,q') | (q, xs) <- M.toList (eForward es), (p,u,q') <- xs ]

eForwardLookup :: (Ord st) => EdgeSet st pred func var -> st -> [(pred, RegisterUpdate var func, st)]
eForwardLookup es st = maybe [] id (M.lookup st (eForward es))

mapEdges :: (Ord st)
         => ((st, pred, RegisterUpdate var func, st) -> (st, pred, RegisterUpdate var func, st))
         -> EdgeSet st pred func var
         -> EdgeSet st pred func var
mapEdges f = edgesFromList . map f . edgesToList

-- | Construct an SST from an edge set and a list of final outputs.
construct :: (Ord st, Ord var, Rng func ~ [delta]) =>
       st                                                   -- ^ Initial state
    -> [(st, pred, [(var, UpdateStringFunc var func)], st)] -- ^ Edge set
    -> [(st, UpdateString var [delta])]                     -- ^ Final outputs
    -> SST st pred func var
construct qin es os =
  SST
  { sstS = S.fromList (qin:concat [ [q, q'] | (q, _, _, q') <- es ])
  , sstE = edgesFromList [ (q, p, ru us, q') | (q, p, us, q') <- es ]
  , sstI = qin
  , sstF = outf [(q, normalizeUpdateString us) | (q, us) <- os]
  }
  where
    outf = M.fromListWith (error "Inconsistent output function: Same state has more than one update.")
    ru = normalizeRegisterUpdate
         . M.fromListWith (error "Inconsistent register update: Same variable updated more than once.")

construct' :: (Ord st, Ord var, Rng func ~ [delta]) =>
       st                                        -- ^ Initial state
    -> [(st, pred, RegisterUpdate var func, st)] -- ^ Edge set
    -> [(st, [Either var [delta]])]              -- ^ Final outputs
    -> SST st pred func var
construct' qin es os =
  SST
  { sstS = S.fromList (qin:concat [ [q, q'] | (q, _, _, q') <- es ])
  , sstE = edgesFromList [ (q, p, normalizeRegisterUpdate ru, q') | (q, p, ru, q') <- es ]
  , sstI = qin
  , sstF = outf [(q, normalizeUpdateString us) | (q, us) <- os]
  }
  where
    outf = M.fromListWith (error "Inconsistent output function: Same state has more than one update.")

{-- Analysis --}

data AbstractVal rng = Exact rng | Ambiguous deriving (Eq, Ord, Show, Functor)
type AbstractValuation var delta = M.Map var (AbstractVal [delta])
type AbstractEnvironment st var delta = M.Map st (AbstractValuation var delta)

instance Applicative AbstractVal where
  pure = Exact
  (Exact f) <*> (Exact x) = Exact (f x)
  _ <*> _ = Ambiguous

isExact :: AbstractVal a -> Bool
isExact (Exact _) = True
isExact _ = False

-- | Compute the least upper bound of two abstract values.
lubAbstractVal :: (Eq a) => AbstractVal a -> AbstractVal a -> AbstractVal a
lubAbstractVal (Exact x) (Exact y) = if x == y then Exact x else Ambiguous
lubAbstractVal _ _ = Ambiguous

-- | Compute the least upper bound of a set of abstract valuations.
lubAbstractValuations :: (Ord var, Eq delta) =>
                         [AbstractValuation var delta] -> AbstractValuation var delta
lubAbstractValuations = M.unionsWith lubAbstractVal


liftAbstractValuation :: (Ord var, Function func, Rng func ~ [delta]) =>
                         AbstractValuation var delta
                      -> UpdateStringFunc var func
                      -> Maybe (AbstractVal [delta])
liftAbstractValuation rho = go
  where
    go [] = return (pure [])
    go (VarA v:xs) = liftA2 (++) <$> M.lookup v rho <*> go xs
    go (FuncA f:xs) = case isConst f of
                        Nothing -> Just Ambiguous
                        Just ys  -> liftA (ys++) <$> go xs
    go (ConstA ys:xs) = liftA (ys++) <$> go xs

updateAbstractValuation :: (Ord var, Function func, Rng func ~ [delta]) =>
                           AbstractValuation var delta
                        -> RegisterUpdate var func
                        -> AbstractValuation var delta
updateAbstractValuation rho kappa = M.union (M.mapMaybe (liftAbstractValuation rho) kappa) rho

-- | Weaker version of the above. Variables will be considered ambiguous if they
-- depend on their own value.
updateAbstractValuationWeak :: (Ord var, Function func, Rng func ~ [delta]) =>
                           AbstractValuation var delta
                        -> RegisterUpdate var func
                        -> AbstractValuation var delta
updateAbstractValuationWeak rho kappa =
  let rho' = M.mapMaybeWithKey
               (\k usf ->
                 liftAbstractValuation (M.insert k Ambiguous rho) usf)
               kappa
  in M.union rho' rho

applyAbstractValuation :: (Ord var, {- necessary? -} Function func, Rng func ~ [delta]) => 
                          AbstractValuation var delta
                       -> UpdateStringFunc var func
                       -> UpdateStringFunc var func
applyAbstractValuation rho = normalizeUpdateStringFunc . map subst
    where
      subst (VarA v) | Just (Exact ys) <- M.lookup v rho = ConstA ys
                     | otherwise = VarA v
      subst a = a

applyAbstractValuationUS :: (Ord var) =>
                            AbstractValuation var delta
                         -> UpdateString var [delta]
                         -> UpdateString var [delta]
applyAbstractValuationUS rho = normalizeUpdateString . map subst
    where
      subst (Left v) | Just (Exact ys) <- M.lookup v rho = Right ys
                     | otherwise = Left v
      subst a = a

updateAbstractEnvironment :: (Ord st, Ord var, Eq delta
                             ,Function func, Rng func ~ [delta]) =>
                             Bool
                          -> SST st pred func var
                          -> S.Set st
                          -> S.Set st
                          -> AbstractEnvironment st var delta
                          -> (AbstractEnvironment st var delta, S.Set st)
updateAbstractEnvironment weak sst oldStates states gamma =
  (M.union updates gamma
  ,S.unions (map succs (M.keys updates)))
  where
    -- Compute the set of successors of a given state
    succs q =
      S.fromList [ q' | Just xs <- [M.lookup q (eForward $ sstE sst)], (_,_,q') <- xs ]

    updates = M.unions $ do
      s <- S.toList states
      -- Get the previous abstract valuation for the current state
      let rho_s = maybe M.empty id (M.lookup s gamma)
      -- Compute the abstract valuation for the current state by applying the
      -- update function to the abstract valuations of all predecessors and
      -- taking the least upper bound.
      let rho_s' =
            lubAbstractValuations $
              rho_s:
              [ maybe M.empty id (M.lookup r gamma) `updateRho` kappa
                | (_, kappa, r) <- maybe [] id (M.lookup s (eBackward $ sstE sst))
                  -- Only consider abstract valuations from the states that were
                  -- updated in last iteration
                , S.member r oldStates ]
      -- Did we learn more information?
      guard (rho_s' /= rho_s)
      return (M.singleton s rho_s')

    updateRho = if weak then updateAbstractValuationWeak else updateAbstractValuation

abstractInterpretation :: (Ord st, Ord var, Eq delta
                          ,Function func, Rng func ~ [delta]) =>
                          Bool
                       -> SST st pred func var
                       -> AbstractEnvironment st var delta
abstractInterpretation weak sst = go (sstS sst)
                                     (sstS sst)
                                     (M.fromList [(st, M.empty) | st <- S.toList (sstS sst)])
    where
      go _         states gamma | S.null states = gamma
      go oldStates states gamma = let (gamma', states') = updateAbstractEnvironment weak sst oldStates states gamma
                                  in go states states' gamma'

applyAbstractEnvironment :: (Ord st, Ord var, Function func, Rng func ~ [delta]) =>
                            AbstractEnvironment st var delta
                         -> SST st pred func var
                         -> SST st pred func var
applyAbstractEnvironment gamma sst =
  sst { sstE = mapEdges apply (sstE sst)
      , sstF = M.mapWithKey applyFinal (sstF sst)
      }
  where
    apply (q, p, kappa, q') =
      let -- The static environment when exiting the source state
          srcRho = maybe M.empty id (M.lookup q gamma)
          -- Get the list of variables that are statically known in the destination state
          exactKeys = M.keys $ M.filter isExact $ maybe M.empty id (M.lookup q' gamma)
          -- Apply the static environment of the source state and delete all static updates
          kappa' = M.map (applyAbstractValuation srcRho) $ foldr M.delete kappa exactKeys
      in (q, p, kappa', q')

    applyFinal q us =
      let rho = maybe M.empty id (M.lookup q gamma)
      in normalizeUpdateString $ applyAbstractValuationUS rho us

optimize :: (Eq delta, Ord st, Ord var, Function func, Rng func ~ [delta]) =>
            Int
         -> SST st pred func var
         -> SST st pred func var
optimize level sst =
  let weak = level < 3
      applyOpt = level > 0
      gamma = abstractInterpretation weak sst
  in if applyOpt then applyAbstractEnvironment gamma sst else sst

enumerateStates :: (Ord k, Ord var) => SST k pred func var -> SST Int pred func var
enumerateStates sst =
    SST
    { sstS = S.fromList $ M.elems states
    , sstE = edgesFromList [ (aux q, p, f, aux q') | (q, p, f, q') <- edgesToList (sstE sst) ]
    , sstI = aux . sstI $ sst
    , sstF = M.fromList [ (aux q, o) | (q, o) <- M.toList (sstF sst) ]
    }
    where
      states = M.fromList (zip (S.toList (sstS sst)) [(0::Int)..])
      aux q = states M.! q

enumerateVariables :: forall var st pred func. (Ord var, Ord st) => SST st pred func var -> SST st pred func Int
enumerateVariables sst =
  SST
  { sstS = sstS sst
  , sstE = edgesFromList [ (st, p, transMap f, st') | (st, p, f, st') <- edgesToList (sstE sst) ]
  , sstI = sstI sst
  , sstF = M.map usReplace $ sstF sst
  }
  where
    vids          = M.fromList $ zip (S.toList (sstV sst)) [0..]
    transMap m    = M.fromList [ (replace x, usfReplace v) | (x, v) <- M.toList m ]
    replace v     = vids M.! v
    usfReplace as = do
      at <- as
      case at of
        VarA v   -> [VarA (replace v)]
        ConstA c -> [ConstA c]
        FuncA f  -> [FuncA f]
    usReplace as  = do
      at <- as
      case at of
        Left v  -> [Left (replace v)]
        Right c -> [Right c]

{-- Simulation --}

data Stream a = Chunk a (Stream a) | Done | Fail String
  deriving (Show)

valuate :: (Ord var) => Valuation var delta -> UpdateString var [delta] -> [delta]
valuate _ [] = []
valuate s (Right d:xs) = d ++ valuate s xs
valuate s (Left v:xs) = maybe (error "valuate: Variable not in valuation") id (M.lookup v s)
                        ++ valuate s xs

run :: (Ord t, Ord st, SetLike pred (Dom func),
        Function func, Rng func ~ [delta])
    => SST st pred func t
    -> [Dom func]
    -> Stream [delta]
run sst = go (sstI sst) (M.fromList [ (x, []) | x <- S.toList (sstV sst) ])
    where
      outVar = S.findMin (sstV sst)

      extractOutput s =
        case M.lookup outVar s of
          Nothing -> error "Output variable not in valuation"
          Just x -> (x, M.insert outVar [] s)

      go q s [] =
        case M.lookup q (sstF sst) of
          Nothing -> Fail "End of input reached, but final state is not accepting."
          Just out -> Chunk (valuate s out) Done
      go q s (a:as) = maybe (Fail "No match") id $ do
        ts <- M.lookup q (eForward $ sstE sst)
        (upd, q') <- findTrans a ts
        let (out, s') = extractOutput $ M.map (valuate s . evalUpdateStringFunc a) upd
        return $ Chunk out (go q' s' as)

      findTrans _ [] = Nothing
      findTrans a ((p, upd, q'):ts) =
        if member a p then
            Just (upd, q')
        else
            findTrans a ts
