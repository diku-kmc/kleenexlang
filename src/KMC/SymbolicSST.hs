{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module KMC.SymbolicSST
       (UpdateString
       ,UpdateStringFunc
       ,Atom(..)
       ,EdgeSet
       ,SST(..)
       ,RegisterUpdate
       ,composeRegisterUpdate
       ,constUpdateStringFunc
       ,normalizeUpdateStringFunc
       ,evalUpdateStringFunc
       ,construct,construct'
       ,eForwardLookup
       ,sstOut
       ,edgesToList
       ,sstV
       ,enumerateStates
       ,enumerateVariables
       ,optimize
       ,Stream(..)
       ,run
       ,flattenStream
       ,flattenStream'
       ,composeStream
       )
where
import           Control.Applicative
import           Data.Monoid
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Data.List (maximumBy)
import           Data.Ord (comparing)

import           KMC.Theories

-- Silence unused import warnings on migration to base>=4.8
import           Prelude

type Valuation var delta       = M.Map var [delta]

data Atom var func             = VarA var           -- ^ SST register variable
                               | ConstA (Rng func)  -- ^ Constant
                               | FuncA func         -- ^ Function call: func(input[next...next + n])
type UpdateStringFunc var func = [Atom var func]
type UpdateString var rng      = [Either var rng]
type RegisterUpdate var func   = M.Map var (UpdateStringFunc var func)

type EdgeSet st pred func var = M.Map st [([pred], RegisterUpdate var func, st)]

edgesFromList :: (Ord st) => [(st, [pred], RegisterUpdate var func, st)] -> EdgeSet st pred func var
edgesFromList xs = M.fromListWith (++) [ (q,  [(ps, u, q')]) | (q,ps,u,q') <- xs ]

edgesToList :: EdgeSet st pred func var -> [(st, [pred], RegisterUpdate var func, st)]
edgesToList es = [ (q,ps,u,q') | (q, xs) <- M.toList es, (ps,u,q') <- xs ]

eForwardLookup :: (Ord st) => EdgeSet st pred func var -> st -> [([pred], RegisterUpdate var func, st)]
eForwardLookup es st = maybe [] id (M.lookup st es)

mapEdges :: (Ord st)
         => ((st, [pred], RegisterUpdate var func, st) -> (st, [pred], RegisterUpdate var func, st))
         -> EdgeSet st pred func var
         -> EdgeSet st pred func var
mapEdges f = edgesFromList . map f . edgesToList

data SST st pred func var =
  SST
  { sstS :: S.Set st                               -- ^ State set
  , sstE :: EdgeSet st pred func var               -- ^ Symbolic transition relation
  , sstI :: st                                     -- ^ Initial state
  , sstF :: M.Map st (UpdateString var (Rng func)) -- ^ Final states with final output
  }

-- | Output variables. The minimal variable is the designated output variable.
sstV :: (Ord var) => SST st pred func var -> S.Set var
sstV sst = S.unions $ [ M.keysSet upd | (_,_,upd,_) <- edgesToList $ sstE sst ]
                   ++ [ S.singleton var | (_,atoms) <- M.toList $ sstF sst, (Left var) <- atoms]

-- | Get the designated output variable of an SST.
sstOut :: (Ord var) => SST st pred func var -> var
sstOut = S.findMin . sstV

deriving instance (Show var, Show func, Show (Rng func)) => Show (Atom var func)
deriving instance (Eq var, Eq func, Eq (Rng func)) => Eq (Atom var func)
deriving instance (Ord var, Ord func, Ord (Rng func)) => Ord (Atom var func)
deriving instance (Show st, Show pred, Show func, Show var, Show (Rng func), Show (Dom func))
             => Show (SST st pred func var)

evalUpdateStringFunc :: (Function func, Dom func ~ [sigma], Rng func ~ [delta]) =>
                        [sigma] -> UpdateStringFunc var func -> UpdateString var [delta]
evalUpdateStringFunc xs = normalizeUpdateString . map subst
    where
      subst (VarA v)   = Left v
      subst (ConstA y) = Right y
      subst (FuncA f)  = Right $ eval f xs

constUpdateStringFunc :: UpdateString var (Rng func) -> UpdateStringFunc var func
constUpdateStringFunc = map subst
    where
      subst (Left v) = VarA v
      subst (Right x) = ConstA x

normalizeUpdateStringFunc :: (Rng func ~ [delta]) => UpdateStringFunc var func -> UpdateStringFunc var func
normalizeUpdateStringFunc = go
    where
      go [] = []
      go (ConstA []:xs) = go xs
      go (ConstA x:ConstA y:xs) = go (ConstA (x ++ y):xs)
      go (ConstA x:xs) = ConstA x:go xs
      go (VarA v:xs) = VarA v:go xs
      go (FuncA f:xs) = FuncA f:go xs

normalizeRegisterUpdate :: (Rng func ~ [delta]) => RegisterUpdate var func -> RegisterUpdate var func
normalizeRegisterUpdate = M.map normalizeUpdateStringFunc

composeRegisterUpdate :: (Ord var, Rng func ~ [delta])
                      => RegisterUpdate var func -> RegisterUpdate var func -> RegisterUpdate var func
composeRegisterUpdate kappa =
  normalizeRegisterUpdate
  . M.map subst
  -- ensure that domain of left update is contained in the domain of the right
  -- update. Every key that is in the left but not in the right is treated as an
  -- identity update.
  . ensure (M.keys kappa)
  where
    subst [] = []
    subst (VarA v:us) = fromMaybe [VarA v] (M.lookup v kappa) ++ subst us
    subst (x:us) = x:subst us

    ensure vs kappa' = M.union kappa' (M.fromList [(v, [VarA v]) | v <- vs])

normalizeUpdateString :: UpdateString var [delta] -> UpdateString var [delta]
normalizeUpdateString = go
    where
      go [] = []
      go (Right []:xs) = go xs
      go (Right x:Right y:xs) = Right (x++y):xs
      go (Left v:xs) = Left v:go xs
      go (Right x:xs) = Right x:go xs

-- | Construct an SST from an edge set and a list of final outputs.
construct :: (Ord st, Ord var, Rng func ~ [delta]) =>
       st                                                     -- ^ Initial state
    -> [(st, [pred], [(var, UpdateStringFunc var func)], st)] -- ^ Edge set
    -> [(st, UpdateString var [delta])]                       -- ^ Final outputs
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
       st                                          -- ^ Initial state
    -> [(st, [pred], RegisterUpdate var func, st)] -- ^ Edge set
    -> [(st, [Either var [delta]])]                -- ^ Final outputs
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

lteAbstractVal :: (Eq a) => AbstractVal a -> AbstractVal a -> Bool
lteAbstractVal _ Ambiguous = True -- Ambiguous is the top element
lteAbstractVal v1 v2 = v1 == v2 -- Everything else is discretely ordered

lubAbstractValuation :: (Ord var, Eq delta) =>
                        AbstractValuation var delta
                     -> AbstractValuation var delta
                     -> AbstractValuation var delta
lubAbstractValuation = M.unionWith lubAbstractVal

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

applyAbstractValuation :: (Ord var, Rng func ~ [delta]) =>
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
                          -> AbstractEnvironment st var delta
                          -> (AbstractEnvironment st var delta, S.Set st)
updateAbstractEnvironment weak sst states gamma =
  (M.union updates gamma
  ,M.keysSet updates)
  where
    updateOldRho s rho' =
      let rho_s = maybe M.empty id (M.lookup s gamma)
      in if M.isSubmapOfBy lteAbstractVal rho' rho_s then
           Nothing
         else
           Just (lubAbstractValuation rho_s rho')

    updates = M.mapMaybeWithKey updateOldRho $ M.fromListWith lubAbstractValuation $ do
      r <- S.toList states
      let rho_r = maybe M.empty id (M.lookup r gamma)
      (_, kappa, s) <- eForwardLookup (sstE sst) r
      return (s, rho_r `updateRho` kappa)

    updateRho = if weak then updateAbstractValuationWeak else updateAbstractValuation

abstractInterpretation :: (Ord st, Ord var, Eq delta
                          ,Function func, Rng func ~ [delta]) =>
                          Bool
                       -> SST st pred func var
                       -> (AbstractEnvironment st var delta, Int)
abstractInterpretation weak sst = go (sstS sst)
                                     (M.fromList [(st, M.empty) | st <- S.toList (sstS sst)])
                                     0
    where
      go states gamma i | S.null states = (gamma, i)
      go states gamma i = let (gamma', states') = updateAbstractEnvironment weak sst states gamma
                          in go states' gamma' (i+1)

applyAbstractEnvironment :: (Ord st, Ord var, Rng func ~ [delta]) =>
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
         -> (SST st pred func var, Int)
optimize level sst =
  let weak = level < 3
      applyOpt = level > 0
      (gamma, i) = abstractInterpretation weak sst
  in if applyOpt then (applyAbstractEnvironment gamma sst, i) else (sst, 0)

enumerateStates :: (Ord k) => SST k pred func var -> SST Int pred func var
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

flattenStream' :: (Monoid m) => Stream m -> Either String m
flattenStream' (Fail e) = Left e
flattenStream' Done = Right mempty
flattenStream' (Chunk x s) = mappend x <$> flattenStream' s

flattenStream :: (Monoid m) => Stream m -> m
flattenStream (Fail e) = error e
flattenStream Done = mempty
flattenStream (Chunk x s) = mappend x (flattenStream s)

-- | Compose two streams lazily; all data in the first stream is fed to the
-- given function, regardless of the final status in the tail. Errors are lazily
-- propagated to the tail in the resulting stream. If the function results in a
-- failing stream, the error message in the original stream takes priority.
composeStream :: (Monoid m) => Stream m -> (m -> Stream m') -> Stream m'
composeStream s f = let (m, e) = flat s in attachTail e $ f m
  where
    flat Done = (mempty, Nothing)
    flat (Fail e) = (mempty, Just e)
    flat (Chunk m s') = let (m', e) = flat s' in (mappend m m', e)

    attachTail Nothing s' = s'
    attachTail (Just e) s' = case s' of
      Chunk xs s'' -> Chunk xs (attachTail (Just e) s'')
      _ -> Fail e

valuate :: (Ord var) => Valuation var delta -> UpdateString var [delta] -> [delta]
valuate _ [] = []
valuate s (Right d:xs) = d ++ valuate s xs
valuate s (Left v:xs) = maybe [] id (M.lookup v s)
                        ++ valuate s xs

run :: forall var st pred func sigma delta.
        (Ord var, Ord st,
         Dom func ~ [sigma],
         SetLike pred sigma,
        Function func, Rng func ~ [delta])
    => SST st pred func var
    -> [sigma]
    -> Stream [delta]
run sst = go (sstI sst) (M.fromList [ (x, []) | x <- S.toList (sstV sst) ])
    where
      outVar = sstOut sst

      extractOutput s =
        case M.lookup outVar s of
          Nothing -> ([], M.insert outVar [] s)
          Just x -> (x, M.insert outVar [] s)

      go q s [] =
        case M.lookup q (sstF sst) of
          Nothing -> Fail "End of input reached, but final state is not accepting."
          Just out -> Chunk (valuate s out) Done
      go q s as = maybe (Fail "No match") id $ do
        ts <- M.lookup q (sstE sst)
        (upd, q', cs, as') <- findTrans as ts
        let (out, s') = extractOutput $ M.map (valuate s . evalUpdateStringFunc cs) upd
        return $ Chunk out (go q' s' as')

      match :: [sigma] -> [pred] -> Maybe (Int, [sigma], [sigma])
      match as [] = Just (0, [], as)
      match (a:as) (p:ps)
        | member a p = do
            (k, cs, as') <- match as ps
            return (k+1, a:cs, as')
      match _ _ = Nothing

      findTrans :: [sigma] -> [([pred], RegisterUpdate var func, st)]
                -> Maybe (RegisterUpdate var func, st, [sigma], [sigma])
      findTrans as ts =
        let cand = [ (k, (upd, st', cs, as'))
                     | (ps, upd, st') <- ts, Just (k, cs, as') <- [match as ps] ]
        in if null cand then Nothing else Just $ snd $ maximumBy (comparing fst) cand
