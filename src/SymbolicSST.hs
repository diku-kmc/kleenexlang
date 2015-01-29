{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module SymbolicSST where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid

import qualified Data.Set as S
import qualified Data.Map as M

import           Theories

type Valuation var delta = M.Map var [delta]
type Environment st var delta = M.Map st (Valuation var delta)
type RegisterUpdate var func = M.Map var func

data EdgeSet st pred func var =
  EdgeSet
  { eForward  :: M.Map st [(pred, RegisterUpdate var func, st)]
  , eBackward :: M.Map st [(pred, RegisterUpdate var func, st)]
  }
  deriving (Show)

data SST st pred func var delta =
  SST
  { sstS :: S.Set st -- ^ State set
  , sstE :: EdgeSet st pred func var -- ^ Symbolic transition relation
  , sstI :: st -- ^ Initial state
  , sstF :: M.Map st [Either var delta] -- ^ Final states with final output
  , sstV :: S.Set var -- ^ Output variables. The minimal variable is the designated output variable.
  }
  deriving (Show)

edgesFromList :: (Ord st) => [(st, pred, RegisterUpdate var func, st)] -> EdgeSet st pred func var
edgesFromList xs = EdgeSet { eForward  = M.fromListWith (++) [ (q,  [(p, u, q')]) | (q,p,u,q') <- xs ]
                           , eBackward = M.fromListWith (++) [ (q', [(p, u, q)])  | (q,p,u,q') <- xs ]
                           }

edgesToList :: EdgeSet st pred func var -> [(st, pred, RegisterUpdate var func, st)]
edgesToList es = [ (q,p,u,q') | (q, xs) <- M.toList (eForward es), (p,u,q') <- xs ]

-- | Construct an SST from an edge set and a list of final outputs.
construct :: (Ord st, Ord var) =>
       st                              -- ^ Initial state
    -> [(st, pred, [(var, func)], st)] -- ^ Edge set
    -> [(st, [Either var delta])]      -- ^ Final outputs
    -> SST st pred func var delta
construct qin es os =
  SST
  { sstS = S.fromList (qin:concat [ [q, q'] | (q, _, _, q') <- es ])
  , sstE = edgesFromList [ (q, p, ru us, q') | (q, p, us, q') <- es ]
  , sstI = qin
  , sstF = outf os
  , sstV = S.fromList [ v | (_,_,xs,_) <- es, (v,_) <- xs ]
  }
  where
    outf = M.fromListWith (error "Inconsistent output function: Same state has more than one update.")
    ru = M.fromListWith (error "Inconsistent register update: Same variable updated more than once.")


{-- Analysis --}

data AbstractVal a = Exact a | Ambiguous deriving (Eq, Ord, Show, Functor)
type AbstractValuation var delta = M.Map var (AbstractVal [delta])
type AbstractEnvironment st var delta = M.Map st (AbstractValuation var delta)

instance Applicative AbstractVal where
  pure = Exact
  (Exact f) <*> (Exact x) = Exact (f x)
  _ <*> _ = Ambiguous

isExact :: AbstractVal a -> Bool
isExact (Exact _) = True
isExact _ = False

lubAbstractVal :: (Eq a) => AbstractVal a -> AbstractVal a -> AbstractVal a
lubAbstractVal (Exact x) (Exact y) = if x == y then Exact x else Ambiguous
lubAbstractVal _ _ = Ambiguous

lubAbstractValuations :: (Ord var, Eq delta) => [AbstractValuation var delta] -> AbstractValuation var delta
lubAbstractValuations = M.unionsWith lubAbstractVal

liftAbstractValuation :: (Ord var
                         ,DecFunction func [Either var delta]) =>
                         AbstractValuation var delta
                      -> func
                      -> Maybe (AbstractVal [delta])
liftAbstractValuation rho f
  | Just xs <- isConstant f = go xs
  | otherwise = Just Ambiguous
  where
    go [] = return (pure [])
    go (Left v:xs) = liftA2 (++) <$> M.lookup v rho <*> go xs
    go (Right x:xs) = liftA (x:) <$> go xs

updateAbstractValuation :: (Ord var
                           ,DecFunction func [Either var delta]) =>
                           AbstractValuation var delta
                        -> RegisterUpdate var func
                        -> AbstractValuation var delta
updateAbstractValuation rho kappa = M.union (M.mapMaybe (liftAbstractValuation rho) kappa) rho

applyAbstractValuation :: (Monad func
                          ,Monoid (func (Either var delta))
                          ,Ord var) => 
                          AbstractValuation var delta
                       -> func (Either var delta)
                       -> func (Either var delta)
applyAbstractValuation rho f = do
  atom <- f
  case atom of
   Right x -> return (Right x)
   Left v -> case M.lookup v rho of
               Just (Exact x) -> mconcat $ map (return . Right) x
               _ -> return (Left v)

updateAbstractEnvironment :: (Monad func
                             ,Monoid (func (Either var delta))
                             ,DecFunction (func (Either var delta)) [Either var delta]
                             ,Ord st, Ord var, Eq delta) =>
                             SST st pred (func (Either var delta)) var delta
                          -> [st]
                          -> AbstractEnvironment st var delta
                          -> (AbstractEnvironment st var delta, [st])
updateAbstractEnvironment sst states gamma =
  (M.union updates gamma
  ,S.toList $ S.unions (map succs (M.keys updates)))
  where
    -- Compute the set of successors of a given state
    succs q =
      S.fromList [ q' | Just xs <- [M.lookup q (eForward $ sstE sst)], (_,_,q') <- xs ]

    updates = M.unions $ do
      s <- states
      -- Compute the abstract valuation for the current state by applying the
      -- update function to the abstract valuations of all predecessors and
      -- taking the least upper bound.
      let rho_s' =
            lubAbstractValuations $
              [ maybe M.empty id (M.lookup r gamma) `updateAbstractValuation` kappa
                | (_, kappa, r) <- maybe [] id (M.lookup s (eBackward $ sstE sst)) ]
      -- Get the previous abstract valuation for the current state
      let rho_s = maybe M.empty id (M.lookup s gamma)
      -- Did we learn more information?
      guard (rho_s' /= rho_s)
      return (M.singleton s rho_s')

abstractInterpretation :: (Monad func
                          ,Monoid (func (Either var delta))
                          ,DecFunction (func (Either var delta)) [Either var delta]
                          ,Ord st, Ord var, Eq delta) =>
                          SST st pred (func (Either var delta)) var delta
                       -> AbstractEnvironment st var delta
abstractInterpretation sst = go (S.toList $ sstS sst) M.empty
    where
      go [] gamma = gamma
      go states gamma = let (gamma', states') = updateAbstractEnvironment sst states gamma
                        in go states' gamma'


{-- Simulation --}

valuate :: (Ord var) => Valuation var delta -> [Either var delta] -> [delta]
valuate _ [] = []
valuate s (Right d:xs) = d:valuate s xs
valuate s (Left v:xs) = maybe (error "valuate: Variable not in valuation") id (M.lookup v s)
                        ++ valuate s xs

data Stream a = Chunk a (Stream a) | Done | Fail String
  deriving (Show)

run :: (Ord st, Ord var, EffBoolean pred dom, Function func dom [Either var delta])
       => SST st pred func var delta -> [dom] -> Stream [delta]
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
        let (out, s') = extractOutput $ M.map (valuate s . flip evalFunction a) upd
        return $ Chunk out (go q' s' as)

      findTrans _ [] = Nothing
      findTrans a ((p, upd, q'):ts) =
        if evalBoolean p a then
            Just (upd, q')
        else
            findTrans a ts
