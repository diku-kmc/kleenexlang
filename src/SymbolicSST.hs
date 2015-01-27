{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module SymbolicSST where

import           Control.Applicative

import qualified Data.Set as S
import qualified Data.Map as M

import           Theories

type Valuation var delta = M.Map var [delta]
type Environment st var delta = M.Map st (Valuation var delta)
type RegisterUpdate var func = M.Map var func

data SST st pred func var delta =
  SST
  { sstS :: S.Set st -- ^ State set
  , sstE :: M.Map st [(pred, RegisterUpdate var func, st)] -- ^ Symbolic transition relation
  , sstI :: st -- ^ Initial state
  , sstF :: M.Map st [Either var delta] -- ^ Final states with final output
  , sstV :: S.Set var -- ^ Output variables. The minimal variable is the designated output variable.
  }
  deriving (Show)

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

-- TODO: Implement static analysis. See old code.

{-- Simulation --}

valuate :: (Ord var) => Valuation var delta -> [Either var delta] -> [delta]
valuate _ [] = []
valuate s (Right d:xs) = d:valuate s xs
valuate s (Left v:xs) = maybe (error "valuate: Variable not in valuation") id (M.lookup v s)
                        ++ valuate s xs

data Stream a = Chunk a (Stream a) | Done | Fail String

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
        ts <- M.lookup q (sstE sst)
        (upd, q') <- findTrans a ts
        let (out, s') = extractOutput $ M.map (valuate s . flip evalFunction a) upd
        return $ Chunk out (go q' s' as)

      findTrans _ [] = Nothing
      findTrans a ((p, upd, q'):ts) =
        if evalBoolean p a then
            Just (upd, q')
        else
            findTrans a ts
