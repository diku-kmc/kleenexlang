{-# LANGUAGE FlexibleContexts #-}
module SSTConstruction where

import           Control.Monad.State
import           Data.Monoid
import qualified Data.Set as S
import qualified Data.Map as M
import           SymbolicFST
import           SymbolicSST
import           Theories
import           TreeWriter

data Var = Var [Int]
  deriving (Eq, Ord, Show)

{-- Closure monad --}

-- | The closure monad models a computation consisting of concurrent threads
-- with shared state executed in round-robin
type Closure st delta func = TreeWriterT (func (Either Var delta)) (State (S.Set st))

visit :: (Ord st) => st -> Closure st t delta st
visit st = do { vis <- get;
                when (S.member st vis) zero;
                modify (S.insert st);
                return st
              }

-- | Resume a closure computation
resume :: Tree (func (Either Var delta)) a -> Closure st delta func a
resume (Tip w x) = tell w >> return x
resume (Fork w t1 t2) = tell w >> plus (resume t1) (resume t2)

-- | Resume a possibly failing closure computation
resume' :: Maybe (Tree (func (Either Var delta)) a) -> Closure st delta func a
resume' = maybe zero resume

-- | Interpret a closure computation as a path tree.
runClosure :: (Monoid (func (Either Var delta)))
           => Closure st delta func a -> Maybe (Tree (func (Either Var delta)) a)
runClosure x = evalState (evalTreeWriterT x) S.empty

{------------------------------------------------------------------------------}
{-- Computing path trees --}

inj :: (Monad func, Monoid (func (Either Var delta))) => [delta] -> func (Either Var delta)
inj = mconcat . map (return . Right)

-- | Non-deterministically follow all non-input transitions.
closure :: (Monad func, Monoid (func (Either Var delta)), Ord st)
           => FST st pred (func delta) [delta] -> st -> Closure st delta func st
closure fst' q =
  case fstEvalEpsilonEdges fst' q of
    [] -> return q
    [(out, q')] ->
        tell (inj out) >> visit q' >> closure fst' q'
    [(out1, q1'), (out2, q2')] ->
       plus (tell (inj out1) >> visit q1' >> closure fst' q1')
            (tell (inj out2) >> visit q2' >> closure fst' q2')
    _ -> error "Computing closures for FSTs with epsilon-fanout greater than two is not supported yet"

step :: (DecBoolean pred, Monad func, Monoid (func (Either Var delta)), Ord st)
        => FST st pred (func delta) [delta] -> pred -> st -> Closure st delta func st
step fst' p q =
  case fstAbstractEvalEdges fst' q p of
    [] -> zero
    [(f, q')] ->
      tell (f >>= return . Right) >> closure fst' q'
    _ -> error "Stepping for FSTs with read-fanout greater than one is not supported yet"

eof :: (Ord st) => FST st pred (func delta) [delta] -> st -> Closure st delta func st
eof fst' q | S.member q (fstF fst') = return q
           | otherwise = zero

stepTree :: (DecBoolean pred, Monad func, Monoid (func (Either Var delta)), Ord st)
            => FST st pred (func delta) [delta]
            -> pred
            -> Maybe (Tree (func (Either Var delta)) st)
            -> Maybe (Tree (func (Either Var delta)) st)
stepTree fst' p tr = runClosure (resume' tr >>= step fst' p)

{------------------------------------------------------------------------------}
{-- Abstracting path trees --}

abstract :: (Monad func)
            => Tree (func (Either Var delta)) a
            -> (RegisterUpdate Var (func (Either Var delta))
               ,Tree (func (Either Var delta)) a)
abstract t = go [] t
  where
  go v (Tip w a) = (M.singleton (Var v) w, Tip (return . Left $ Var v) a)
  go v (Fork w t1 t2) =
    let (m1', t1') = go (0:v) t1
        (m2', t2') = go (1:v) t2
    in (M.insert (Var v) w (M.union m1' m2'), Fork (return . Left $ Var v) t1' t2')

abstract' :: (Monad func)
            => Maybe (Tree (func (Either Var delta)) a)
            -> (RegisterUpdate Var (func (Either Var delta))
               ,Maybe (Tree (func (Either Var delta)) a))
abstract' Nothing = (M.empty, Nothing)
abstract' (Just t) = let (m', t') = abstract t in (m', Just t')


{------------------------------------------------------------------------------}
{-- SST specialization --}

specialize :: (Monad func
              ,Monoid (func (Either var delta))
              ,Function (func (Either var delta)) dom [Either var delta]
              ,Enumerable pred dom
              ,Ord st) =>
              [(st, pred, [(var, func (Either var delta))], st)]
              -> [(st, pred, [(var, func (Either var delta))], st)]
specialize ts =
  [ (q, p, [ (v, specFunc p f) | (v,f) <- xs ], q')
    | (q, p, xs, q') <- ts ]
  where
    specFunc p f =
      if size p == 1 then
          mconcat $ map return (evalFunction f (lookupIndex 0 p))
      else
          f

{------------------------------------------------------------------------------}
{-- SST construction from FST --}

sstFromFST :: (Monad func
              ,Monoid (func (Either Var delta))
              ,Enumerable pred dom
              ,Function (func (Either Var delta)) dom [Either Var delta]
              ,Ord st) =>
              FST st pred (func delta) [delta]
           -> SST st pred (func (Either Var delta)) Var delta
sstFromFST fst' =
  construct initialState (specialize transitions) outputs
  where
    transitions  = undefined
    initialState = undefined
    outputs      = undefined
