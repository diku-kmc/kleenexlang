{-# LANGUAGE FlexibleContexts #-}
module SSTConstruction where

import           Control.Monad.State
import           Data.Maybe (isJust)
import           Data.Monoid
import qualified Data.Set as S
import           SymbolicFST
import           SymbolicSST
import           Theories
import           TreeWriter

data Var = Var [Int]
  deriving (Eq, Ord, Show)

instance PartialOrder Var where
  lte (Var l) (Var r) = reverse l `prefixOf` reverse r
      where
        prefixOf [] _ = True
        prefixOf (x:xs) (y:ys) = (x==y) && prefixOf xs ys
        prefixOf _ _ = False

{-- Closure monad --}

-- | The closure monad models a computation consisting of concurrent threads
-- with shared state executed in round-robin
type Closure st delta func = TreeWriterT (func (Either Var delta)) (State (S.Set st))
type PathTree w a = Maybe (Tree w a)

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
resume' :: PathTree (func (Either Var delta)) a -> Closure st delta func a
resume' = maybe zero resume

-- | Interpret a closure computation as a path tree.
runClosure :: (Monoid (func (Either Var delta)))
           => Closure st delta func a -> PathTree (func (Either Var delta)) a
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

consume :: (PartialOrder pred, Monad func, Monoid (func (Either Var delta)), Ord st)
        => FST st pred (func delta) [delta] -> pred -> st -> Closure st delta func st
consume fst' p q =
  case fstAbstractEvalEdges fst' q p of
    [] -> zero
    [(f, q')] ->
      tell (f >>= return . Right) >> return q' --closure fst' q'
    _ -> error "Stepping for FSTs with read-fanout greater than one is not supported yet"

--step :: (PartialOrder pred, Monad func, Monoid (func (Either Var delta)), Ord st)
--        => FST st pred (func delta) [delta] -> pred -> st -> Closure st delta func st
--step fst' p q = closure fst' q >>= consume fst' p

eof :: (Ord st) => FST st pred (func delta) [delta] -> st -> Closure st delta func st
eof fst' q | S.member q (fstF fst') = return q
           | otherwise = zero

closureAbstractTree :: (Monad func, Monoid (func (Either Var delta)), Ord st)
            => FST st pred (func delta) [delta]
            -> PathTree Var st
            -> PathTree (func (Either Var delta)) st
closureAbstractTree fst' tr =
  let tr' = fmap (mapOutput (return . Left)) tr
  in runClosure (resume' tr' >>= closure fst')

eofTree :: (Ord st, Monoid (func (Either Var delta)))
        => FST st pred (func delta) [delta]
        -> PathTree (func (Either Var delta)) st
        -> PathTree (func (Either Var delta)) st
eofTree fst' tr = runClosure (resume' tr >>= eof fst')

consumeTree :: (PartialOrder pred, Monad func, Monoid (func (Either Var delta)), Ord st)
            => FST st pred (func delta) [delta]
            -> pred
            -> PathTree (func (Either Var delta)) st
            -> PathTree (func (Either Var delta)) st
consumeTree fst' p tr = runClosure (resume' tr >>= consume fst' p)

{------------------------------------------------------------------------------}
{-- Abstracting path trees --}

abstract :: (Monad func)
            => Tree (func (Either Var delta)) a
            -> ([(Var, func (Either Var delta))], Tree Var a)
abstract t = go [] t
  where
  go v (Tip w a) = ([(Var v, w)], Tip (Var v) a)
  go v (Fork w t1 t2) =
    let (m1', t1') = go (0:v) t1
        (m2', t2') = go (1:v) t2
    in ((Var v, w):(m1' ++ m2'), Fork (Var v) t1' t2')

abstract' :: (Monad func)
            => PathTree (func (Either Var delta)) a
            -> ([(Var, func (Either Var delta))], PathTree Var a)
abstract' Nothing = ([], Nothing)
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
              ,PartialOrder pred
              ,Enumerable pred dom
              ,Function (func (Either Var delta)) dom [Either Var delta]
              ,Ord st, Ord pred)
           => FST st pred (func delta) [delta]
           -> SST (PathTree Var st) pred (func (Either Var delta)) Var delta
sstFromFST fst' =
  construct initialState (specialize transitions) outputs
  where
    initialState           = Just (Tip (Var []) (fstI fst'))
    (transitions, outputs) = saturate (S.singleton initialState) S.empty [] []

    saturate ws states trans outs
      | S.null ws                      = (trans, outs)
      | (t, ws') <- S.deleteFindMin ws, S.member t states
                                       = saturate ws' states trans outs
      | (t, ws') <- S.deleteFindMin ws =
        let t'  = closureAbstractTree fst' t
            wl' = S.fromList [ newt | (_,_,_,newt) <- ts ]
            ts  = do p <- coarsestPredicateSet fst' (tflat' t')
                     let (kappa, t'') = abstract' $ consumeTree fst' p t'
                     guard (isJust t'')
                     return (t, p, kappa, t'')
            os  = [ undefined | Just (Tip w _) <- [eofTree fst' t'] ]
        in saturate (S.union ws' wl') (S.insert t states) (ts ++ trans) (os ++ outs)
