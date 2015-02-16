{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module KMC.SSTConstruction where

import           Control.Monad.State
import           Data.Maybe (isJust)
import           Data.Monoid
import qualified Data.Set as S
import           KMC.SymbolicFST
import           KMC.SymbolicSST
import           KMC.Theories
import           KMC.TreeWriter

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
type Closure st w = TreeWriterT w (State (S.Set st))
type PathTree w a = Maybe (Tree w a)

visit :: (Ord st) => st -> Closure st w st
visit st = do { vis <- get;
                when (S.member st vis) zero;
                modify (S.insert st);
                return st
              }

-- | Resume a closure computation
resume :: Tree w a -> Closure st w a
resume (Tip w x) = tell w >> return x
resume (Fork w t1 t2) = tell w >> plus (resume t1) (resume t2)

-- | Resume a possibly failing closure computation
resume' :: PathTree w a -> Closure st w a
resume' = maybe zero resume

-- | Interpret a closure computation as a path tree.
runClosure :: (Monoid w) => Closure st w a -> PathTree w a
runClosure x = evalState (evalTreeWriterT x) S.empty


{------------------------------------------------------------------------------}
{-- Computing path trees --}


-- | Non-deterministically follow all non-input transitions.
closure :: (Ord st)
           => FST st pred func
           -> st
           -> Closure st (UpdateString var (Rng func)) st
closure fst' q =
  case fstEvalEpsilonEdges fst' q of
    [] -> return q
    [(out, q')] ->
        tell [Right out] >> visit q' >> closure fst' q'
    [(out1, q1'), (out2, q2')] ->
       plus (tell [Right out1] >> visit q1' >> closure fst' q1')
            (tell [Right out2] >> visit q2' >> closure fst' q2')
    _ -> error "Computing closures for FSTs with epsilon-fanout greater than two is not supported yet"

consume :: (PartialOrder pred, Ord st)
        => FST st pred func -> pred -> st -> Closure st (UpdateStringFunc var func) st
consume fst' p q =
  case fstAbstractEvalEdges fst' q p of
    []        -> zero
    [(f, q')] -> tell [FuncA f] >> visit q'
    _ -> error "Stepping for FSTs with read-fanout greater than one is not supported yet"

eof :: (Ord st) => FST st pred func -> st -> Closure st w st
eof fst' q | S.member q (fstF fst') = visit q
           | otherwise = zero

closureAbstractTree :: (Ord a) =>
                       FST a pred func
                    -> PathTree var a
                    -> PathTree (UpdateString var (Rng func)) a
closureAbstractTree fst' tr =
  let tr' = fmap (mapOutput (return . Left)) tr
  in runClosure (resume' tr' >>= closure fst')

eofTree :: (Ord a, Monoid w) => FST a pred func -> PathTree w a -> PathTree w a
eofTree fst' tr = runClosure (resume' tr >>= eof fst')

consumeTree :: (PartialOrder pred, Ord st)
            => FST st pred func
            -> pred
            -> PathTree (UpdateStringFunc var func) st
            -> PathTree (UpdateStringFunc var func) st
consumeTree fst' p tr = runClosure (resume' tr >>= consume fst' p)

{------------------------------------------------------------------------------}
{-- Abstracting path trees --}

abstract :: Tree (UpdateStringFunc Var func) a
            -> ([(Var, UpdateStringFunc Var func)], Tree Var a)
abstract t = go [] t
  where
  go v (Tip w a) = ([(Var v, w)], Tip (Var v) a)
  go v (Fork w t1 t2) =
    let (m1', t1') = go (0:v) t1
        (m2', t2') = go (1:v) t2
    in ((Var v, w):(m1' ++ m2'), Fork (Var v) t1' t2')

abstract' :: PathTree (UpdateStringFunc Var func) a
            -> ([(Var, UpdateStringFunc Var func)], PathTree Var a)
abstract' Nothing = ([], Nothing)
abstract' (Just t) = let (m', t') = abstract t in (m', Just t')

unabstract :: PathTree (UpdateString var (Rng func)) a -> PathTree (UpdateStringFunc var func) a
unabstract = fmap (mapOutput (constUpdateStringFunc))

{------------------------------------------------------------------------------}
{-- SST specialization --}

specialize :: (Function func, Rng func ~ [delta]
              ,Enumerable pred (Dom func)
              ,Ord st) =>
              [(st, pred, [(var, UpdateStringFunc var func)], st)]
              -> [(st, pred, [(var, UpdateStringFunc var func)], st)]
specialize ts =
  [ (q, p, [ (v, specFunc p f) | (v,f) <- xs ], q')
    | (q, p, xs, q') <- ts ]
  where
    specFunc p f =
      if size p == 1 then
          constUpdateStringFunc $ evalUpdateStringFunc (lookupIndex 0 p) f
      else
          f

{------------------------------------------------------------------------------}
{-- SST construction from FST --}

sstFromFST :: (Ord a, Ord pred, PartialOrder pred, Function func,
               Enumerable pred (Dom func), Rng func ~ [delta]) =>
              FST a pred func -> SST (Maybe (Tree Var a)) pred func Var
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
        let tcl  = closureAbstractTree fst' t
            tcl' = unabstract tcl
            wl'  = S.fromList [ newt | (_,_,_,newt) <- ts ]
            ts   = do p <- coarsestPredicateSet fst' (tflat' tcl')
                      let (kappa, t'') = abstract' $ consumeTree fst' p tcl'
                      guard (isJust t'')
                      return (t, p, kappa, t'')
            os   = [ (t, w) | Just (Tip w _) <- [eofTree fst' tcl] ]
        in saturate (S.union ws' wl') (S.insert t states) (ts ++ trans) (os ++ outs)
