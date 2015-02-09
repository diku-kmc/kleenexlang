{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module KMC.FSTConstruction
(Mu(..)
,fromMu)
where

import           Control.Monad.State
import           Data.Monoid
import qualified Data.Set as S

import           KMC.Expression
import           KMC.SymbolicFST
import           KMC.Theories

data ConstructState st pred func =
  ConstructState { edges     :: [Edge st pred func]
                 , nextState :: st
                 , states    :: S.Set st
                 }

type Construct st pred func = State (ConstructState st pred func)

fresh :: (Enum st, Ord st) => Construct st pred func st
fresh = do
  q <- gets nextState
  modify $ \s -> s { nextState = succ q
                   , states    = S.insert q (states s)
                   }
  return q

addEdge :: st -> Either (pred, func) (Rng func) -> st -> Construct st pred func ()
addEdge q lbl q' = modify $ \s -> s { edges = (q, lbl, q'):edges s }

construct :: (Enum st, Ord st, Monoid (Rng func))
             => st -> Mu pred func st -> Construct st pred func st
construct _ (Var q) = return q
construct qf (Loop e) = mfix (construct qf . e)
construct qf (RW p f e) = do
  q' <- construct qf e
  q <- fresh
  addEdge q (Left (p, f)) q'
  return q
construct qf (W d e) = do
  q' <- construct qf e
  q <- fresh
  addEdge q (Right d) q'
  return q
construct qf (Alt e1 e2) = do
  q1 <- construct qf e1
  q2 <- construct qf e2
  q <- fresh
  addEdge q (Right mempty) q1
  addEdge q (Right mempty) q2
  return q
construct qf Accept = return qf
construct qf (Seq e1 e2) = do
  q2 <- construct qf e2
  construct q2 e1

fromMu :: (Enum st, Ord st, Monoid (Rng func)) =>
          Mu pred func st
       -> FST st pred func
fromMu e =
  let (qin, cs) = runState (construct (toEnum 0) e)
                           (ConstructState { edges     = []
                                           , nextState = toEnum 1
                                           , states    = S.singleton (toEnum 0)
                                           })
  in FST { fstS = states cs
         , fstE = edgesFromList (edges cs)
         , fstI = qin
         , fstF = S.singleton (toEnum 0)
         }
