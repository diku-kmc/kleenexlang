{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module KMC.FSTConstruction
(fromMu
,fromMuWithDFA
,edgesFromList)
where

import           Control.Monad.State
import qualified Data.Set as S

import           KMC.Expression
import           KMC.DFAConstruction
import           KMC.SymbolicAcceptor
import           KMC.SymbolicFST
import           KMC.Theories
import           KMC.OutputTerm

data ConstructState st pred func =
  ConstructState { edges     :: [Edge st pred func]
                 , nextState :: st
                 , states    :: S.Set st
                 , marks     :: Marks
                 }

type Construct st pred func = State (ConstructState st pred func)

fresh :: (Enum st, Ord st) => Construct st pred func st
fresh = do
  q <- gets nextState
  modify $ \s -> s { nextState = succ q
                   , states    = S.insert q (states s)
                   }
  return q

undoFresh :: (Enum st, Ord st) => Construct st pred func ()
undoFresh = do
  last <- gets nextState
  modify $ \s -> s { nextState = pred last
                   , states    = S.delete last (states s)
                   }

addStates :: (Enum st, Ord st) => FST st pred a -> Construct st pred func st
addStates fst = do
  q <- gets nextState
  let newNext = skipN q
  modify $ \s -> s { nextState = newNext
                   , states = (fstS fst) `S.union` states s
                   }
  return newNext
      where
        skipN x = foldr (const succ) x [1 .. S.size (fstS fst) - 1]

addNullEdges :: FST st pred (Const a b)
             -> Construct st pred (func :+: (Const a b)) ()
addNullEdges fst = modify $ \s -> s {
                     edges = edges s ++ map chg (edgesToList (fstE fst))
                   }
    where
      chg (q, Left (p, f), q') = (q, Left (p, Inr f), q')
--      chg (q, r, q')     = (q, r, q') -- case never happens

addEdge :: st -> Either (pred, func) (Rng func) -> st
        -> Construct st pred func ()
addEdge q lbl q' = modify $ \s -> s { edges = (q, lbl, q'):edges s }


construct' :: (Predicate pred, Enum st, Ord st, Monoid (Rng func))
           => Pos -> st -> Mu pred func st
           -> Construct st pred (func :+: (Const (Dom func) (Rng func))) st
construct' _ _ (Var q) = return q
construct' curPos qf (Loop e) = mfix (construct (L:curPos) qf . e)
construct' curPos qf (RW p f e) = do
  q' <- construct curPos qf e
  q <- fresh
  addEdge q (Left (p, Inl f)) q'
  return q
construct' curPos qf (W d e) = do
  q' <- construct curPos qf e
  q <- fresh
  addEdge q (Right d) q'
  return q
construct' curPos qf (Alt e1 e2) = do
  q1 <- construct (L:curPos) qf e1
  q2 <- construct (R:curPos) qf e2
  q <- fresh
  addEdge q (Right mempty) q1
  addEdge q (Right mempty) q2
  return q
construct' _ qf Accept = return qf
construct' curPos qf (Seq e1 e2) = do
  -- Note that we /first/ construct the right-hand side!
  q2 <- construct (R:curPos) qf e2
  construct (L:curPos) q2 e1


construct :: (Predicate pred, Enum st, Ord st, Monoid (Rng func))
          => Pos -> st -> Mu pred func st
          -> Construct st pred (func :+: (Const (Dom func) (Rng func))) st
construct curPos qf e = do
  ms <- gets marks
  if curPos `S.member` ms then
      do
        q <- fresh
        let dfa = enumerateDFAStatesFrom q $ mergeEdges $
                  minimizeDFA $ dfaFromMu e
        if isDFAPrefixFree dfa then
            do
              let dfaFST = dfaAsFST dfa
              -- Add all the "null edges" from the DFA
              addNullEdges dfaFST
              -- Connect the final states of the DFA to the current final state.
              connectTo (S.toList (fstF dfaFST)) qf
              -- Add the states and increment the state counter to avoid clashes.
              addStates dfaFST
              -- The new final state is the initial state of the DFA.
              return (fstI dfaFST)
        else
            -- Otherwise it is unsound to use the DFA, so don't.
            undoFresh >> construct' curPos qf e
  else
      construct' curPos qf e

connectTo :: (Monoid (Rng func)) => [st] -> st -> Construct st pred func ()
connectTo states to = mapM_ (\from -> addEdge from (Right mempty) to) states

-- | Constructs an FST from the given mu-term but uses a DFA construction on
-- all subterms at the positions in the given `Marks` set.  If the set is
-- empty a normal FST will be produced.
fromMuWithDFA :: (Predicate pred, Enum st, Ord st, Monoid (Rng func))
              => Marks
              -> Mu pred func st -> FST st pred (func :+: (Const (Dom func) (Rng func)))
fromMuWithDFA ms e =
  let (qin, cs) = runState (construct [] (toEnum 0) e)
                           (ConstructState { edges     = []
                                           , nextState = toEnum 1
                                           , states    = S.singleton (toEnum 0)
                                           , marks     = ms
                                           })
  in FST { fstS = states cs
         , fstE = edgesFromList (edges cs)
         , fstI = qin
         , fstF = S.singleton (toEnum 0)
         }

fromMu :: (Predicate pred, Enum st, Ord st, Monoid (Rng func)) =>
          Mu pred func st -> FST st pred (func :+: (Const (Dom func) (Rng func)))
fromMu = fromMuWithDFA S.empty

