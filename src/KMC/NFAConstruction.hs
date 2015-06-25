{-# LANGUAGE QuasiQuotes #-}
module KMC.NFAConstruction ( nfaFromMu ) where

import qualified Data.Set as S
import           Data.Word
import           Control.Monad.State

import           KMC.SymbolicAcceptor
import           KMC.Expression

data NFAConstructState st pred =
  NFAConstructState { edges     :: [NFAEdge st pred]
                    , nextState :: st
                    , states    :: S.Set st
                    }

type NFAConstruct st pred = State (NFAConstructState st pred)

fresh :: (Enum st, Ord st) => NFAConstruct st pred st
fresh = do
  q <- gets nextState
  modify $ \s -> s { nextState = succ q
                   , states    = S.insert q (states s)
                   }
  return q

addEdge :: st -> Maybe pred -> st -> NFAConstruct st pred ()
addEdge q lbl q' = modify $ \s -> s { edges = (q, lbl, q'):edges s }


nfaConstruct :: (Enum st, Ord st) => st -> Mu pred f st -> NFAConstruct st pred st
nfaConstruct _ (Var q) = return q
nfaConstruct qf (Loop e) = mfix (nfaConstruct qf . e)
nfaConstruct qf (RW p _ e) = do
  q' <- nfaConstruct qf e
  q <- fresh
  addEdge q (Just p) q' -- read predicate p
  return q
nfaConstruct qf (W _ e) = do
  q' <- nfaConstruct qf e
  q <- fresh
  addEdge q Nothing q' -- epsilon
  return q
nfaConstruct qf (Alt e1 e2) = do
  q1 <- nfaConstruct qf e1
  q2 <- nfaConstruct qf e2
  q <- fresh
  addEdge q Nothing q1 -- epsilon
  addEdge q Nothing q2 -- epsilon
  return q
nfaConstruct qf Accept = return qf
nfaConstruct qf (Seq e1 e2) = do
  q2 <- nfaConstruct qf e2
  nfaConstruct q2 e1


nfaFromMu :: (Ord pred, Enum st, Ord st) => Mu pred f st -> NFA st pred
nfaFromMu e =
  let (qin, cs) = runState (nfaConstruct (toEnum 0) e)
                           (NFAConstructState { edges     = []
                                              , nextState = toEnum 1
                                              , states    = S.singleton (toEnum 0)
                                              })
  in NFA $ Acceptor { accS = states cs
                    , accE = nfaEdgesFromList (edges cs)
                    , accI = S.singleton qin
                    , accF = S.singleton (toEnum 0)
                    }

