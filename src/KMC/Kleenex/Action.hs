{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE MultiParamTypeClasses #-}

module KMC.Kleenex.Action where

import           Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Word
import           KMC.Expression
import           KMC.OutputTerm
import qualified KMC.RangeSet as RS
import           KMC.SymbolicSST
import           KMC.Theories

type KleenexAction = ActionExpr Int Word8 [Word8]
type KleenexActionMu a = Mu BitInputTerm KleenexAction a

type BitInputTerm = RS.RangeSet Word8

matchVal :: BitInputTerm
matchVal = RS.singleton minBound

-- | The term that copies the input char to output.
parseBitsAction :: RS.RangeSet Word8 -> KleenexAction
parseBitsAction rs = ParseBits rs

-- | The term that outputs nothing.
nop :: a -> KleenexAction
nop = const (OutputConst [])


-- SST Construction with a bit oracle for determinization
data ConstructState st pred func var =
  ConstructState { edges     :: EdgeSet st pred func var
                 , nextState :: st
                 , states    :: S.Set st
                 , marks     :: Marked
                 }

type Construct st pred func var = State (ConstructState st pred func var)

fresh :: (Enum st, Ord st) => Construct st pred func var st
fresh = do
  q <- gets nextState
  modify $ \s -> s { nextState = succ q
                   , states    = S.insert q (states s)
                   }
  return q

addEdge :: (Ord st) => st -> [pred] -> EdgeAction var func -> st
        -> Construct st pred func var ()
addEdge q m o q' = modify $ \s -> s { edges = M.insertWith (++) q [(m, o, q')] (edges s) }

actionConstruct :: (Enum st, Ord st, Monoid (Rng KleenexAction))
             => st -> Mu BitInputTerm KleenexAction st -> Construct st BitInputTerm (WithNull KleenexAction) Int st
actionConstruct _ (Var q) = return q
actionConstruct qf (Loop e) = mfix (actionConstruct qf . e)
actionConstruct qf (RW p f e) = do
  q' <- actionConstruct qf e
  q <- fresh
  addEdge q [p] (fixAction f) q'
  return q
actionConstruct qf (W d e) = do
  q' <- actionConstruct qf e
  q <- fresh
  addEdge q [] (Inr $ OutputConst d) q'
  return q
actionConstruct qf (Action a e) = do
  q' <- actionConstruct qf e
  q <- fresh
  addEdge q [] (fixAction a) q'
  return q
actionConstruct qf (Alt e1 e2) = do
  q1 <- actionConstruct qf e1
  q2 <- actionConstruct qf e2
  q <- fresh
  addEdge q [bFalse] (Inr $ OutputConst []) q1
  addEdge q [bTrue]  (Inr $ OutputConst []) q2
  return q
actionConstruct qf Accept = return qf
actionConstruct qf (Seq e1 e2) = do
  q2 <- actionConstruct qf e2
  actionConstruct q2 e1

fixAction :: KleenexAction -> EdgeAction Int (WithNull KleenexAction)
fixAction (RegUpdate var atoms) = Inl $ M.singleton var $ map conv atoms
  where
    conv (FuncA f i) = FuncA (Inl f) i
    conv (VarA v) = VarA v
    conv (ConstA c) = ConstA c
fixAction (ParseBits rs) = Inl $ M.singleton 0 [VarA 0, FuncA (Inl $ ParseBits rs) 0]
fixAction a = Inr $ a

genActionSST :: (Ord st, Enum st, Show st) => KleenexActionMu st -> SST st BitInputTerm (WithNull KleenexAction) Int
genActionSST mu = sst
    where
        (qin, cs) = runState (actionConstruct (toEnum 0) mu)
                             (ConstructState { edges     = M.empty
                                             , nextState = toEnum 1
                                             , states    = S.singleton (toEnum 0)
                                             , marks     = S.empty
                                             })
        sst = do SST { sstS = states cs
                     , sstE = edges cs
                     , sstI = qin
                     , sstF = final
                     }
            where
                final = M.fromList $ [(toEnum 0,[Left 0])]

