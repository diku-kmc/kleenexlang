{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
module KMC.FSTConstruction
(Mu(..)
,fromMu
)
where

import           Control.Monad.State
import           Data.Monoid (Monoid, mempty)
import qualified Data.Set as S

import           KMC.Expression
import           KMC.DFAConstruction
import           KMC.SymbolicAcceptor
import           KMC.SymbolicFST
import           KMC.Theories
import           KMC.OutputTerm 
import           KMC.DFAConstruction

-- for tests in bottom
import KMC.Kleenex.Lang
import KMC.Util.Heredoc
import KMC.RangeSet
import Data.Word
import Debug.Trace
import KMC.Visualization

data ConstructState st pred func =
  ConstructState { edges     :: [Edge st pred func]
                 , nextState :: st
                 , states    :: S.Set st
                 , marks     :: Marked
                 }

type Construct st pred func = State (ConstructState st pred func)

fresh :: (Enum st, Ord st) => Construct st pred func st
fresh = do
  q <- gets nextState
  modify $ \s -> s { nextState = succ q
                   , states    = S.insert q (states s)
                   }
  return q

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

addNullEdges :: FST st pred (NullFun a) -> Construct st pred (func :+: (NullFun a)) ()
addNullEdges fst = modify $ \s -> s {
                     edges = edges s ++ map chg (edgesToList (fstE fst))
                   }
    where
      chg (q, Left (p, f), q') = (q, Left (p, Inr f), q')
--      chg (q, r, q')     = (q, r, q')

addEdge :: st -> Either (pred, func) (Rng func) -> st -> Construct st pred func ()
addEdge q lbl q' = modify $ \s -> s { edges = (q, lbl, q'):edges s }


construct' :: (Predicate pred, Enum st, Ord st, Monoid (Rng func))
           => Pos -> st -> Mu pred func st -> Construct st pred (func :+: (NullFun a)) st
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
          => Pos -> st -> Mu pred func st -> Construct st pred (func :+: (NullFun a)) st
construct curPos qf e = do
  ms <- gets marks
  if curPos `S.member` ms then
      do
        q <- fresh
        let dfaFST = dfaAsFST $ enumerateDFAStatesFrom q $ minimizeDFA $ dfaFromMu e
        addNullEdges dfaFST
        mapM_ (\q' -> addEdge q' (Right mempty) qf) (S.toList $ fstF dfaFST)
        -- Jump the state counter forward to avoid name clashes.
        addStates dfaFST
        q' <- fresh
        addEdge q' (Right mempty) (fstI dfaFST)
        return q'
  else
      construct' curPos qf e

fromMuWithAcceptor :: (Predicate pred, Enum st, Ord st, Monoid (Rng func))
                   => Marked
                   -> Mu pred func st -> FST st pred (func :+: (NullFun a))
fromMuWithAcceptor ms e =
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
          Mu pred func st -> FST st pred (func :+: (NullFun a))
fromMu = fromMuWithAcceptor S.empty

------------------------------------------------------------
------------------------------------------------------------
------------------------------------------------------------

s1 :: String
s1 =  [strQ|x
       x := (~aaa | aa)*
       aaa := <aaa> "bcd"
       aa := <aa> "de"
       |]

s2 :: String
s2 = [strQ|x
x := ~(l r)
l := <(a|b)*> "AB"
r := <(c|d)*> "CD"
|]

mu1 :: (KleenexMu a, Marked)
mu1 = either (error "mu1 - fail") head $ testKleenex s1

sm1 :: (SimpleMu, Marked)
sm1 = either (error "sm1") head $ testSimple s1

mu2 = either (error "mu2") head $ testKleenex s2

      
f1 :: FST Int (RangeSet Word8) (KleenexOutTerm :+: (NullFun Word8))
f1 = fromMu (fst mu1)

g1 :: FST Int (RangeSet Word8) (KleenexOutTerm :+: (NullFun Word8))
g1 = fromMuWithAcceptor (snd mu1) (fst mu1)

f2 :: FST Int (RangeSet Word8) (KleenexOutTerm :+: (NullFun Word8))
f2 = fromMu (fst mu2)
g2 :: FST Int (RangeSet Word8) (KleenexOutTerm :+: (NullFun Word8))
g2 = fromMuWithAcceptor (snd mu2) (fst mu2)


viz :: FST Int (RangeSet Word8) (KleenexOutTerm :+: (NullFun Word8))
    -> FilePath -> IO ()
viz = mkVizToFile fstToDot
