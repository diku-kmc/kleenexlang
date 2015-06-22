{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
module KMC.FSTConstruction ( Mu(..)
                           , fromMu
                           , fromMuWithDFA
                           ) where

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
import KMC.SSTConstruction hiding (Var)

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

addNullEdges :: FST st pred (NullFun a b)
             -> Construct st pred (func :+: (NullFun a b)) ()
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
           -> Construct st pred (func :+: (NullFun a b)) st
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
          -> Construct st pred (func :+: (NullFun a b)) st
construct curPos qf e = do
  ms <- gets marks
  if curPos `S.member` ms then
      do
        q <- fresh
        let dfaFST = dfaAsFST $ enumerateDFAStatesFrom q $ mergeEdges $
                     minimizeDFA $ dfaFromMu e
        -- Add all the "null edges" from the DFA
        addNullEdges dfaFST
        -- Connect the final states of the DFA to the current final state.
        connectTo (S.toList (fstF dfaFST)) qf
        -- Add the states and increment the state counter to avoid clashes.
        addStates dfaFST
        -- The new final state is the initial state of the DFA.
        return (fstI dfaFST)
  else
      construct' curPos qf e

connectTo :: (Monoid (Rng func)) => [st] -> st -> Construct st pred func ()
connectTo states to = mapM_ (\from -> addEdge from (Right mempty) to) states
                 
-- | Constructs an FST from the given mu-term but uses a DFA construction on
-- all subterms at the positions in the given `Marked` set.  If the set is
-- empty a normal FST will be produced.
fromMuWithDFA :: (Predicate pred, Enum st, Ord st, Monoid (Rng func))
              => Marked
              -> Mu pred func st -> FST st pred (func :+: (NullFun a b))
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
          Mu pred func st -> FST st pred (func :+: (NullFun a b))
fromMu = fromMuWithDFA S.empty

------------------------------------------------------------
------------------------------------------------------------
------------------------------------------------------------

s1 :: String
s1 =  [strQ|x
       x := (~aaa | aa)*
       aaa := /aaa/ "bcd"
       aa := /aa/ "de"
       |]

s2 :: String
s2 = [strQ|x
x := ~(l r)
l := /(a|b)*/ "AB"
r := /(c|d)*/ "CD"
|]

s3 :: String
s3 = [strQ|x
x := /a/ ~/b/ /c/
|]

s4 :: String
s4 = [strQ|x
x := ~(/def*/)?
y := /end/
|]

mu1 :: (KleenexMu a, Marked)
mu1 = either (error "mu1 - fail") head $ testKleenex s1

sm1 :: (SimpleMu, Marked)
sm1 = either (error "sm1") head $ testSimple s1

mu2 = either (error "mu2") head $ testKleenex s2
mu3 = either (error "mu3") head $ testKleenex s3
mu4 = either (error "mu4") head $ testKleenex s4
      
f1 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
f1 = fromMu (fst mu1)

g1 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
g1 = fromMuWithDFA (snd mu1) (fst mu1)

f2 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
f2 = fromMu (fst mu2)
g2 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
g2 = fromMuWithDFA (snd mu2) (fst mu2)

f3 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
f3 = fromMu (fst mu3)
g3 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
g3 = fromMuWithDFA (snd mu3) (fst mu3)


f4 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
f4 = fromMu (fst mu4)
g4 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
g4 = fromMuWithDFA (snd mu4) (fst mu4)
     
          

viz :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
    -> FilePath -> IO ()
viz = mkVizToFile fstToDot

vizSST :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
       -> FilePath -> IO ()
vizSST f p = let s = sstFromFST f True
             in mkVizToFile sstToDot s p
