{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module KMC.SSTConstruction where

import           Control.Monad.State
import           Data.Maybe (isJust)
import qualified Data.Set as S
import           KMC.SymbolicFST
import           KMC.SymbolicSST
import           KMC.Theories
import           KMC.TreeWriter
import           KMC.Util.List (lcpMany1, linv')

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
type PathTree w a = [Tree w a]

visit :: (Ord st) => st -> Closure st w st
visit st = do { vis <- get;
                when (S.member st vis) zero;
                modify (S.insert st);
                return st
              }

-- | Resume a closure computation
resume :: Tree w a -> Closure st w a
resume (Tip w x) = tell w >> return x
resume (Fork w ts) = tell w >> branch (map resume ts)

-- | Resume a possibly failing closure computation
resume' :: PathTree w a -> Closure st w a
resume' [] = zero
resume' [t] = resume t
resume' _ = error "invalid path tree"

-- | Reduce a tree by removing longest common prefixes
reduce :: (Eq a) => Tree [a] b -> Tree [a] b
reduce (Tip w b)   = Tip w b
reduce (Fork w ts) =
  Fork (w ++ p) $ [ t { tOutput = linv' p (tOutput t) } | t <- ts' ]
  where
    ts' = map reduce ts
    p   = lcpMany1 (map tOutput ts')

-- | Interpret a closure computation as a path tree and reduce the result.
runClosure :: (Eq gamma) => Closure st [gamma] a -> PathTree [gamma] a
runClosure x = reduce <$> evalState (evalTreeWriterT x) S.empty

{------------------------------------------------------------------------------}
{-- Computing path trees --}

genclosure :: (Ord st, Monoid w)
           => FST st pred func
           -> (Rng func -> w)
           -> st
           -> Closure st w st
genclosure fst' inj q =
  case fstEvalEpsilonEdges fst' q of
    [] -> return q
    qs -> branch $
            map (\(out, q') -> tell (inj out)
                               >> visit q'
                               >> genclosure fst' inj q') qs

-- | Non-deterministically follow all non-input transitions.
closure :: (Ord st)
           => FST st pred func
           -> st
           -> Closure st (UpdateString var (Rng func)) st
closure fst' q = genclosure fst' (\c -> [Right c]) q

closureFunc :: (Ord st) =>
               FST st pred func
            -> st
            -> Closure st (UpdateStringFunc var func) st
closureFunc fst' q = genclosure fst' (\c -> [ConstA c]) q

consume :: (PartialOrder pred, Ord st)
        => FST st pred func -> pred -> Int -> st -> Closure st (UpdateStringFunc var func) st
consume fst' p i q =
  case fstAbstractEvalEdgesAll fst' q p of
    []        -> zero
    [(f, q')] -> tell [FuncA f i] >> visit q'
    _ -> error "Stepping for FSTs with read-fanout greater than one is not supported yet"

eof :: (Ord st) => FST st pred func -> st -> Closure st w st
eof fst' q | S.member q (fstF fst') = visit q
           | otherwise = zero

kill :: (Ord st) => S.Set st -> st -> Closure st (UpdateStringFunc var func) st
kill kills q | S.member q kills = zero
             | otherwise = return q

closureAbstractTree :: (Eq var, Eq (Rng func), Ord a) =>
                       FST a pred func
                    -> PathTree var a
                    -> PathTree (UpdateString var (Rng func)) a
closureAbstractTree fst' tr =
  let tr' = fmap (mapOutput (return . Left)) tr
  in runClosure (resume' tr' >>= closure fst')

closureTree :: (Eq var, Eq (Rng func), Ord st) =>
               FST st pred func
            -> PathTree (UpdateString var (Rng func)) st
            -> PathTree (UpdateString var (Rng func)) st
closureTree fst' tr = runClosure (resume' tr >>= closure fst')

closureTreeFunc :: (Eq var, Eq func, Eq (Rng func), Ord st) =>
               FST st pred func
            -> PathTree (UpdateStringFunc var func) st
            -> PathTree (UpdateStringFunc var func) st
closureTreeFunc fst' tr = runClosure (resume' tr >>= closureFunc fst')

eofTree :: (Ord a, Eq gamma) => FST a pred func -> PathTree [gamma] a -> PathTree [gamma] a
eofTree fst' tr = runClosure (resume' tr >>= eof fst')

consumeTree :: (Eq func, Eq var, Eq (Rng func), PartialOrder pred, Ord st)
            => FST st pred func
            -> pred
            -> Int
            -> PathTree (UpdateStringFunc var func) st
            -> PathTree (UpdateStringFunc var func) st
consumeTree fst' p i tr = runClosure (resume' tr >>= consume fst' p i)

killTree :: (Ord st, Eq var, Eq func, Eq (Rng func)) =>
            S.Set st -> PathTree (UpdateStringFunc var func) st
                     -> PathTree (UpdateStringFunc var func) st
killTree kills tr = runClosure (resume' tr >>= kill kills)

{------------------------------------------------------------------------------}
{-- Abstracting path trees --}

abstract :: Tree (UpdateStringFunc Var func) a
            -> ([(Var, UpdateStringFunc Var func)], Tree Var a)
abstract t = go [] t
  where
  go v (Tip w a) = ([(Var v, w)], Tip (Var v) a)
  go v (Fork w ts) =
    let (ms, ts') = unzip [ go (m:v) t' | (m, t') <- zip [0..] ts ]
    in ((Var v, w):concat ms, Fork (Var v) ts')

abstract' :: PathTree (UpdateStringFunc Var func) a
            -> ([(Var, UpdateStringFunc Var func)], PathTree Var a)
abstract' [] = ([], [])
abstract' [t] = let (m', t') = abstract t in (m', [t'])
abstract' _ = error "invalid path tree"

unabstract :: PathTree (UpdateString var (Rng func)) a
           -> PathTree (UpdateStringFunc var func) a
unabstract = fmap (mapOutput (constUpdateStringFunc))

{------------------------------------------------------------------------------}
{-- SST specialization --}

specialize :: (Function func, Rng func ~ [delta]
              ,Enumerable pred (Dom func)
              ,Ord st) =>
              [(st, [pred], [(var, UpdateStringFunc var func)], st)]
              -> [(st, [pred], [(var, UpdateStringFunc var func)], st)]
specialize ts =
  [ (q, ps, [ (v, resolveConstFunc $ specFunc ps f) | (v,f) <- xs ], q')
    | (q, ps, xs, q') <- ts ]
  where
    resolveConstFunc = normalizeUpdateStringFunc . concatMap resolve
    resolve (FuncA f _) | Just c <- isConst f = [ConstA c]
    resolve x = [x]

    specFunc ps f =
      if product (map size ps) == 1 then
          constUpdateStringFunc $ evalUpdateStringFunc (map (lookupIndex 0) ps) f
      else
          f

{------------------------------------------------------------------------------}
{-- SST construction from FST --}

consumeTreeMany :: (PartialOrder pred, Ord st, Eq var, Eq func, Eq (Rng func)) =>
                   FST st pred func
                -> [pred]
                -> PathTree (UpdateStringFunc var func) st
                -> PathTree (UpdateStringFunc var func) st
consumeTreeMany fst' = go 0
    where
      go _ [] tr = tr
      go i (p:ps) tr = go (i+1) ps (closureTreeFunc fst' (consumeTree fst' p i (closureTreeFunc fst' tr)))

sstFromFST :: (Ord a, Eq delta, Eq func, Ord pred, PartialOrder pred, Function func,
               Enumerable pred (Dom func), Rng func ~ [delta]) =>
              FST a pred func -> Bool -> SST (PathTree Var a) pred func Var
sstFromFST fst' singletonMode =
  construct initialState (specialize transitions) outputs
  where
    initialState           = [Tip (Var []) (fstI fst')]
    (transitions, outputs) = saturate (S.singleton initialState) S.empty [] []

    saturate ws states trans outs
      | S.null ws                      = (trans, outs)
      | (t, ws') <- S.deleteFindMin ws, S.member t states
                                       = saturate ws' states trans outs
      | (t, ws') <- S.deleteFindMin ws =
        let tcl  = closureAbstractTree fst' t
            tcl' = unabstract tcl
            os   = [ (t, w) | Tip w _ <- eofTree fst' tcl ]

            ts   = do (ps, kills) <- prefixTests fst' singletonMode (concatMap tflat tcl')
                      guard $ not $ null ps
                      let (kappa, t'') = abstract' $ consumeTreeMany fst' ps $ killTree kills tcl'
                      guard (not $ null t'')
                      return (t, ps, kappa, t'')
            wl'  = S.fromList [ newt | (_,_,_,newt) <- ts ]
        in saturate (S.union ws' wl') (S.insert t states) (ts ++ trans) (os ++ outs)
