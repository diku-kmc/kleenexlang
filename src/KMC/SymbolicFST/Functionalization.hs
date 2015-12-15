{-# LANGUAGE FlexibleContexts #-}
module KMC.SymbolicFST.Functionalization (functionalize) where

import qualified Data.Set as S

import           KMC.SymbolicFST
import           KMC.Theories

type St q = (q, S.Set q)

choice :: (Ord st, Monoid (Rng func)) => FST st pred func -> St st -> [(Rng func, St st)]
choice fst' (q, r) = go rclos (rightClosure fst' q)
  where
    rclos = S.unions $ map (rightInputClosure fst') $ S.toList r

    go _ [] = []
    go racc ((out, q'):xs)
      | S.member q' racc = go racc xs
      | otherwise        = (out, (q', racc))
                           :go (S.insert q' racc) xs

next :: (Ord st, PartialOrder pred)
     => FST st pred func -> St st -> pred -> [(pred, func, St st)]
next fst' (q, r) p = go rnext (fstAbstractEvalEdgesAll fst' q p)
  where
    rclos = S.unions $ map (rightInputClosure fst') $ S.toList r
    rnext = S.fromList $ concatMap absEv $ S.toList rclos
    absEv st = map snd $ fstAbstractEvalEdgesAll fst' st p

    go _ [] = []
    go racc ((func, q'):xs)
      | S.member q' racc = go racc xs
      | otherwise        = (p, func, (q', racc))
                           :go (S.insert q' racc) xs
    
functionalize :: (Ord q, Ord pred, Monoid (Rng func), Boolean pred, PartialOrder pred)
              => FST q pred func
              -> FST (q, S.Set q) pred func
functionalize fst' =
  let initialState    = (fstI fst', S.empty)
      (states, edges) = go (S.singleton initialState) S.empty []
  in trim $ FST { fstS = states
                , fstE = edgesFromList edges
                , fstI = initialState
                , fstF = S.filter isFinal states
                }
  where
    isFinal (q, r) = S.member q (fstF fst') && not (S.member q r)
    
    go ws states trans
      | S.null ws                       = (states, trans)
      | (st, ws') <- S.deleteFindMin ws
        , S.member st states            = go ws' states trans
      | (st@(q, r), ws') <- S.deleteFindMin ws =
          let choices = choice fst' st
              ps      = coarsestPredicateSet fst' (q:S.toList r)
              succs   = ps >>= next fst' st
              ws''    = S.union (S.fromList (map snd choices))
                                (S.fromList (map (\(_,_,st') -> st') succs))
              ts      = [ (st, Right y, st') | (y, st') <- choices, fst st /= fst st' ]
                        ++ [ (st, Left (p, f), st') | (p, f, st') <- succs ]
          in go (S.union ws' ws'') (S.insert st states) (ts ++ trans)
