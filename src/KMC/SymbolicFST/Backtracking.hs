{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module KMC.SymbolicFST.Backtracking(interp) where

import           Control.Applicative
import           Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Set as S

import           KMC.Backtracking
import           KMC.SymbolicFST
import           KMC.Theories

interp :: (Monoid m, Function func, SetLike pred (Dom func), Stream s (Dom func))
       => (Rng func -> m)
       -> FST Int pred func
       -> P s m
interp phi fst' = lfp!(fstI fst') <* eof
  where
    lfp = M.fromList [ (q, go q) | q <- S.toList $ fstS fst' ]
    go q =
      (if isJoinState fst' q then barrier q else pure ())
      *> case fstEvalEpsilonEdges fst' q of
           [] -> case fstEdges fst' q of
                   [] -> pure mempty
                   es -> consume es
                 -- TODO: Only insert fBarrier if q is on an epsilon-loop
           es -> fBarrier q *> choose es

    consume es =
      foldl1 (<|>)
        [ mappend <$> (phi . eval func <$> litp (flip member p)) <*> (lfp!q')
        | (p, func, q') <- es ]

    choose [] = empty
    choose es = foldl1 (<|>) [ mappend (phi y) <$> (lfp!q') | (y, q') <- es ]

{-
-- | Example: This is how to interpret an action FST
interpAction :: (Function func
                ,SetLike pred Word8
                ,Dom func ~ Word8
                ,Rng func ~ [Either Word8 RegAction])
             => FST Int pred func -> ByteString -> Maybe Action
interpAction fst' b = runP' (interp (mconcat . map adjActionSem) fst') (0::Int, b)
-}
