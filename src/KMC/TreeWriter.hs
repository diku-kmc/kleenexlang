{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
module KMC.TreeWriter
(Tree(..)
,TreeWriterT
,TreeWriter
,mapOutput
,plus
,zero
,tell
,tflat
,tflat'
,evalTreeWriterT
,evalTreeWriter
) where

import Data.Monoid
import Control.Monad.Trans.Free.Church
import Control.Applicative
import Control.Monad.Identity

{------------------------------------------------------------------------------}
{-- Path trees and monad --}

-- Path trees are binary trees with an output w at each node and a state at each
-- fork.
data Tree w a = Tip { tOutput :: w, tValue :: a }
              | Fork { tOutput :: w
                     , tLeft :: Tree w a
                     , tRight :: Tree w a }
  deriving (Eq, Ord)

instance Functor (Tree w) where
  fmap f (Tip w a) = Tip w (f a)
  fmap f (Fork w t1 t2) = Fork w (fmap f t1) (fmap f t2)

mapOutput :: (w -> w') -> Tree w a -> Tree w' a
mapOutput f (Tip w a) = Tip (f w) a
mapOutput f (Fork w t1 t2) = Fork (f w) (mapOutput f t1) (mapOutput f t2)

-- | Prepends the given value to the top output of a tree
tprepend :: (Monoid w) => w -> Tree w a -> Tree w a
tprepend w (Tip w' x) = Tip (mappend w w') x
tprepend w (Fork w' x1 x2) = Fork (mappend w w') x1 x2

-- | Functor representing the operations that are performed in an NFA
-- simulation. The free monad over this functor can be interpreted as a path
-- tree.
data T w a = Out w a  -- ^ Output value w
           | Plus a a -- ^ Run computations in parallel from state q
           | Zero     -- ^ Fail
  deriving (Show, Functor)

-- | Free monad transformer over the T functor
type TreeWriterT w m = FT (T w) m
type TreeWriter w = TreeWriterT w Identity

-- | Branch operation
plus :: MonadFree (T w) m => m a -> m a -> m a
plus x1 x2 = wrap (Plus x1 x2)

-- | Fail operation
zero :: MonadFree (T w) m => m a
zero = wrap Zero

-- | Write operation
tell :: MonadFree (T w) m => w -> m ()
tell w = wrap (Out w (return ()))

-- | Interpretation of free monad over T as a tree of branching computations
-- with possibility of failure.
evalTreeWriterT :: (Monad m, Monoid w) => TreeWriterT w m a -> m (Maybe (Tree w a))
evalTreeWriterT x = runFT x (return . Just . Tip mempty) ev
    where
      ev Zero = return Nothing
      ev (Plus x1 x2) =
        do { t1 <- x1; t2 <- x2;
             return $ (Fork mempty <$> t1 <*> t2) <|> t1 <|> t2
           }
      ev (Out w y) = do { t <- y; return (tprepend w <$> t) }

evalTreeWriter :: (Monoid w) => TreeWriter w a -> Maybe (Tree w a)
evalTreeWriter = runIdentity . evalTreeWriterT

{------------------------------------------------------------------------------}
{-- Operations on trees --}

tflat :: Tree w a -> [a]
tflat t = go t []
    where go (Tip _ a) xs = a:xs
          go (Fork _ t1 t2) xs = go t1 (go t2 xs)

tflat' :: Maybe (Tree w a) -> [a]
tflat' = maybe [] tflat

{------------------------------------------------------------------------------}
{-- Printing trees, with indentation --}

instance (Show w, Show a) => Show (Tree w a) where
    showsPrec prec t = go prec 0 t
        where
          go p n (Tip w a) = showIndent n .
                             (showParen (p > 10)
                             $ showString "Tip "
                             . showsPrec 11 w
                             . showString " "
                             . showsPrec 11 a)
          go p n (Fork w t1 t2) = showIndent n .
                                  (showParen (p > 10)
                                   $ showString "Fork "
                                   . showsPrec 11 w
                                   . showString "\n     " . showIndent n
                                   . go 11 (n+1) t1
                                   . showString "\n     " . showIndent n
                                   . go 11 (n+1) t2)

          showIndent n = showString (replicate (n*3) ' ')
