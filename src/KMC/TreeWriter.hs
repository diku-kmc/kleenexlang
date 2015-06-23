{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module KMC.TreeWriter
       (Tree(..)
       ,TreeWriterT
       ,TreeWriter
       ,mapOutput
       ,branch
       ,zero
       ,tell
       ,tflat
       ,runTreeWriterT
       ,runTreeWriter
       )
where

import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.State
import Data.Maybe (catMaybes)

data Tree w a = Tip { tOutput :: w, tValue :: a }
              | Fork { tOutput :: w
                     , tForks :: [Tree w a] }
  deriving (Eq, Ord)

instance Functor (Tree w) where
  fmap f (Tip w x) = Tip w (f x)
  fmap f (Fork w xs) = Fork w (map (fmap f) xs)

newtype TreeWriterT w m a = TreeWriterT { runTreeWriterT :: m (Maybe (Tree w a)) }
  deriving (Functor)

type TreeWriter w a = TreeWriterT w Identity a

mapOutput :: (w -> w') -> Tree w a -> Tree w' a
mapOutput f (Tip w a) = Tip (f w) a
mapOutput f (Fork w xs) = Fork (f w) (map (mapOutput f) xs)

tprepend :: (Monoid w) => w -> Tree w a -> Tree w a
tprepend w (Tip w' x) = Tip (mappend w w') x
tprepend w (Fork w' xs) = Fork (mappend w w') xs

zero :: Monad m => TreeWriterT w m a
zero = TreeWriterT $ return Nothing

tell :: Monad m => w -> TreeWriterT w m ()
tell w = TreeWriterT $ return $ Just $ Tip w ()

branch :: (Monad m, Monoid w) => [TreeWriterT w m a] -> TreeWriterT w m a
branch ts = TreeWriterT $ do
  ts' <- mapM runTreeWriterT ts
  case catMaybes ts' of
   [] -> return Nothing
   [t] -> return (Just t)
   ts'' -> return (Just (Fork mempty ts''))

{------------------------------------------------------------------------------}
{-- Evaluation --}

returnTreeWriterT :: (Monoid w, Monad m) => a -> TreeWriterT w m a
returnTreeWriterT a = TreeWriterT $ return (Just $ Tip mempty a)

joinTreeWriterT :: (Monoid w, Monad m) => TreeWriterT w m (TreeWriterT w m a) -> TreeWriterT w m a
joinTreeWriterT (TreeWriterT twt) = TreeWriterT $ do
  y <- twt
  case y of
   Nothing -> return Nothing
   Just t -> flatten t
  where
    flatten (Tip w (TreeWriterT x)) = do
      mx <- x
      case mx of
       Nothing -> return Nothing
       Just t -> return $ Just (tprepend w t)
    flatten (Fork w ys) = do
      ts <- catMaybes <$> mapM flatten ys
      case ts of
       [] -> return Nothing
       [t] -> return $ Just (tprepend w t)
       ts' -> return $ Just (Fork w ts')

runTreeWriter :: Monoid w => TreeWriterT w Identity a -> Maybe (Tree w a)
runTreeWriter = runIdentity . runTreeWriterT

{------------------------------------------------------------------------------}
{-- Instances --}

instance (Monoid w, Monad m) => Applicative (TreeWriterT w m) where
  pure = returnTreeWriterT
  t1 <*> t2 = joinTreeWriterT (fmap (\f -> fmap f t2) t1)

instance (Monoid w, Monad m) => Monad (TreeWriterT w m) where
  return = returnTreeWriterT
  x >>= f = joinTreeWriterT $ fmap f x

instance (Monoid w) => MonadTrans (TreeWriterT w) where
  lift x = TreeWriterT $ x >>= \a -> return (Just (Tip mempty a))

instance (Monoid w, Monad (TreeWriterT w m), MonadState s m) => MonadState s (TreeWriterT w m) where
  get = lift get
  put = lift . put

{------------------------------------------------------------------------------}
{-- Operations on trees --}

tflat :: Tree w a -> [a]
tflat t = go t []
  where
    go (Tip _ a) = (a:)
    go (Fork _ ts) = flip (foldr go) ts

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
          go p n (Fork w ts) =
            showIndent n .
            (showParen (p > 10)
             $ showString "Fork "
             . showsPrec 11 w
             . foldr (\t' sf -> showString "\n     "
                               . showIndent n
                               . go 11 (n+1) t'
                               . sf) id ts)

          showIndent n = showString (replicate (n*3) ' ')
