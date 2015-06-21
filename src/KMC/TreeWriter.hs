{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module KMC.TreeWriter
       (Tree(..)
       ,TreeWriterT
       ,TreeWriter
       ,mapOutput
       ,branch
       ,zero
       ,tell
       ,tflat
       ,evalTreeWriterT
       ,evalTreeWriter
       )
where

import Control.Monad.Trans.Free.Church
import Control.Monad.Identity

data Tree w a = Tip { tOutput :: w, tValue :: a }
              | Fork { tOutput :: w
                     , tForks :: [Tree w a] }
  deriving (Eq, Ord)

instance Functor (Tree w) where
  fmap f (Tip w x) = Tip w (f x)
  fmap f (Fork w xs) = Fork w (map (fmap f) xs)

mapOutput :: (w -> w') -> Tree w a -> Tree w' a
mapOutput f (Tip w a) = Tip (f w) a
mapOutput f (Fork w xs) = Fork (f w) (map (mapOutput f) xs)

tprepend :: (Monoid w) => w -> Tree w a -> Tree w a
tprepend w (Tip w' x) = Tip (mappend w w') x
tprepend w (Fork w' xs) = Fork (mappend w w') xs

data T w a = Out w a
           | Sum [a]
  deriving (Functor)

type TreeWriterT w m = FT (T w) m
type TreeWriter w = TreeWriterT w Identity

branch :: MonadFree (T w) m => [m a] -> m a
branch xs = wrap (Sum xs)

zero :: MonadFree (T w) m => m a
zero = wrap (Sum [])

tell :: MonadFree (T w) m => w -> m ()
tell w = wrap (Out w (return ()))

evalTreeWriterT :: (Monad m, Monoid w) => TreeWriterT w m a -> m [Tree w a]
evalTreeWriterT twt = runFT twt (\a -> return [Tip mempty a]) ev
  where
    ev :: (Monoid w, Monad m) => T w (m [Tree w a]) -> m [Tree w a]
    ev (Out w y) = map (tprepend w) <$> y
    ev (Sum ys) = do
      ts <- sequence ys
      case concat ts of
       [] -> return []
       [x] -> return [x]
       xs -> return [Fork mempty xs]

evalTreeWriter :: Monoid w => TreeWriter w a -> [Tree w a]
evalTreeWriter = runIdentity . evalTreeWriterT

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
