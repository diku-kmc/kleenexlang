{-# LANGUAGE DeriveFunctor #-}
module KMC.OutputTerm where

import Data.Monoid

data Term bool b = Const b
                 | Code bool
  deriving (Functor, Show, Eq, Ord)

newtype OutputTerm bool b = OutputTerm { unpack :: [Term bool b] }
  deriving (Show, Functor)

data ConstFunction a = ConstFunction a
  deriving (Show, Eq, Ord)

data Identity a = Identity
  deriving (Show, Eq, Ord)

data InList a = InList a
  deriving (Show, Eq, Ord)

data Inl a = Inl a
  deriving (Show, Eq, Ord)

data Inr a = Inr a
  deriving (Show, Eq, Ord)

type Term' a = Either (InList (Identity a)) (ConstFunction [a])

test_t1 :: Term' Int
test_t1 = Left (InList Identity)

test_t2 :: Term' Int
test_t2 = Right (ConstFunction [42])

instance Monad (OutputTerm bool) where
    return x = OutputTerm [Const x]
    t >>= f = subst t f

instance Monoid (OutputTerm bool a) where
    mempty = OutputTerm []
    mappend (OutputTerm xs) (OutputTerm ys) = OutputTerm $ xs ++ ys

subst :: OutputTerm bool a -> (a -> OutputTerm bool b) -> OutputTerm bool b
subst (OutputTerm xs) sigma = OutputTerm $ concatMap aux xs
    where
      aux (Const a) = unpack $ sigma a
      aux (Code x) = [Code x]
