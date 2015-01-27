{-# LANGUAGE DeriveFunctor #-}
module OutputTerm where

--import qualified Data.Map as M

data Term bool b = Const b
                 | Code bool
  deriving (Functor, Show, Eq, Ord)

newtype OutputTerm bool b = OutputTerm { unpack :: [Term bool b] }
  deriving (Functor)

data ConstFunction a = ConstFunction a
  deriving (Show, Eq, Ord)

instance Monad (OutputTerm bool) where
    return x = OutputTerm [Const x]
    t >>= f = subst t f

subst :: OutputTerm bool a -> (a -> OutputTerm bool b) -> OutputTerm bool b
subst (OutputTerm xs) sigma = OutputTerm $ concatMap aux xs
    where
      aux (Const a) = unpack $ sigma a
      aux (Code x) = [Code x]
