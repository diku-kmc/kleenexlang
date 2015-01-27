{-# LANGUAGE DeriveFunctor #-}
module OutputTerm where

import qualified Data.Map as M

data Term bool b = Const b
                 | Code bool
  deriving (Show, Eq, Ord)

newtype OutputTerm bool b = OutputTerm { unpack :: [Term bool b] }

data ConstFunction a = ConstFunction a
  deriving (Show, Eq, Ord)

subst :: OutputTerm bool a -> (a -> OutputTerm bool b) -> OutputTerm bool b
subst (OutputTerm xs) sigma = OutputTerm $ concatMap aux xs
    where
      aux (Const a) = unpack $ sigma a
      aux (Code x) = [Code x]

newtype Var = Var Int
  deriving (Eq, Ord, Show)

-- | Every closed function is also a function with free variables.
inj :: OutputTerm bool a -> OutputTerm bool (Either Var a)
inj t = subst t $ \x -> OutputTerm [Const (Right x)]

type Update bool a = M.Map Var (OutputTerm bool (Either Var a))

fromUpdate :: Update bool a -> Var -> OutputTerm bool (Either Var a)
fromUpdate upd v = maybe (OutputTerm [Const (Left v)]) id (M.lookup v upd)

update' :: OutputTerm bool (Either Var a) -> Update bool a -> OutputTerm bool (Either Var a)
update' t upd = update t (fromUpdate upd)

update :: OutputTerm bool (Either Var a) -> (Var -> OutputTerm bool (Either Var a)) -> OutputTerm bool (Either Var a)
update t rho = subst t $ \c -> case c of
                                 Left v -> rho v
                                 Right x -> OutputTerm [Const (Right x)]
