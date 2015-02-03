{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module KMC.OutputTerm where

import Control.Applicative hiding (Const)
import Data.Monoid

import KMC.Theories
import KMC.Coding

data Enumerator e dom rng = Enumerator e
data Join f rng = Join [f]
data Const dom rng = Const rng
data f :+: g = Inl f | Inr g

{-
data Func t a b where
  Func   :: t -> Func t (Dom t) (Rng t)
  Id     :: Func t a a
  ConstF :: b -> Func t a b

  Inl    :: Func t a b -> Func t a (Either b c)
  Inr    :: Func t a c -> Func t a (Either b c)
  List   :: [Func t a b] -> Func t a [b]

  Concat :: Func t a [[b]] -> Func t a [b]

instance (Function t) => Function (Func t a b) where
  type Dom (Func t a b) = a
  type Rng (Func t a b) = b

  eval (Func f)     = eval f
  eval Id           = id
  eval (ConstF x)   = const x

  eval (Inl f)      = Left . eval f
  eval (Inr f)      = Right . eval f
  eval (List xs)    = \x -> map (flip eval x) xs
  eval (Concat f)   = concat . eval f

  isConst (Func f)   = isConst f
  isConst Id         = Nothing
  isConst (ConstF x) = Just x
  isConst (Inl f)    = Left <$> isConst f
  isConst (Inr f)    = Right <$> isConst f
  isConst (List xs)  = mapM isConst xs
  isConst (Concat f) = concat <$> isConst f
-}

instance (Function f, Monoid rng, Rng f ~ rng) => Function (Join f rng) where
  type Dom (Join f rng) = Dom f
  type Rng (Join f rng) = rng
  eval (Join fs) x = mconcat $ map (flip eval x) fs
  isConst (Join fs) = mconcat <$> mapM isConst fs

instance (Function f, Function g, Dom f ~ Dom g, Rng f ~ Rng g) => Function (f :+: g) where
  type Dom (f :+: g) = Dom f
  type Rng (f :+: g) = Rng f
  eval (Inl f) x = eval f x
  eval (Inr g) x = eval g x
  isConst (Inl f) = isConst f
  isConst (Inr g) = isConst g

instance Function (Const dom rng) where
  type Dom (Const dom rng) = dom
  type Rng (Const dom rng) = rng
  eval (Const x) = const x
  isConst (Const x) = Just x

instance (Enumerable e dom, Enum rng, Bounded rng) => Function (Enumerator e dom rng) where
  type Dom (Enumerator e dom rng) = dom
  type Rng (Enumerator e dom rng) = [rng]
  eval (Enumerator e) x = codeFixedWidthEnumSized (size e) (indexOf x e)
  isConst (Enumerator e) = if size e == 1 then
                               Just $ eval (Enumerator e) (lookupIndex 0 e)
                           else
                               Nothing
