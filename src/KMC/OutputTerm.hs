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

data Ident a = Ident deriving (Eq, Ord, Show)
data InList f = InList f deriving (Eq, Ord, Show)
data Enumerator e dom rng = Enumerator e deriving (Eq, Ord, Show)
data Join f rng = Join [f] deriving (Eq, Ord, Show)
data Const dom rng = Const rng deriving (Eq, Ord, Show)
data f :+: g = Inl f | Inr g deriving (Eq, Ord, Show)

instance Function (Ident a) where
  type Dom (Ident a) = a
  type Rng (Ident a) = a
  eval Ident = id
  isConst Ident = Nothing
  inDom _ _ = True

instance (Function f) => Function (InList f) where
  type Dom (InList f) = Dom f
  type Rng (InList f) = [Rng f]
  eval (InList f) x = [eval f x]
  isConst (InList f) = (:[]) <$> isConst f
  inDom x (InList f) = inDom x f

instance (Function f, Monoid rng, Rng f ~ rng) => Function (Join f rng) where
  type Dom (Join f rng) = Dom f
  type Rng (Join f rng) = rng
  eval (Join fs) x = mconcat $ map (flip eval x) fs
  isConst (Join fs) = mconcat <$> mapM isConst fs
  inDom x (Join fs) = all (inDom x) fs

instance (Function f, Function g, Dom f ~ Dom g, Rng f ~ Rng g) => Function (f :+: g) where
  type Dom (f :+: g) = Dom f
  type Rng (f :+: g) = Rng f
  eval (Inl f) x = eval f x
  eval (Inr g) x = eval g x
  isConst (Inl f) = isConst f
  isConst (Inr g) = isConst g
  inDom x (Inl f) = inDom x f
  inDom x (Inr g) = inDom x g

instance Function (Const dom rng) where
  type Dom (Const dom rng) = dom
  type Rng (Const dom rng) = rng
  eval (Const x) = const x
  isConst (Const x) = Just x
  inDom _ _ = True

instance (Enumerable e dom, Enum rng, Bounded rng) => Function (Enumerator e dom rng) where
  type Dom (Enumerator e dom rng) = dom
  type Rng (Enumerator e dom rng) = [rng]
  eval (Enumerator e) x = codeFixedWidthEnumSized (size e) (indexOf x e)
  isConst (Enumerator e) = if size e == 1 then
                               Just $ eval (Enumerator e) (lookupIndex 0 e)
                           else
                               Nothing
  inDom x (Enumerator e) = member x e
