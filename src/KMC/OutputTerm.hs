{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module KMC.OutputTerm where

import Control.Monad (liftM, liftM2)

import KMC.Theories
import KMC.Coding

data Ident a = Ident deriving (Eq, Ord, Show)
data InList f = InList f deriving (Eq, Ord, Show)
data Enumerator e dom rng = Enumerator e deriving (Eq, Ord, Show)
data Join f rng = Join [f] deriving (Eq, Ord, Show)
data Const dom rng = Const rng deriving (Eq, Ord, Show)
data f :+: g = Inl f | Inr g deriving (Eq, Ord, Show)
data NullFun a b = NullFun deriving (Eq, Ord, Show)
data f :*: g = f :*: g deriving (Eq, Ord, Show)
             
data o :>: f = o :>: f deriving (Eq, Ord, Show)
data f :<: o = f :<: o deriving (Eq, Ord, Show)

type Pre f = Rng f :>: f
type Post f = f :<: Rng f

type WithNull f = f :+: (NullFun (Dom f) (Rng f))

instance (Monoid (Rng f), Function f, o ~ Rng f) => Function (o :>: f) where
    type Dom (o :>: f) = Dom f
    type Rng (o :>: f) = Rng f
    eval (o :>: f) x = o `mappend` eval f x
    isConst (o :>: f) = fmap (mappend o) $ isConst f
    inDom x (_ :>: f) = x `inDom` f
    domain (_ :>: f) = domain f

instance (Monoid (Rng f), Function f, o ~ Rng f) => Function (f :<: o) where
    type Dom (f :<: o) = Dom f
    type Rng (f :<: o) = Rng f
    eval (f :<: o) x  = eval f x `mappend` o
    isConst (f :<: o) = fmap (flip mappend o) $ isConst f
    inDom x (f :<: _) = x `inDom` f
    domain (f :<: _) = domain f
             
instance (Function f, Function g) => Function (f :*: g) where
    type Dom (f :*: g) = (Dom f, Dom g)
    type Rng (f :*: g) = (Rng f, Rng g)
    eval (f :*: g) (x, y)  = (eval f x, eval g y)
    isConst (_ :*: _)      = Nothing
    inDom (x, y) (f :*: g) = (x `inDom` f) && (y `inDom` g)
    domain (f :*: g) = zip (domain f) (domain g)

instance (Function f) => Function (Maybe f) where
    type Dom (Maybe f) = Maybe (Dom f)
    type Rng (Maybe f) = Maybe (Rng f)
    eval f x  = liftM2 eval f x
    isConst f = liftM isConst f
    inDom x f = maybe False id $ liftM2 inDom x f
    domain (Just f) = map Just (domain f)
    domain Nothing  = []
   
instance (Monoid b) => Function (NullFun a b) where
    type Dom (NullFun a b) = a
    type Rng (NullFun a b) = b
    eval NullFun _  = mempty
    isConst NullFun = Just mempty
    inDom _ NullFun = True
    domain NullFun  = []

instance (Enum a, Bounded a) => Function (Ident a) where
  type Dom (Ident a) = a
  type Rng (Ident a) = a
  eval Ident = id
  isConst Ident = Nothing
  inDom _ _ = True
  domain Ident = [minBound .. maxBound]

instance (Function f) => Function (InList f) where
  type Dom (InList f) = Dom f
  type Rng (InList f) = [Rng f]
  eval (InList f) x = [eval f x]
  isConst (InList f) = (:[]) <$> isConst f
  inDom x (InList f) = inDom x f
  domain (InList f) = domain f

instance (Function f, Monoid rng, Rng f ~ rng) => Function (Join f rng) where
  type Dom (Join f rng) = Dom f
  type Rng (Join f rng) = rng
  eval (Join fs) x = mconcat $ map (flip eval x) fs
  isConst (Join fs) = mconcat <$> mapM isConst fs
  inDom x (Join fs) = all (inDom x) fs
  domain (Join fs) = mconcat $ map domain fs

instance (Function f, Function g, Dom f ~ Dom g, Rng f ~ Rng g) => Function (f :+: g) where
  type Dom (f :+: g) = Dom f
  type Rng (f :+: g) = Rng f
  eval (Inl f) x = eval f x
  eval (Inr g) x = eval g x
  isConst (Inl f) = isConst f
  isConst (Inr g) = isConst g
  inDom x (Inl f) = inDom x f
  inDom x (Inr g) = inDom x g
  domain (Inl f) = domain f
  domain (Inr g) = domain g

instance Function (Const dom rng) where
  type Dom (Const dom rng) = dom
  type Rng (Const dom rng) = rng
  eval (Const x) = const x
  isConst (Const x) = Just x
  inDom _ _ = True
  domain (Const _) = []

instance (Enumerable e dom, Enum rng, Bounded rng, Num dom, Enum dom) => Function (Enumerator e dom rng) where
  type Dom (Enumerator e dom rng) = dom
  type Rng (Enumerator e dom rng) = [rng]
  eval (Enumerator e) x = codeFixedWidthEnumSized (size e) (indexOf x e)
  isConst (Enumerator e) = if size e == 1 then
                               Just $ eval (Enumerator e) (lookupIndex 0 e)
                           else
                               Nothing
  inDom x (Enumerator e) = member x e
  domain (Enumerator e)  = [lookupIndex i e | i <- [0 .. size e]]

