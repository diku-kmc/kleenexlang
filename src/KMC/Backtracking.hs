{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module KMC.Backtracking
       ( Stream(..)
       , eof
       , lit
       , litp
       , barrier
       , fBarrier
       , next
       , P
       , runP
       , runP'
       ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import           Data.Word (Word8)

class Stream s a | s -> a where
  uncons :: s -> Maybe (a, s)
  index :: s -> Int
  nullStream :: s -> Bool
  nullStream = maybe True (const False) . uncons

instance Stream (Int, L.ByteString) Word8 where
  uncons (k, b) = case L.uncons b of
    Nothing -> Nothing
    Just (a, b') -> Just (a, (k+1, b'))
  index (k, _) = k

instance Stream (Int, B.ByteString) Word8 where
  uncons (k, b) = case B.uncons b of
    Nothing -> Nothing
    Just (a, b') -> Just (a, (k+1, b'))
  index (k, _) = k

-- | A Name serves as a unique identifier for a program position
type Name = Int

-- | An Index serves as a unique position in the input
type Index = Int

-- | Marks associates each program position with a set of input indices.
type Marks = M.IntMap S.IntSet

mark :: Name -> Index -> Marks -> Marks
mark p i = M.insertWith' S.union p (S.singleton i)

isMarked :: Name -> Index -> Marks -> Bool
isMarked p i m = maybe False (S.member i) (M.lookup p m)

-- | An input is conceptually just a [sigma], but for performance reasons we
-- maintain an Index to quickly compare input positions.
--type Input sigma = (Index, [sigma])

-- | CPS monad from http://www-ps.informatik.uni-kiel.de/~sebf/data/pub/atps09.pdf
newtype CPS c a = CPS { (>>-) :: forall b. (a -> c b) -> c b }

-- | CPS is a monad
instance Monad (CPS c) where
    return x = CPS ($ x)
    a >>= f  = CPS $ \c -> a >>- \x -> f x >>- c

-- | If the computation has a "yield" operation, we can run the monad
class Computation c where
  yield :: a -> c a

runCPS :: (Computation c) => CPS c a -> c a
runCPS a = a >>- yield

-- | Functor and applicative instances
instance Functor (CPS c) where
    fmap f a = CPS $ \c -> a >>- (c . f)

instance Applicative (CPS c) where
    pure x  = CPS ($ x)
    a <*> b = CPS $ \c -> a >>- \f -> b >>- (c . f)

-- | A computation is non-deterministic if it supports failure and choice.
class Nondet n where
    failure :: n a
    choice :: n a -> n a -> n a

-- | CPS over non-deterministic computations is a MonadPlus
instance (Nondet n) => MonadPlus (CPS n) where
    mzero     = CPS $ const failure
    mplus a b = CPS $ \c -> choice (a >>- c) (b >>- c)

-- | Alternative instance
instance (Nondet n) => Alternative (CPS n) where
    empty     = CPS $ const failure
    a <|> b   = CPS $ \c -> choice (a >>- c) (b >>- c)

-- | We want to be able to stop backtracking early by maintaining extra state in
-- the form of a Marks set. Additionally, we only want to be able to update our
-- marks set only when a computation backtracks, and not when it succeeds. To
-- accomplish this, we introduce MarkLists, which are simply regular lists with a
-- Marks set at the end. In the finite case, this is equivalent to ([a], Marks)
-- and hence is not that interesting. However, when we represent MarkLists as
-- diff lists, the representation gets more interesting.
data MarkList a = Nil Marks
                | a :. (MarkList a)
  deriving (Show)
infixr 1 :.

-- | A diff list is a list with a hole in it. For regular lists, diff lists have
-- the type [a] -> [a]. For an example, consider the diff list \x -> 1:2:3:x,
-- which stuffs any x to the end of the list [1,2,3]. Diff lists enjoy constant
-- time append, which makes for an efficient backtracking monad.
--
-- What about MarkLists with holes in them? These are functions such as
-- \l -> 1:2:3:(l m), where m is a Markset. The type becomes

type DiffMarkList a = (Marks -> MarkList a) -> MarkList a

-- | The (Marks -> MarkList a) argument in the type of DiffMarkList can be thought
-- of as a failure continuation. We now have everything needed to define the
-- computation type for our backtracking monad:
newtype Comp s a =
  Comp { unComp :: s -> Marks -> DiffMarkList (a, s) }

-- The following is definitions of various operations for constructing and
-- working with diff mark lists.

{-
-- Not actually used:

dfromList :: ([a], Marks) -> DiffMarkList a
dfromList (xs, m) k = foldr (:.) (k m) xs
-}

dtoMarkList :: DiffMarkList a -> MarkList a
dtoMarkList l = l Nil

dnil :: Marks -> DiffMarkList a
dnil m k = k m

dcons :: a -> DiffMarkList a -> DiffMarkList a
dcons x l k = x :. l k

-- It is inefficient to define a monad instance on Comp directly, since the
-- definition of bind requires us to go back and forth between MarkList and
-- DiffMarkList, defeating the purpose of the latter. We can get a Monad
-- instance via CPS, however, which just requires us to give instances of
-- Computation and Nondet.

-- The type ``CPS (Comp sigma) a'' is equivalent to the type
--    forall b. (a -> Comp sigma b) -> Comp sigma b
-- which is equivalent to
--    forall b. (a -> Input sigma -> Marks -> (Marks -> MarkList b) -> MarkList b)
--              -> Input sigma -> Marks -> (Marks -> MarkList b) -> MarkList b


instance Computation (Comp s) where
  yield x = Comp $ \iw m -> dcons (x, iw) (dnil m)

instance Nondet (Comp s) where
  failure = Comp $ \_iw m -> dnil m
  choice (Comp c1) (Comp c2) = Comp $ \iw m kfail ->
    c1 iw m $ \m' -> c2 iw m' kfail

-- | Read next symbol and tick; or fail
next :: (Stream s a) => CPS (Comp s) a
next = CPS $ \k -> Comp $ \w ->
         case uncons w of
           Nothing      -> dnil
           Just (a, w') -> unComp (k a) w'

-- | Check barrier and set it on backtracking
barrier :: (Stream s a) => Name -> CPS (Comp s) ()
barrier x = CPS $ \k -> Comp $ \w m ->
  if isMarked x (index w) m then
      dnil m
  else
      \lnext -> unComp (k ()) w m (lnext . mark x (index w))

-- | Check barrier and set it in the future
fBarrier :: (Stream s a) => Name -> CPS (Comp s) ()
fBarrier x = CPS $ \k -> Comp $ \w m ->
  if isMarked x (index w) m then
      dnil m
  else
      unComp (k ()) w (mark x (index w) m)

-- | Assert that the next symbol is equal to the given one. Tick on succes; fail otherwise.
lit :: (Stream s a, Eq a) => a -> CPS (Comp s) a
lit a = CPS $ \k -> Comp $ \w ->
         case uncons w of
           Just (a', w') | a == a' -> unComp (k a) w'
           _                       -> dnil

-- | Generalized version of `lit` taking a predicate.
litp :: (Stream s a) => (a -> Bool) -> CPS (Comp s) a
litp p = CPS $ \k -> Comp $ \w ->
  case uncons w of
    Just (a', w') | p a' -> unComp (k a') w'
    _                    -> dnil

-- | Assert that there are no more symbols available. Fail otherwise.
eof :: (Stream s a) => CPS (Comp s) ()
eof = CPS $ \k -> Comp $ \w -> if nullStream w then unComp (k ()) w else dnil

-- | Parser monad
type P s = CPS (Comp s)

-- | Run a parser on the given input
runP :: P s a -> s -> MarkList (a, s)
runP c inp = dtoMarkList (unComp (runCPS c) inp M.empty)

-- | Throw away all results but the first one.
runP' :: P s a -> s -> Maybe a
runP' c inp = case runP c inp of
  (x,_) :. _ -> Just x
  _          -> Nothing
