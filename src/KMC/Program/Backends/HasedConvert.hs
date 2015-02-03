{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module KMC.Program.Backends.HasedConvert where

import           Control.Monad.State
import qualified Data.Map as M
import           Data.Word (Word8)
import           Data.Char (ord)
import           Data.Maybe (fromJust)

import           KMC.Theories
import           KMC.RangeSet
import           KMC.Expression (Mu(..))
import           KMC.Syntax.External (Regex(..))
import           KMC.OutputTerm
import qualified KMC.Program.Backends.HasedParser as H

-- import System.IO.Unsafe

data Nat = Z | S Nat deriving (Eq, Ord, Show)
data Skip = Skip | NoSkip deriving (Eq, Ord, Show)
data SimpleMu = SMVar Nat
              | SMLoop SimpleMu
              | SMAlt SimpleMu SimpleMu
              | SMSeq SimpleMu SimpleMu
              | SMWrite String SimpleMu
              | SMRegex Skip Regex
              | SMAccept
  deriving (Eq, Ord, Show)

-- | These mu-terms either output the identity on the input character,
-- injected into a list, or the output a constant list of characters.
type HasedOutTerm = Either (InList (Identity Word8)) (ConstFunction [Word8])

type HasedMu a = Mu (RangeSet Word8) HasedOutTerm [Word8] a

-- | The term that copies the input char to output.
copyInput :: HasedOutTerm
copyInput = Left (InList Identity)

-- | The term that outputs a fixed string (list of Word8).
out :: [Word8] -> HasedOutTerm
out = Right . ConstFunction

out' :: String -> HasedOutTerm
out' = out . map char2word

-- | Get the index of an element in a list, or Nothing.
pos :: (Eq a) => a -> [a] -> Maybe Nat
pos _ []                 = Nothing
pos x (e:es) | x == e    = return Z
pos x (_:es) | otherwise = S `fmap` (pos x es)

-- | Get the nth element on the stack, or Nothing.
getStack :: Nat -> [a] -> Maybe a
getStack Z     (e : _)  = Just e
getStack (S n) (_ : es) = getStack n es
getStack _ _            = Nothing

-- | Converts a Hased AST which consists of a set of terms bound to variables
-- to one simplified mu term, with the terms inlined appropriately.
-- The given identifier is treated as the top-level bound variable,
-- i.e., it becomes the first mu.  
toSimpleMu :: H.Identifier -> H.Hased -> SimpleMu 
toSimpleMu i (H.Hased ass) = SMLoop $ go [i] (fromJust $ M.lookup i mp)
    where
      mp = M.fromList (map (\(H.HA (k,v)) -> (k, v)) ass)
      go :: [H.Identifier] -> H.HasedTerm -> SimpleMu
      go vars (H.Constructor n ts) = SMWrite n (foldr1 SMSeq $ map (go vars) ts)
      go vars (H.Var name) =
          case name `pos` vars of
            Nothing ->
                case M.lookup name mp of
                  Nothing -> error $ "Name not found: " ++ show name
                  Just t  -> SMLoop $ go (name : vars) t
            Just p  -> SMVar p
      go vars (H.Sum l r) = SMAlt (go vars l) (go vars r)
      go _ (H.RE re) = SMRegex NoSkip re
      go _ (H.Skip re) = SMRegex Skip re
      go _ (H.NamedArg _ _) = error "NamedArg not supported!"

char2word :: Char -> Word8
char2word = toEnum . ord

toMu :: SimpleMu -> HasedMu a
toMu = flip simpleToComplexMu' []

hasedToMuTerm :: (H.Identifier, H.Hased) -> HasedMu a
hasedToMuTerm (i, h) = toMu $ toSimpleMu i h

simpleToComplexMu' :: SimpleMu -> [HasedMu a] -> HasedMu a
simpleToComplexMu' (SMVar n) st   = maybe (error "stack exceeded") id $ getStack n st
simpleToComplexMu' (SMLoop sm) st = Loop $ \x -> simpleToComplexMu' sm ((Var x) : st)
simpleToComplexMu' (SMAlt l r) st = Alt (simpleToComplexMu' l st) (simpleToComplexMu' r st)
simpleToComplexMu' (SMSeq l r) st = Seq (simpleToComplexMu' l st) (simpleToComplexMu' r st)
simpleToComplexMu' (SMWrite s e) st = W (map char2word s) (simpleToComplexMu' e st)
simpleToComplexMu' (SMRegex Skip re) _ = fromRegex (out []) re
simpleToComplexMu' (SMRegex NoSkip re) _ = fromRegex copyInput re
simpleToComplexMu' SMAccept _ = Accept

fromRegex :: HasedOutTerm -> Regex -> HasedMu a
fromRegex _ One            = Accept
fromRegex o Dot            = RW top o Accept
fromRegex o (Chr a)        = RW (singleton (char2word a)) o Accept
fromRegex o (Group _ e)    = fromRegex o e
fromRegex o (Concat e1 e2) = Seq (fromRegex o e1) (fromRegex o e2)
fromRegex o (Branch e1 e2) = Alt (fromRegex o e1) (fromRegex o e2)
fromRegex o (Class b rs)   = RW rs' o Accept
    where rs' = (if b then id else complement) $
                rangeSet [(toEnum (ord lo), toEnum (ord hi)) | (lo, hi) <- rs]
fromRegex o (Star e)       = Loop $ \x -> Alt (Seq (fromRegex o e) (Var x)) Accept
fromRegex o (LazyStar e)   = Loop $ \x -> Alt Accept (Seq (fromRegex o e) (Var x))
fromRegex o (Plus e)       = Seq (fromRegex o e) (fromRegex o (Star e))
fromRegex o (LazyPlus e)   = Seq (fromRegex o e) (fromRegex o (LazyStar e))
fromRegex o (Question e)   = Alt (fromRegex o e) Accept
fromRegex o (LazyQuestion e) = Alt Accept (fromRegex o e)
fromRegex _ (Suppress e)    = fromRegex (out []) e
fromRegex _ (NamedSet _ _) = error "Named sets not yet supported"
fromRegex _ (Range _ _ _)  = error "Ranges not yet supported"
fromRegex _ (LazyRange _ _ _) = error "Lazy ranges not yet supported"


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

test1 = simplify (H.Identifier "x") $ unsafePerformIO $ H.pf' H.t4


sstFromHased :: String
             -> SST (PathTree Var Int)
                    (RangeSet Word8)
                    HasedOutTerm
                    Var
                    Bool
sstFromHased str =
  case H.parseHased str of
    Left e -> error e
    Right (ident, h) -> sstFromFST $ fromMu $ hasedToMuTerm (ident, h)
