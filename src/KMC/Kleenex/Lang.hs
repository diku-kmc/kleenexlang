{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module KMC.Kleenex.Lang where

import qualified Data.Map as M
import           Data.Word (Word8)
import           Data.ByteString (unpack, ByteString)
import           Data.Char (chr, ord)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)

import           KMC.Coding (codeFixedWidthEnumSized, decodeEnum)
import           KMC.Expression (Mu (..))
import qualified KMC.Kleenex.Parser as H
import           KMC.OutputTerm (Const(..), InList(..), Ident(..), (:+:)(..))
import           KMC.RangeSet (singleton, complement, rangeSet, union, RangeSet)
import           KMC.Syntax.External (Regex (..), unparse)
import           KMC.Theories (top)
import           KMC.Visualization (Pretty(..))

toChar :: (Enum a, Bounded a) => [a] -> Char
toChar = chr . decodeEnum

{-- Intermediate internal data type for the mu-terms.  These use de Bruijn-indices. --}
data Nat = Z | S Nat      deriving (Eq, Ord, Show)
data SimpleMu = SMVar Nat
              | SMLoop SimpleMu
              | SMAlt SimpleMu SimpleMu
              | SMSeq SimpleMu SimpleMu
              | SMWrite ByteString
              | SMRegex Regex
              | SMIgnore SimpleMu
              | SMAccept
  deriving (Eq, Ord)
nat2int :: Nat -> Int
nat2int Z = 0
nat2int (S n) = 1 + nat2int n
instance Show SimpleMu where
    show (SMVar n) = show (nat2int n)
    show (SMLoop e) = "μ.(" ++ show e ++ ")"
    show (SMAlt l r) = "(" ++ show l ++ ")+(" ++ show r ++ ")"
    show (SMSeq l r) = show l ++ show r
    show (SMWrite s) = show s
    show (SMRegex r) = "<" ++ unparse r ++ ">"
    show (SMIgnore s) = "skip:[" ++ show s ++ "]"
    show SMAccept = "1"

-- | Mu-terms created from Kleenex programs either output the identity on
-- the input character, injected into a list, or the output a constant list
-- of characters.
type KleenexOutTerm = (InList (Ident Word8)) :+: (Const Word8 [Word8])
instance Pretty KleenexOutTerm where
    pretty (Inl (InList _)) = "COPY"
    pretty (Inr (Const [])) = "SKIP"
    pretty (Inr (Const ws)) = "\"" ++ map toChar [ws] ++ "\""

type KleenexMu a = Mu (RangeSet Word8) KleenexOutTerm a

-- | The term that copies the input char to output.
copyInput :: KleenexOutTerm
copyInput = Inl (InList Ident)

-- | The term that outputs a fixed string (list of Word8).
out :: [Word8] -> KleenexOutTerm
out = Inr . Const

-- | Get the index of an element in a list, or Nothing.
pos :: (Eq a) => a -> [a] -> Maybe Nat
pos _ []                 = Nothing
pos x (e:_)  | x == e    = return Z
pos x (_:es) | otherwise = S `fmap` (pos x es)

-- | Get the nth element on the stack, or Nothing.
getStack :: Nat -> [a] -> Maybe a
getStack Z     (e : _)  = Just e
getStack (S n) (_ : es) = getStack n es
getStack _ _            = Nothing

-- | Converts a Kleenex AST which consists of a set of terms bound to variables
-- to one simplified mu term, with the terms inlined appropriately.
-- The given identifier is treated as the top-level bound variable,
-- i.e., it becomes the first mu.  
kleenexToSimpleMu :: H.Identifier -> H.Kleenex -> SimpleMu 
kleenexToSimpleMu initVar (H.Kleenex ass) = SMLoop $ go [initVar] (fromJust $ M.lookup initVar mp)
    where
      mp :: M.Map H.Identifier H.KleenexTerm
      mp = M.fromList (map (\(H.HA (k, v)) -> (k, v)) ass)
      
      go :: [H.Identifier] -> H.KleenexTerm -> SimpleMu
      go _    (H.Constant n) = SMWrite n
      go vars (H.Var name) =
          case name `pos` vars of
            Nothing ->
                case M.lookup name mp of
                  Nothing -> error $ "Name not found: " ++ show name
                  Just t  -> SMLoop $ go (name : vars) t
            Just p  -> SMVar p
      go vars (H.Sum l r) = SMAlt (go vars l) (go vars r)
      go vars (H.Seq l r) = SMSeq (go vars l) (go vars r)
      go _    H.One       = SMAccept
      go vars (H.Ignore e) = SMIgnore $ go vars e
      go _ (H.RE re) = SMRegex re

-- | A simple mu term is converted to a "real" mu term by converting the
-- de Bruijn-indexed variables to Haskell variables, and encoding the mu
-- abstractions as Haskell functions.  This function therefore converts
-- a de Bruijn representation to a HOAS representation.  All embedded
-- regular expressions are also represented as mu-terms.
simpleMuToMuTerm :: [KleenexMu a] -> Bool -> SimpleMu -> KleenexMu a
simpleMuToMuTerm st ign sm =
    case sm of
      SMVar n      -> maybe (error "stack exceeded") id $ getStack n st
      SMLoop sm'   -> Loop $ \x -> simpleMuToMuTerm ((Var x) : st) ign sm'
      SMAlt l r    -> (simpleMuToMuTerm st ign l) `Alt` (simpleMuToMuTerm st ign r)
      SMSeq l r    -> (simpleMuToMuTerm st ign l) `Seq` (simpleMuToMuTerm st ign r)
      SMWrite s    -> if ign
                      then W [] Accept
                      else W (unpack s) Accept
      SMRegex re   -> if ign
                      then regexToMuTerm (out []) re
                      else regexToMuTerm copyInput re
      SMIgnore sm' -> simpleMuToMuTerm st True sm'
      SMAccept     -> Accept

-- | Convert a Kleenex program to a mu-term that encodes the string transformation
-- expressed in Kleenex.
kleenexToMuTerm :: (H.Identifier, H.Kleenex) -> KleenexMu a
kleenexToMuTerm (i, h) = simpleMuToMuTerm [] False $ kleenexToSimpleMu i h

encodeChar :: Char -> [Word8]
encodeChar = unpack . encodeUtf8 . T.singleton

-- | Translates a regular expression into a mu-term that performs the given
-- action on the matched symbols: It either copies any matched symbols or
-- ignores them.
regexToMuTerm :: KleenexOutTerm -> Regex -> KleenexMu a
regexToMuTerm o re =
    case re of
       One        -> Accept
       Dot        -> RW top o Accept
       Chr a      -> foldr1 Seq $ map (\c -> RW (singleton c) o Accept) (encodeChar a)
       Group _ e  -> regexToMuTerm o e
       Concat l r -> (regexToMuTerm o l) `Seq` (regexToMuTerm o r)
       Branch l r -> (regexToMuTerm o l) `Alt` (regexToMuTerm o r)
       Class b rs ->
           let rs' = (if b then id else complement) $
                     rangeSet [ (toEnum (ord lo), toEnum (ord hi)) |
                                (lo, hi) <- rs ]
           in RW rs' o Accept
       Star e     -> Loop $ \x -> ((regexToMuTerm o e) `Seq` (Var x)) `Alt` Accept
       LazyStar e -> Loop $ \x -> Accept `Alt`
                                    ((regexToMuTerm o e) `Seq` (Var x))
       Plus e     -> (regexToMuTerm o e) `Seq`
                                 (regexToMuTerm o (Star e))
       LazyPlus e -> (regexToMuTerm o e) `Seq`
                                 (regexToMuTerm o (LazyStar e))
       Question e -> (regexToMuTerm o e) `Alt` Accept
       LazyQuestion e -> Accept `Alt` (regexToMuTerm o e)
       Suppress e -> regexToMuTerm (out []) e
       Range e n m ->
           let start expr = case m of
                             Nothing -> expr `Seq` (regexToMuTerm o (Star e))
                             Just n' -> expr `Seq` (foldr Seq Accept
                                                    (replicate (n' - n)
                                                    (regexToMuTerm o (Question e))))
           in start (foldr Seq Accept
                     (replicate n (regexToMuTerm o e)))
       NamedSet _ _    -> error "Named sets not yet supported"
       LazyRange _ _ _ -> error "Lazy ranges not yet supported"


testKleenex :: String -> Either String (KleenexMu a)
testKleenex s = either Left (Right . kleenexToMuTerm) (H.parseKleenex s)
  
