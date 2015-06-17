{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module KMC.Kleenex.Lang where

import           Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Word (Word8)
import           Data.ByteString (unpack, ByteString)
import           Data.Char (chr, ord)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)

import           KMC.Coding (codeFixedWidthEnumSized, decodeEnum)
import           KMC.Expression
import qualified KMC.Kleenex.Parser as H
import           KMC.OutputTerm (Const(..), InList(..), Ident(..), (:+:)(..), NullFun(..))
import           KMC.RangeSet (singleton, complement, rangeSet, union, RangeSet)
import           KMC.Syntax.External (Regex (..), unparse)
import           KMC.Theories (top)
import           KMC.Visualization (Pretty(..))

-- for test
import KMC.Util.Heredoc
import Debug.Trace
    
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
  deriving (Eq, Ord, Show)
nat2int :: Nat -> Int
nat2int Z = 0
nat2int (S n) = 1 + nat2int n
-- instance Show SimpleMu where
--     show (SMVar n) = show (nat2int n)
--     show (SMLoop e) = "μ.(" ++ show e ++ ")"
--     show (SMAlt l r) = "(" ++ show l ++ ")+(" ++ show r ++ ")"
--     show (SMSeq l r) = show l ++ show r
--     show (SMWrite s) = show s
--     show (SMRegex r) = "<" ++ unparse r ++ ">"
--     show (SMIgnore s) = "skip:[" ++ show s ++ "]"
--     show SMAccept = "1"

-- | Mu-terms created from Kleenex programs either output the identity on
-- the input character, injected into a list, or the output a constant list
-- of characters.
--type KleenexOutTerm = (InList (Ident Word8)) :+: (Const Word8 [Word8])
-- instance Pretty KleenexOutTerm where
--     pretty (Inl (InList _)) = "COPY"
--     pretty (Inr (Const [])) = "SKIP"
--     pretty (Inr (Const ws)) = "\"" ++ map toChar [ws] ++ "\""

type KleenexOutTerm = (InList (Ident Word8)) :+: (Const Word8 [Word8])
instance Pretty KleenexOutTerm where
    pretty l = case l of
                 Inl (InList _) -> "COPY"
                 Inr (Const ws) -> "\"" ++ map toChar [ws] ++ "\""
    

    
type KleenexMu a = Mu (RangeSet Word8) KleenexOutTerm a

-- | The term that copies the input char to output.
copyInput :: KleenexOutTerm
copyInput = Inl (InList Ident)

-- | The term that outputs a fixed string (list of Word8).
out :: [Word8] -> KleenexOutTerm
out = Inr . Const

-- | The term that throws away the input char.
skip :: KleenexOutTerm
skip = out [] --Inr NullFun

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
kleenexToSimpleMu initVar (H.Kleenex ass) =
        SMLoop $ flip evalState 0 $ go [initVar] (fromJust $ M.lookup initVar mp)
    where
      mp :: M.Map H.Identifier H.KleenexTerm
      mp = M.fromList (map (\(H.HA (k, v)) -> (k, v)) ass)

      nextIdent :: State Int H.Identifier
      nextIdent = do i <- get
                     put (i+1)
                     return $ H.Identifier $ "|_kleenex_to_simple_mu_ident_" ++ show i

      go :: [H.Identifier] -> H.KleenexTerm -> State Int SimpleMu
      go _    (H.Constant n) = return $ SMWrite n
      go vars (H.Var name) =
          case name `pos` vars of
            Nothing ->
                case M.lookup name mp of
                  Nothing -> error $ "Name not found: " ++ show name
                  Just t  -> fmap SMLoop $ go (name : vars) t
            Just p  -> return $ SMVar p
      go vars (H.Sum l r)    = do gol <- go vars l
                                  gor <- go vars r
                                  return $ SMAlt gol gor
      go vars (H.Seq l r)    = do gol <- go vars l
                                  gor <- go vars r
                                  return $ SMSeq gol gor
      go vars (H.Star e)     = do i <- nextIdent
                                  fmap SMLoop $ go (i : vars) $ H.Sum (H.Seq e $ H.Var i) H.One
      go vars (H.Plus e)     = go vars $ H.Seq e $ H.Star e
      go vars (H.Question e) = do goe <- go vars e
                                  return $ SMAlt goe SMAccept
      go _    H.One          = return $ SMAccept
      go vars (H.Ignore e)   = fmap SMIgnore $ go vars e
      go _    (H.RE re)      = return $ SMRegex re


-- mark :: Pos -> State Marked ()
-- mark pos = trace ("Marking " ++ show pos) $  modify (S.insert pos)

-- Work on the first-order representation.
findSuppressedSubterms :: SimpleMu -> Marked
findSuppressedSubterms = go [] S.empty
    where
      go cp marked sm =
          case sm of
            SMLoop e   -> go (L:cp) marked e
            SMAlt l r  -> (go (L:cp) marked l) `S.union` (go (R:cp) marked r)
            SMSeq l r  -> (go (L:cp) marked l) `S.union` (go (R:cp) marked r)
            SMIgnore e -> go cp (S.insert cp marked) e
            SMAccept   -> marked
            SMVar _    -> marked
            SMWrite _  -> marked
            SMRegex _  -> marked
            
                    

-- | A simple mu term is converted to a "real" mu term by converting the
-- de Bruijn-indexed variables to Haskell variables, and encoding the mu
-- abstractions as Haskell functions.  This function therefore converts
-- a de Bruijn representation to a HOAS representation.  All embedded
-- regular expressions are also represented as mu-terms.
simpleMuToMuTerm' :: [KleenexMu a] -> Bool -> SimpleMu -> KleenexMu a
simpleMuToMuTerm' st ign sm =
    case sm of
      SMVar n      -> maybe (error "stack exceeded") id $ getStack n st
      SMLoop sm'   -> Loop $ \x -> simpleMuToMuTerm' ((Var x) : st) ign sm'
      SMAlt l r    -> Alt (simpleMuToMuTerm' st ign l)
                          (simpleMuToMuTerm' st ign r)
      SMSeq l r    -> Seq (simpleMuToMuTerm' st ign l)
                          (simpleMuToMuTerm' st ign r)
      SMWrite s    -> if ign
                      then Accept
                      else W (unpack s) Accept
      SMRegex re   -> if ign
                      then regexToMuTerm skip re
                      else regexToMuTerm copyInput re
      SMIgnore sm' -> simpleMuToMuTerm' st True sm'
      SMAccept     -> Accept

simpleMuToMuTerm :: SimpleMu -> KleenexMu a
simpleMuToMuTerm sm = simpleMuToMuTerm' [] False sm

                      
-- | Convert a Kleenex program to a list of mu-term that encodes the string transformations
-- expressed in Kleenex.
kleenexToMuTerm :: ([H.Identifier], H.Kleenex) -> [(KleenexMu a, Marked)]
kleenexToMuTerm (is, h) = map f is
    where
      f i = let sm = kleenexToSimpleMu i h
            in (simpleMuToMuTerm sm, findSuppressedSubterms sm)

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


testKleenex :: String -> Either String [(KleenexMu a, Marked)]
testKleenex s = either Left (Right . kleenexToMuTerm) (H.parseKleenex s)


testSimple :: String -> Either String [(SimpleMu, Marked)]
testSimple s = either Left (Right . g) (H.parseKleenex s)
    where
      g (is, h) = map (f h) is
      f h i = let sm = kleenexToSimpleMu i h
              in (sm, findSuppressedSubterms sm)
                  
-- mu2 :: SimpleMu -- (KleenexMu a, Marked)
-- mu2 = either (error "woot") head  $ testSimple $ --  testKleenex $
--       [strQ|x
-- x := a | ~b
-- a := <a>
-- b := ~<b>
-- |]
-- mu1 :: (KleenexMu a, Marked)
-- mu1 = either (error "woot") head  $ testKleenex $
--       [strQ|x
-- x := a | ~b
-- a := <a>
-- b := ~<b>
-- |]
