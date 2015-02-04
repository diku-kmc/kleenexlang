{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module KMC.Program.Backends.HasedConvert where

import           Control.Monad.State
import qualified Data.Map as M
import           Data.Word (Word8)
import           Data.Char (chr, ord)
import           Data.Maybe (fromJust)
import qualified Data.GraphViz as GV

import qualified KMC.SSTConstruction as SC    
import qualified KMC.FSTConstruction as FC
import           KMC.Syntax.Config
import           KMC.Syntax.Parser
import           KMC.Visualization (Pretty(..), mkViz, mkVizToFile, fstToDot, sstToDot)
import           KMC.OutputTerm
import           KMC.RangeSet
import           KMC.SymbolicSST
import           KMC.SymbolicFST
import           KMC.Theories
import           KMC.Expression (Mu (..))
import           KMC.Syntax.External (Regex (..))
import qualified KMC.Program.Backends.HasedParser as H


fromChar :: Enum a => Char -> a
fromChar = toEnum . ord

toChar :: Enum a => a -> Char
toChar = chr . fromEnum
    
{-- Intermediate internal data type for the mu-terms.  These use de Bruijn-indices. --}
data Nat = Z | S Nat      deriving (Eq, Ord, Show)
data SimpleMu = SMVar Nat
              | SMLoop SimpleMu
              | SMAlt SimpleMu SimpleMu
              | SMSeq SimpleMu SimpleMu
              | SMWrite String SimpleMu
              | SMRegex Skip Regex
              | SMAccept
  deriving (Eq, Ord, Show)

-- | Mu-terms created from Hased programs either output the identity on
-- the input character, injected into a list, or the output a constant list
-- of characters.
type HasedOutTerm = (InList (Ident Word8)) :+: (Const Word8 [Word8])
instance Pretty HasedOutTerm where
    pretty (Inl (InList _)) = "COPY"
    pretty (Inr (Const [])) = "SKIP"
    pretty (Inr (Const ws)) = "\"" ++ map toChar ws ++ "\""

type HasedMu a = Mu (RangeSet Word8) HasedOutTerm a

-- | The term that copies the input char to output.
copyInput :: HasedOutTerm
copyInput = Inl (InList Ident)

-- | The term that outputs a fixed string (list of Word8).
out :: [Word8] -> HasedOutTerm
out = Inr . Const
-- | Friendlier version of 'out'.
out' :: String -> HasedOutTerm
out' = out . map fromChar

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
hasedToSimpleMu :: H.Identifier -> H.Hased -> SimpleMu 
hasedToSimpleMu init (H.Hased ass) = SMLoop $ go [init] (fromJust $ M.lookup init mp)
    where
      mp :: M.Map H.Identifier H.HasedTerm
      mp = M.fromList (map (\(H.HA (k, v)) -> (k, v)) ass)
      
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

-- | A simple mu term is converted to a "real" mu term by converting the
-- de Bruijn-indexed variables to Haskell variables, and encoding the mu
-- abstractions as Haskell functions.  This function therefore converts
-- a de Bruijn representation to a HOAS representation.
simpleMuToMuTerm :: [HasedMu a] -> SimpleMu -> HasedMu a
simpleMuToMuTerm st sm =
    case sm of
      SMVar n           -> maybe (error "stack exceeded") id $ getStack n st
      SMLoop sm         -> Loop $ \x -> simpleMuToMuTerm ((Var x) : st) sm
      SMAlt l r         -> Alt (simpleMuToMuTerm st l) (simpleMuToMuTerm st r)
      SMSeq l r         -> Seq (simpleMuToMuTerm st l) (simpleMuToMuTerm st r)
      SMWrite s e       -> W (map fromChar s) (simpleMuToMuTerm st e)
      SMRegex Skip re   -> fromRegex (out []) re
      SMRegex NoSkip re -> fromRegex copyInput re
      SMAccept          -> Accept

-- | Convert a Hased program to a mu-term that encodes the string transformation
-- expressed in Hased.
hasedToMuTerm :: (H.Identifier, H.Hased) -> HasedMu a
hasedToMuTerm (i, h) = simpleMuToMuTerm [] $ hasedToSimpleMu i h

-- | Translates a regular expression into a mu-term that performs the given
-- action on the matched symbols: It either copies any matched symbols or
-- ignores them.
fromRegex :: HasedOutTerm -> Regex -> HasedMu a
fromRegex _ One            = Accept
fromRegex o Dot            = RW top o Accept
fromRegex o (Chr a)        = RW (singleton (fromChar a)) o Accept
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
fromRegex _ (Suppress e)   = fromRegex (out []) e
fromRegex _ (NamedSet _ _) = error "Named sets not yet supported"
fromRegex _ (Range _ _ _)  = error "Ranges not yet supported"
fromRegex _ (LazyRange _ _ _) = error "Lazy ranges not yet supported"


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


hasedToFSTDot :: String -> GV.DotGraph Int
hasedToFSTDot str =
    case H.parseHased str of
      Left e -> error e
      Right ih -> let fst = FC.fromMu (hasedToMuTerm ih)
                  in fstToDot fst

hasedToSSTDot :: String -> GV.DotGraph Int
hasedToSSTDot str =
    case H.parseHased str of
      Left e -> error e
      Right ih -> let sst = SC.sstFromFST (FC.fromMu (hasedToMuTerm ih))
                          :: SST (SC.PathTree SC.Var Int)
                             (RangeSet Word8)
                             HasedOutTerm
                             SC.Var
                  in sstToDot (optimize $ enumerateStates sst)

-- Opening an Xlib canvas does not work in my cabal sandbox-version of GraphViz.
vizHasedAsFST :: String -> IO ()
vizHasedAsFST s = mkVizToFile hasedToFSTDot s "test_fst.pdf"

vizHasedAsSST :: String -> IO ()
vizHasedAsSST s = mkVizToFile hasedToSSTDot s "test_sst.pdf"

streamToString :: Enum a => Stream [a] -> Stream String
streamToString (Chunk e es) = Chunk (map toChar e) (streamToString es)
streamToString Done = Done
streamToString (Fail s) = Fail s

runHasedSST :: String -> String -> Stream String
runHasedSST str ws = streamToString $ 
    case H.parseHased str of
      Left e -> error e
      Right ih -> let sst = SC.sstFromFST (FC.fromMu (hasedToMuTerm ih))
                          :: SST (SC.PathTree SC.Var Int)
                             (RangeSet Word8)
                             HasedOutTerm
                             SC.Var
                  in KMC.SymbolicSST.run sst (map fromChar ws)
