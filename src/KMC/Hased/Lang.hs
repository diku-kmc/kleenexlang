{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module KMC.Hased.Lang where

import qualified Data.Map as M
import           Data.Word (Word8)
import           Data.Char (chr, ord)
import           Data.Maybe (fromJust)
import qualified Data.GraphViz as GV

import           KMC.SSTConstruction (sstFromFST, PathTree, Var)
import           KMC.FSTConstruction (fromMu)
import           KMC.Visualization (Pretty(..), mkViz, mkVizToFile, fstToDot, sstToDot)
import           KMC.OutputTerm (Const(..), InList(..), Ident(..), (:+:)(..))
import           KMC.RangeSet (singleton, complement, rangeSet, RangeSet)
import           KMC.SymbolicSST (run, SST, Stream(..), optimize, enumerateStates)
import           KMC.Theories (top)
import           KMC.Expression (Mu (..))
import           KMC.Syntax.External (Regex (..), unparse)
import qualified KMC.Hased.Parser as H

import Debug.Trace

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
              | SMWrite String
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
      go vars (H.Constant n) = SMWrite n
      go vars (H.Var name) =
          case name `pos` vars of
            Nothing ->
                case M.lookup name mp of
                  Nothing -> error $ "Name not found: " ++ show name
                  Just t  -> SMLoop $ go (name : vars) t
            Just p  -> SMVar p
      go vars (H.Sum l r) = SMAlt (go vars l) (go vars r)
      go vars (H.Seq l r) = SMSeq (go vars l) (go vars r)
      go vars H.One       = SMAccept
      go vars (H.Ignore e) = SMIgnore $ go vars e
      go _ (H.RE re) = SMRegex re

-- | A simple mu term is converted to a "real" mu term by converting the
-- de Bruijn-indexed variables to Haskell variables, and encoding the mu
-- abstractions as Haskell functions.  This function therefore converts
-- a de Bruijn representation to a HOAS representation.  All embedded
-- regular expressions are also represented as mu-terms.
simpleMuToMuTerm :: [HasedMu a] -> Bool -> SimpleMu -> HasedMu a
simpleMuToMuTerm st ign sm =
    case sm of
      SMVar n      -> maybe (error "stack exceeded") id $ getStack n st
      SMLoop sm    -> Loop $ \x -> simpleMuToMuTerm ((Var x) : st) ign sm
      SMAlt l r    -> (simpleMuToMuTerm st ign l) `Alt` (simpleMuToMuTerm st ign r)
      SMSeq l r    -> (simpleMuToMuTerm st ign l) `Seq` (simpleMuToMuTerm st ign r)
      SMWrite s    -> if ign
                      then W [] Accept
                      else W (map fromChar s) Accept
      SMRegex re   -> if ign
                      then regexToMuTerm (out []) re
                      else regexToMuTerm copyInput re
      SMIgnore sm' -> simpleMuToMuTerm st True sm'
      SMAccept     -> Accept

-- | Convert a Hased program to a mu-term that encodes the string transformation
-- expressed in Hased.
hasedToMuTerm :: (H.Identifier, H.Hased) -> HasedMu a
hasedToMuTerm (i, h) = simpleMuToMuTerm [] False $ hasedToSimpleMu i h

-- | Translates a regular expression into a mu-term that performs the given
-- action on the matched symbols: It either copies any matched symbols or
-- ignores them.
regexToMuTerm :: HasedOutTerm -> Regex -> HasedMu a
regexToMuTerm o r =
    case r of
       One        -> Accept
       Dot        -> RW top o Accept
       Chr a      -> RW (singleton (fromChar a)) o Accept
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
           let start exp = case m of
                             Nothing -> exp `Seq` (regexToMuTerm o (Star e))
                             Just n' -> exp `Seq` (foldr Seq Accept
                                                   (replicate (n' - n)
                                                    (regexToMuTerm o (Question e))))
           in start (foldr Seq Accept
                     (replicate n (regexToMuTerm o e)))
       NamedSet _ _    -> error "Named sets not yet supported"
       LazyRange _ _ _ -> error "Lazy ranges not yet supported"

f s = runFile "test/issuu.has" s >>= putStr
runFile :: FilePath -> String -> IO String
runFile fp str = H.parseHasedFile fp >>= flip go str
    where
      go (Left e) _ = error e
      go (Right ih) str = do
        let sst = sstFromFST (fromMu (hasedToMuTerm ih))
                :: SST (PathTree Var Int) (RangeSet Word8) HasedOutTerm Var
        return $ showStream $ streamToString $ KMC.SymbolicSST.run sst $ map fromChar str
f' = showFile "test/issuu.has"
showFile :: FilePath -> IO ()
showFile fp = readFile fp >>= H.pf

showStream :: Stream String -> String
showStream (Chunk e es) = e ++ showStream es
showStream Done = ""
showStream (Fail s) = " [ERROR: " ++ s ++ "]"


fileAsSimpleMu :: String -> FilePath -> IO SimpleMu
fileAsSimpleMu n fp = readFile fp >>= H.pf' >>= return . hasedToSimpleMu (H.mkIdent n)

runSimpleMu :: SimpleMu -> String -> Stream String
runSimpleMu m str =
    let sst = sstFromFST (fromMu (simpleMuToMuTerm [] False m))
            :: SST (PathTree Var Int)
               (RangeSet Word8) HasedOutTerm Var
    in streamToString $ KMC.SymbolicSST.run sst (map fromChar str)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


hasedToFSTDot :: String -> GV.DotGraph Int
hasedToFSTDot str =
    case H.parseHased str of
      Left e -> error e
      Right ih -> let fst = fromMu (hasedToMuTerm ih)
                  in fstToDot fst

hasedToSSTDot :: String -> GV.DotGraph Int
hasedToSSTDot str =
    case H.parseHased str of
      Left e -> error e
      Right ih -> let sst = sstFromFST (fromMu (hasedToMuTerm ih))
                          :: SST (PathTree Var Int)
                             (RangeSet Word8)
                             HasedOutTerm
                             Var
                  in sstToDot (optimize $ enumerateStates sst)

-- Opening an Xlib canvas does not work in my cabal sandbox-version of GraphViz.
vizHasedAsFST :: String -> IO ()
vizHasedAsFST s = mkVizToFile hasedToFSTDot s "test_fst.pdf"

vizHasedAsSST :: String -> IO ()
vizHasedAsSST s = mkVizToFile hasedToSSTDot s "test_sst.pdf"

vizHasedFileAsFST :: FilePath -> IO ()
vizHasedFileAsFST fp = readFile fp >>= vizHasedAsFST
vizHasedFileAsSST :: FilePath -> IO ()
vizHasedFileAsSST fp = readFile fp >>= vizHasedAsSST

streamToString :: Enum a => Stream [a] -> Stream String
streamToString (Chunk e es) = Chunk (map toChar e) (streamToString es)
streamToString Done = Done
streamToString (Fail s) = Fail s

runHasedSST :: String -> String -> Stream String
runHasedSST str ws = streamToString $ 
    case H.parseHased str of
      Left e -> error e
      Right ih -> let sst = sstFromFST (fromMu (hasedToMuTerm ih))
                          :: SST (PathTree Var Int)
                             (RangeSet Word8)
                             HasedOutTerm
                             Var
                  in KMC.SymbolicSST.run sst (map fromChar ws)


iss1 = "{visitor_ip:255.233.123.213,visitor_device:browser}"
iss1many = unlines $ replicate 4 iss1

iss2 = "{\"ts\":1393631983,\"visitor_uuid\":\"04daa9ed9dde73d3\",\"visitor_source\":\"external\",\"visitor_device\":\"browser\",\"visitor_useragent\":\"Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.117 Safari/537.36\",\"visitor_ip\":\"6a3273d508a9de04\"}"
iss2many = unlines $ replicate 4 iss2

iss3 = "{\"ts\":1393631983,\"visitor_uuid\":\"04daa9ed9dde73d3\",\"visitor_source\":\"external\",\"visitor_device\":\"browser\",\"visitor_useragent\":\"Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.117 Safari/537.36\",\"visitor_ip\":\"6a3273d508a9de04\",\"visitor_country\":\"ES\",\"visitor_referrer\":\"64f729926497515c\",\"env_type\":\"reader\",\"env_doc_id\":\"140224195414-e5a9acedd5eb6631bb6b39422fba6798\",\"event_type\":\"impression\",\"subject_type\":\"doc\",\"subject_doc_id\":\"140224195414-e5a9acedd5eb6631bb6b39422fba6798\",\"subject_page\":0,\"cause_type\":\"impression\"}"
iss3many = unlines $ replicate 4 iss3
