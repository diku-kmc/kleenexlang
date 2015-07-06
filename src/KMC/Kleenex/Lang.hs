{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module KMC.Kleenex.Lang where

import           Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Word (Word8(..), Word16(..))
import           Data.ByteString (unpack, ByteString)
import           Data.Char (chr, ord)
import           Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)

import           KMC.Coding
import           KMC.Expression
import           KMC.Kleenex.Action
import qualified KMC.Kleenex.Parser as H
import           KMC.OutputTerm (Const(..), InList(..), Ident(..), (:+:)(..), NullFun(..))
import           KMC.RangeSet (singleton, complement, rangeSet, union, RangeSet, size, findMax)
import           KMC.Syntax.External (Regex (..), unparse)
import           KMC.Theories (top)
import           KMC.Util.Bits (packCombine)

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
              | SMAction KleenexAction SimpleMu
              | SMAccept
  deriving (Eq, Ord, Show)

nat2int :: Nat -> Int
nat2int Z = 0
nat2int (S n) = 1 + nat2int n

-- | Mu-terms created from Kleenex programs either output the identity on
-- the input character, injected into a list, or the output a constant list
-- of characters.
type KleenexTerm w = (InList (Ident w)) :+: (Const w [w])
type KleenexMuG w a = Mu (RangeSet w) (KleenexTerm w) a

type KleenexOutTerm = KleenexTerm Word8
type KleenexMu a = KleenexMuG Word8 a

-- | The term that copies the input char to output.
copyInput :: KleenexOutTerm
copyInput = Inl (InList Ident)

-- | The term that outputs a fixed string (list of Word8).
--out :: [Word8] -> KleenexOutTerm
out :: [w] -> KleenexTerm w
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
kleenexToSimpleMu initVar (H.Kleenex _ ass) = SMLoop $ evalState (go [initVar] (getVar initVar)) 0
    where
      getVar :: H.Identifier -> H.KleenexTerm
      getVar var = case M.lookup var mp of
                      Just term -> term
                      Nothing   -> error $ "Undefined term: " ++ show var
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
      go vars (H.Sum l r)     = do gol <- go vars l
                                   gor <- go vars r
                                   return $ SMAlt gol gor
      go vars (H.Seq l r)     = do gol <- go vars l
                                   gor <- go vars r
                                   return $ SMSeq gol gor
      go vars (H.Star e)      = do i <- nextIdent
                                   fmap SMLoop $ go (i : vars) $ H.Sum (H.Seq e $ H.Var i) H.One
      go vars (H.Plus e)      = go vars $ H.Seq e $ H.Star e
      go vars (H.Question e)  = do goe <- go vars e
                                   return $ SMAlt goe SMAccept
      go vars (H.Action a e)   = fmap (SMAction a) $ go vars e
      go vars (H.Range l r e) = do goe <- go vars e
                                   let m = fromMaybe 0 l
                                   rest <- case r of
                                              Nothing -> fmap (: []) $ go vars $ H.Star e
                                              Just n  -> if n < m then error $ "Invalid range: " ++ show m ++ "-" ++ show n
                                                                  else return $ replicate (n-m) (SMAlt goe SMAccept)
                                   return $ foldl1 SMSeq $ replicate m goe ++ rest
      go _    H.One           = return $ SMAccept
      go vars (H.Ignore e)    = fmap SMIgnore $ go vars e
      go _    (H.RE re)       = return $ SMRegex re


-- | Find the locations of subterms that are suppressed.
-- It infers the "most general location" like so:
--      (a) E = ~l ~r     --> E' = ~(l r)
--      (b) E = ~l | ~r   --> E' = ~(l | r)
--      (c) E = (~e)*     --> E' = ~(e*)
findSuppressedSubterms :: SimpleMu -> Marked
findSuppressedSubterms = go [] S.empty
    where
      addIfBoth cp marks =
          if (L:cp) `S.member` marks && (R:cp) `S.member` marks
          then S.insert cp marks
          else marks
      go cp marked sm =
          case sm of
            SMLoop e   ->
                let marks = go (L:cp) marked e
                in if (L:cp) `S.member` marks
                   then S.insert cp marks
                   else marks
            SMAlt l r  ->
                addIfBoth cp $ (go (L:cp) marked l) `S.union` (go (R:cp) marked r)
            SMSeq l r  ->
                addIfBoth cp $ (go (L:cp) marked l) `S.union` (go (R:cp) marked r)
            SMIgnore e -> S.insert cp marked
            SMAccept   -> marked
            SMVar _    -> marked
            SMWrite _  -> marked
            SMRegex _  -> marked

-- | Find the locations of subterms that contain no actions.
findActionfreeSubterms :: SimpleMu -> Marked
findActionfreeSubterms = M.keysSet . M.filter id . findFix M.empty
    where
        -- | Fix-point iterate until we know which recursive subterms
        --   contain actions for sure.
        findFix m sm = let (_, m') = go [] m [] sm
                       in if m == m' then m else findFix m' sm

        -- Returns: (isActionFree, actionFreePaths)
        go cp marked vars sm =
            let (isActionFree, marked') = case sm of
                    SMLoop e   -> go (L:cp) marked (cp:vars) e
                    SMAlt l r  ->
                        let (isActionFree,  marked')  = go (L:cp) marked  vars l
                            (isActionFree', marked'') = go (R:cp) marked' vars r
                        in (isActionFree && isActionFree', marked'')
                    SMSeq l r  ->
                        let (isActionFree,  marked')  = go (L:cp) marked  vars l
                            (isActionFree', marked'') = go (R:cp) marked' vars r
                        in (isActionFree && isActionFree', marked'')
                    SMIgnore e -> go (L:cp) marked vars e
                    SMAccept   -> (True, marked)
                    SMVar v    ->
                        let cp' = maybe (error "unable to locate variable on stack") id $ getStack v vars
                        in (M.findWithDefault True cp' marked, marked)
                    SMWrite _    -> (True, marked)
                    SMAction _ e ->
                        let (_, marked') = go (L:cp) marked vars e
                        in (False, marked')
                    SMRegex _    -> (True, marked)
            in (isActionFree, M.insert cp isActionFree marked')

-- | Find the locations of subterms that are both suppressed, and contain no actions.
--   (i.e. can be safely ignored)
findActionfreeSuppressedSubterms :: SimpleMu -> Marked
findActionfreeSuppressedSubterms sm = go [] S.empty False sm
    where
        afs = findActionfreeSubterms sm

        actFree cp = S.member cp afs

        addIfBoth cp marks =
            if (L:cp) `S.member` marks && (R:cp) `S.member` marks
            then S.insert cp marks
            else marks

        addIfActFree ign cp marks =
            if ign && actFree cp
            then S.insert cp marks
            else marks

        go cp marked ign sm =
            case sm of
                SMLoop e     ->
                    let marks = go (L:cp) marked ign e
                    in if (L:cp) `S.member` marks
                    then S.insert cp marks
                    else marks
                SMAlt l r    ->
                    addIfBoth cp $ (go (L:cp) marked ign l) `S.union` (go (R:cp) marked ign r)
                SMSeq l r    ->
                    addIfBoth cp $ (go (L:cp) marked ign l) `S.union` (go (R:cp) marked ign r)
                SMIgnore e   ->
                    addIfActFree True cp $ go (L:cp) marked True e
                SMAccept     -> addIfActFree ign cp marked
                SMAction _ e -> go (L:cp) marked False e
                    -- For action terms: assume unignored (conservative) - necessary due to push/pop.
                    -- Consider: ~( a@foo ) !a
                    -- Here foo isn't suppressed anymore.
                SMVar _      -> addIfActFree ign cp marked
                SMWrite _    -> addIfActFree ign cp marked
                SMRegex _    -> addIfActFree ign cp marked

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
      SMAction a e -> Accept
      SMAccept -> Accept

simpleMuToMuTerm :: SimpleMu -> KleenexMu a
simpleMuToMuTerm sm = simpleMuToMuTerm' [] False sm

unpack16 :: ByteString -> [Word16]
unpack16 = packCombine 0 . unpack
                      
-- | Convert a Kleenex program to a list of mu-term that encodes the string transformations
-- expressed in Kleenex.
kleenexToActionMuTerm :: H.Kleenex -> Bool -> [KleenexActionMu a]
kleenexToActionMuTerm k@(H.Kleenex is terms) suppressBits =
    map (\i -> simpleMuToActionMuTerm [] False suppressBits $ kleenexToSimpleMu i k) is

kleenexToMuTerm :: H.Kleenex -> [(KleenexMu a, Marked)]
kleenexToMuTerm k@(H.Kleenex is h) = map f is
    where
      f i = let sm = kleenexToSimpleMu i k
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

--- Kleenex with actions translation


simpleMuToActionMuTerm :: [KleenexActionMu a] -> Bool -> Bool -> SimpleMu -> KleenexActionMu a
simpleMuToActionMuTerm st ign suppressBits sm = go st ign sm []
    where
        suppTerms = if suppressBits then findActionfreeSuppressedSubterms sm else S.empty

        go st ign sm cp =
            if S.member cp suppTerms
            then Accept
            else case sm of
                    SMVar n      -> maybe (error "stack exceeded") id $ getStack n st
                    SMLoop sm'   -> (Loop $ \x -> go ((Var x) : st) ign sm' (L:cp))
                    SMAlt l r    -> (go st ign l (L:cp)) `Alt` (go st ign r (R:cp))
                    SMSeq l r    -> (go st ign l (L:cp)) `Seq` (go st ign r (R:cp))
                    SMWrite bs   -> if ign
                                    then Accept
                                    else W (unpack bs) Accept
                    SMRegex re   -> regexToActionMuTerm ign re
                    SMIgnore sm' -> go st True sm' (L:cp)
                    SMAction a e -> Action a $ go st ign e (L:cp)
                    SMAccept     -> Accept

regexToActionMuTerm  :: Bool -> Regex -> KleenexActionMu a
regexToActionMuTerm ign re =
    case re of
        One          -> Accept
        Dot          -> RW (matchTop 8) (o (matchTop 8) top) Accept
        Chr a        -> if ign then Accept else W (encodeChar a) Accept
        Group _ e    -> regexToActionMuTerm ign e
        Concat e1 e2 -> Seq (regexToActionMuTerm ign e1) (regexToActionMuTerm ign e2)
        Branch e1 e2 -> Alt (regexToActionMuTerm ign e1) (regexToActionMuTerm ign e2)
        (Class b rs) -> let len = bitWidth 2 $ size rs'
                            rs' = (if b then id else complement) $ 
                                    rangeSet [(toEnum $ ord a, toEnum $ ord b) | (a,b) <- rs]
                            rs'' = matchRange len (BitString $ codeFixedWidthEnum len 0) 
                                                  (BitString $ codeFixedWidthEnum len $ size rs' - 1)
                        in  RW rs'' (o rs'' rs') Accept
        (Star e)       -> Loop $ \x -> Alt (Seq (regexToActionMuTerm ign e) (Var x)) Accept
        (LazyStar e)   -> Loop $ \x -> Alt Accept (Seq (regexToActionMuTerm ign e) (Var x))
        (Plus e)       -> Seq (regexToActionMuTerm ign e) (regexToActionMuTerm ign (Star e))
        (LazyPlus e)   -> Seq (regexToActionMuTerm ign e) (regexToActionMuTerm ign (LazyStar e))
        (Question e)   -> Alt (regexToActionMuTerm ign e) Accept
        (LazyQuestion e)   -> Alt (regexToActionMuTerm ign e) Accept
        (Suppress e)   -> regexToActionMuTerm True e
        (Range e n m)  -> case m of
                           Nothing -> Seq (repeatRegex' ign n e) (regexToActionMuTerm ign (Star e))
                           Just m' -> if n == m' then repeatRegex' ign n e
                                      else Seq (repeatRegex' ign n e) (repeatRegex' ign m' (Question e))
        (NamedSet _ _) -> error "Named sets not yet supported"
        (LazyRange _ _ _) -> error "Lazy ranges not yet supported"
    where
        o = if ign then nop else parseBitsAction


repeatRegex' :: Bool -> Int -> Regex -> KleenexActionMu a
repeatRegex' ign n re = foldr Seq Accept (replicate n (regexToActionMuTerm ign re))


testSimple :: String -> Either String [(SimpleMu, Marked)]
testSimple s = either Left (Right . g) (H.parseKleenex s)
    where
      g k@(H.Kleenex is _) = map (f k) is
      f h i = let sm = kleenexToSimpleMu i h
              in (sm, findSuppressedSubterms sm)

