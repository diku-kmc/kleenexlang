{-# LANGUAGE TypeOperators #-}
module KMC.Bitcoder
  ( BitOutputTerm
  , BitcodeMu
  , fromRegex
  , kleenexToBitcodeMuTerm
  , kleenexToBytecodeMuTerm
  , bTrue
  , bFalse
  ) where

import           Data.Char
import qualified Data.Set as S
import           Data.Word (Word8)

import           KMC.Syntax.External
import           KMC.Theories
import           KMC.OutputTerm
import           KMC.RangeSet hiding (size)
import           KMC.Expression
import           KMC.Kleenex.Lang
import qualified KMC.Kleenex.Parser as H

type BitOutputTerm bit sigma = (Join (Const sigma [bit] :+: Enumerator (RangeSet sigma) sigma bit) [bit])

type BitcodeMu sigma bit a = Mu (RangeSet sigma) (BitOutputTerm bit sigma) a

-- | Translate a regular expression to a mu-expression which denotes the parsing relation.
fromRegex :: (Ord sigma, Enum sigma, Bounded sigma, Bounded bit) =>
             ([bit] -> BitcodeMu sigma bit a -> BitcodeMu sigma bit a) ->
             (RangeSet sigma -> BitOutputTerm bit sigma -> BitcodeMu sigma bit a -> BitcodeMu sigma bit a) ->
             Regex ->
             BitcodeMu sigma bit a
fromRegex w rw One            = Accept
fromRegex w rw Dot            = rw top (Join [Inr $ Enumerator $ top]) Accept
fromRegex w rw (Chr a)        = rw (singleton n) (Join []) Accept
                                   where n = toEnum (ord a)
fromRegex w rw (Group _ e)    = fromRegex w rw e
fromRegex w rw (Concat e1 e2) = Seq (fromRegex w rw e1) (fromRegex w rw e2)
fromRegex w rw (Branch e1 e2) = Alt (w [bFalse] (fromRegex w rw e1)) (w [bTrue] (fromRegex w rw e2))
fromRegex w rw (Class b rs)   = rw rs' (Join [Inr $ Enumerator rs']) Accept
    where
        rs' = (if b then id else complement)
             $ rangeSet [ (toEnum (ord lo), toEnum (ord hi)) | (lo, hi) <- rs ]
fromRegex w rw (Star e)       = Loop $ \x -> Alt (w [bFalse] (Seq (fromRegex w rw e) (Var x)))
                                                 (w [bTrue] Accept)
fromRegex w rw (LazyStar e)   = Loop $ \x -> Alt (w [bFalse] Accept)
                                                 (w [bTrue] (Seq (fromRegex w rw e) (Var x)))
fromRegex w rw (Plus e)       = Seq (fromRegex w rw e) (fromRegex w rw (Star e))
fromRegex w rw (LazyPlus e)   = Seq (fromRegex w rw e) (fromRegex w rw (LazyStar e))
fromRegex w rw (Question e)   = Alt (w [bFalse] (fromRegex w rw e))
                                    (w [bTrue] Accept)
fromRegex w rw (LazyQuestion e) = Alt (w [bFalse] Accept)
                                      (w [bTrue] (fromRegex w rw e))
fromRegex w rw (Suppress e)   = fromRegex w rw e
    -- fromRegex (flip const) (\i o -> RW i (Join [])) e
    -- Would bypass option
fromRegex w rw (Range e n m)  =
    case m of
        Nothing -> Seq (repeatRegex w rw n e) (fromRegex w rw (Star e))
        Just m' -> if n == m' then repeatRegex w rw n e
                    else Seq (repeatRegex w rw n e) (repeatRegex w rw m' (Question e))
fromRegex w rw (NamedSet _ _) = error "Named sets not yet supported"
fromRegex w rw (LazyRange _ _ _) = error "Lazy ranges not yet supported"


repeatRegex :: (Ord sigma, Enum sigma, Bounded sigma, Bounded bit) =>
    ([bit] -> BitcodeMu sigma bit a -> BitcodeMu sigma bit a) ->
    (RangeSet sigma -> BitOutputTerm bit sigma -> BitcodeMu sigma bit a -> BitcodeMu sigma bit a) ->
    Int -> Regex -> BitcodeMu sigma bit a
repeatRegex w rw n e = foldr Seq Accept (replicate n (fromRegex w rw e))

-- | Convert a Kleenex program to a list of mu-terms, which output a bitcode signifying the path
-- traversed.
kleenexToBitcodeMuTerm :: H.Kleenex -> Bool -> [BitcodeMu Word8 Bool Int]
kleenexToBitcodeMuTerm k@(H.Kleenex is terms) suppressBits =
    map (\i -> simpleMuToBitcodeMuTerm [] suppressBits $ kleenexToSimpleMu i k) is

-- | Convert a Kleenex program to a list of mu-terms, which output a byte-aligned version of the
-- bitcode, signifying the path traversed.
kleenexToBytecodeMuTerm :: H.Kleenex -> Bool -> [BitcodeMu Word8 Word8 Int]
kleenexToBytecodeMuTerm k@(H.Kleenex is terms) suppressBits =
    map (\i -> simpleMuToBitcodeMuTerm [] suppressBits $ kleenexToSimpleMu i k) is

-- | A simple mu term is converted to a "real" mu term, that outputs the bitcode
-- of the path chosen, rather than its normal outputs.
simpleMuToBitcodeMuTerm :: (Ord sigma, Enum sigma, Bounded sigma, Bounded bit) =>
                           [BitcodeMu sigma bit a] -> Bool -> SimpleMu -> BitcodeMu sigma bit a
simpleMuToBitcodeMuTerm st suppressBits sm = go st sm []
    where
        w b = if b then flip const else W
        rw b = if b then (\i o -> RW i (Join [])) else RW

        suppTerms = if suppressBits then findActionfreeSuppressedSubterms sm else S.empty

        go st sm cp =
            let ign = S.member cp suppTerms in
            case sm of
                SMVar n      -> maybe (error "stack exceeded") id $ getStack n st
                SMLoop sm'   -> (Loop $ \x -> go ((Var x) : st) sm' (L:cp))
                SMAlt l r    -> (w ign [bFalse] $ go st l (L:cp)) `Alt`
                                (w ign [bTrue] $ go st r (R:cp))
                SMSeq l r    -> (go st l (L:cp)) `Seq` (go st r (R:cp))
                SMWrite _    -> Accept
                SMRegex re   -> fromRegex (w ign) (rw ign) re
                SMIgnore sm' -> go st sm' (L:cp)
                SMAction a e -> go st e (L:cp)
                SMAccept     -> Accept
