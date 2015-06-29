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
             Regex -> BitcodeMu sigma bit a
fromRegex One            = Accept
fromRegex Dot            = RW top (Join [Inr $ Enumerator $ top]) Accept
fromRegex (Chr a)        = RW (singleton n) (Join [Inl $ Const [bFalse]]) Accept
                           where n = toEnum (ord a)
fromRegex (Group _ e)    = fromRegex e
fromRegex (Concat e1 e2) = Seq (fromRegex e1) (fromRegex e2)
fromRegex (Branch e1 e2) = Alt (W [bFalse] (fromRegex e1)) (W [bTrue] (fromRegex e2))
fromRegex (Class b rs)   = RW rs' (Join [out]) Accept
    where
        out = if size rs' == 1 then (Inl $ Const [bFalse]) else (Inr $ Enumerator rs')
        rs' = (if b then id else complement)
             $ rangeSet [ (toEnum (ord lo), toEnum (ord hi)) | (lo, hi) <- rs ]
fromRegex (Star e)       = Loop $ \x -> Alt (W [bFalse] (Seq (fromRegex e) (Var x)))
                                            (W [bTrue] Accept)
fromRegex (LazyStar e)   = Loop $ \x -> Alt (W [bFalse] Accept)
                                            (W [bTrue] (Seq (fromRegex e) (Var x)))
fromRegex (Plus e)       = Seq (fromRegex e) (fromRegex (Star e))
fromRegex (LazyPlus e)   = Seq (fromRegex e) (fromRegex (LazyStar e))
fromRegex (Question e)   = Alt (W [bFalse] (fromRegex e))
                               (W [bTrue] Accept)
fromRegex (LazyQuestion e) = Alt (W [bFalse] Accept)
                                 (W [bTrue] (fromRegex e))
fromRegex (Suppress e)   = fromRegex e
fromRegex (Range e n m)  = case m of
                             Nothing -> Seq (repeatRegex n e) (fromRegex (Star e))
                             Just m' -> if n == m' then repeatRegex n e
                                        else Seq (repeatRegex n e) (repeatRegex m' (Question e))
fromRegex (NamedSet _ _) = error "Named sets not yet supported"
fromRegex (LazyRange _ _ _) = error "Lazy ranges not yet supported"


repeatRegex :: (Ord sigma, Enum sigma, Bounded sigma, Bounded bit)
       => Int -> Regex -> BitcodeMu sigma bit a
repeatRegex n e = foldr Seq Accept (replicate n (fromRegex e))

-- | Convert a Kleenex program to a list of mu-terms, which output a bitcode signifying the path
-- traversed.
kleenexToBitcodeMuTerm :: H.Kleenex -> [BitcodeMu Word8 Bool Int]
kleenexToBitcodeMuTerm k@(H.Kleenex is terms) = map (\i -> simpleMuToBitcodeMuTerm [] $ kleenexToSimpleMu i k) is

-- | Convert a Kleenex program to a list of mu-terms, which output a byte-aligned version of the
-- bitcode, signifying the path traversed.
kleenexToBytecodeMuTerm :: H.Kleenex -> [BitcodeMu Word8 Word8 Int]
kleenexToBytecodeMuTerm k@(H.Kleenex is terms) = map (\i -> simpleMuToBitcodeMuTerm [] $ kleenexToSimpleMu i k) is

-- | A simple mu term is converted to a "real" mu term, that outputs the bitcode
-- of the path chosen, rather than its normal outputs.
simpleMuToBitcodeMuTerm :: (Ord sigma, Enum sigma, Bounded sigma, Bounded bit) =>
                           [BitcodeMu sigma bit a] -> SimpleMu -> BitcodeMu sigma bit a
simpleMuToBitcodeMuTerm st sm =
    case sm of
      SMVar n      -> maybe (error "stack exceeded") id $ getStack n st
      SMLoop sm'   -> (Loop $ \x -> simpleMuToBitcodeMuTerm ((Var x) : st) sm')
      SMAlt l r    -> (W [bFalse] $ simpleMuToBitcodeMuTerm st l) `Alt`
                        (W [bTrue] $ simpleMuToBitcodeMuTerm st r)
      SMSeq l r    -> (simpleMuToBitcodeMuTerm st l) `Seq` (simpleMuToBitcodeMuTerm st r)
      SMWrite _    -> W [bFalse] Accept
      SMRegex re   -> fromRegex re
      SMIgnore sm' -> simpleMuToBitcodeMuTerm st sm'
      SMAction a e -> W [bFalse] $ simpleMuToBitcodeMuTerm st e
      SMAccept     -> Accept
