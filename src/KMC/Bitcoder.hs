{-# LANGUAGE TypeOperators #-}
module KMC.Bitcoder(BitOutputTerm, fromRegex) where

import Data.Char
import KMC.Syntax.External
import KMC.Theories
import KMC.OutputTerm
import KMC.RangeSet
import KMC.Expression

type BitOutputTerm sigma = (Join (Const sigma [Bool] :+: Enumerator (RangeSet sigma) sigma Bool) [Bool])

-- | Translate a regular expression to a mu-expression which denotes the parsing relation.
fromRegex :: (Ord sigma, Enum sigma, Bounded sigma) =>
             Regex -> Mu (RangeSet sigma) (BitOutputTerm sigma) a
fromRegex One            = Accept
fromRegex Dot            = RW top (Join [Inr $ Enumerator $ top]) Accept
fromRegex (Chr a)        = RW (singleton n) (Join [Inr $ Enumerator (singleton n)]) Accept
                           where n = toEnum (ord a)
fromRegex (Group _ e)    = fromRegex e
fromRegex (Concat e1 e2) = Seq (fromRegex e1) (fromRegex e2)
fromRegex (Branch e1 e2) = Alt (W [False] (fromRegex e1)) (W [True] (fromRegex e2))
fromRegex (Class b rs)   = RW rs' (Join [Inr $ Enumerator rs']) Accept
    where rs' = (if b then id else complement)
                $ rangeSet [ (toEnum (ord lo), toEnum (ord hi)) | (lo, hi) <- rs ]
fromRegex (Star e)       = Loop $ \x -> Alt (W [False] (Seq (fromRegex e) (Var x)))
                                            (W [True] Accept)
fromRegex (LazyStar e)   = Loop $ \x -> Alt (W [False] Accept)
                                            (W [True] (Seq (fromRegex e) (Var x)))
fromRegex (Plus e)       = Seq (fromRegex e) (fromRegex (Star e))
fromRegex (LazyPlus e)   = Seq (fromRegex e) (fromRegex (LazyStar e))
fromRegex (Question e)   = Alt (W [False] (fromRegex e))
                               (W [True] Accept)
fromRegex (LazyQuestion e) = Alt (W [False] Accept)
                                 (W [True] (fromRegex e))
fromRegex (Suppress e)   = fromRegex e
fromRegex (NamedSet _ _) = error "Named sets not yet supported"
fromRegex (Range _ _ _) = error "Ranges not yet supported"
fromRegex (LazyRange _ _ _) = error "Lazy ranges not yet supported"

