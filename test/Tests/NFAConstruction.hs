import KMC.NFAConstruction
import KMC.Kleenex.Lang
import KMC.RangeSet
import KMC.Util.Heredoc


nfa1 :: NFA Int (RangeSet Word8)
nfa1 = either (error "nfa1 - fail") (nfaFromMu . fst . head) $ testKleenex $
       [strQ|x
       x := (aaa | aa)*
       aaa := <aaa> "bcd"
       aa := <aa> "de"
       |]
nfa2 :: NFA Int (RangeSet Word8)
nfa2 = either (error "fst2 - fail") (nfaFromMu . fst . head) $ testKleenex $
       [strQ|y
y := ~x*
x := <a> "b" | <b> "a"
       |]
