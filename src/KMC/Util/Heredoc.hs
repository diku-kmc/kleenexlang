module KMC.Util.Heredoc where

import           Language.Haskell.TH.Quote (QuasiQuoter(..)
                                           ,quoteFile
                                           )
import           Language.Haskell.TH (litE, stringL)

strQ :: QuasiQuoter
strQ = QuasiQuoter
       { quoteExp  = litE . stringL
       , quotePat  = err
       , quoteType = err
       , quoteDec  = err
       }
    where
      err = error "strQ is an expression quasiquoter"

fileQ :: QuasiQuoter
fileQ = quoteFile strQ
