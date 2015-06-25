import KMC.Kleenex.Lang
import KMC.Util.Heredoc
import KMC.RangeSet
import Data.Word
import Debug.Trace
import KMC.Visualization
import KMC.SSTConstruction hiding (Var)
import KMC.FSTConstruction 

s1 :: String
s1 =  [strQ|
       main := (~aaa | aa)*
       aaa := /aaa/ "bcd"
       aa := /aa/ "de"
       |]

s2 :: String
s2 = [strQ|
main := ~(l r)
l := /(a|b)*/ "AB"
r := /(c|d)*/ "CD"
|]

s3 :: String
s3 = [strQ|
main := /a/ ~/b/ /c/
|]

s4 :: String
s4 = [strQ|
main := ~(/def*/?)
|]

mu1 :: (KleenexMu a, Marked)
mu1 = either (error "mu1 - fail") head $ testKleenex s1

sm1 :: (SimpleMu, Marked)
sm1 = either (error "sm1") head $ testSimple s1

mu2 = either (error "mu2") head $ testKleenex s2
mu3 = either (error "mu3") head $ testKleenex s3
mu4 = either (error "mu4") head $ testKleenex s4
      
f1 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
f1 = fromMu (fst mu1)

g1 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
g1 = fromMuWithDFA (snd mu1) (fst mu1)

f2 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
f2 = fromMu (fst mu2)
g2 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
g2 = fromMuWithDFA (snd mu2) (fst mu2)

f3 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
f3 = fromMu (fst mu3)
g3 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
g3 = fromMuWithDFA (snd mu3) (fst mu3)


f4 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
f4 = fromMu (fst mu4)
g4 :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
g4 = fromMuWithDFA (snd mu4) (fst mu4)
     
          

viz :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
    -> FilePath -> IO ()
viz = mkVizToFile fstToDot

vizSST :: FST Int (RangeSet Word8) (WithNull KleenexOutTerm)
       -> FilePath -> IO ()
vizSST f p = let s = sstFromFST f False
             in mkVizToFile sstToDot s p
