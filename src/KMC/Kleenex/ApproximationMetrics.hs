module KMC.Kleenex.ApproximationMetrics where

import qualified Data.HashMap.Strict as HM
import           Data.Word
import           KMC.Kleenex.Actions
import           KMC.Kleenex.Syntax
import qualified KMC.RangeSet as RS

type RTermAct = RTerm Word8 (Either Word8 RegAction)
type RProgAct = RProg Word8 (Either Word8 RegAction)

type Decls = HM.HashMap RIdent RTermAct
data ApproxState = AS { newDecls :: Decls
                      , mappings :: HM.HashMap (RIdent,Int) RIdent
                      , counter ::  Int } deriving (Show)

data ApproxMetric = LCS | Hamming | Levenshtein deriving (Show)
data ApproxMode = Correction | Matching | Explicit deriving (Show, Eq)


------------------
-- EPSILON TERM --
------------------

rewriteEpsilon :: ApproxMetric -> ApproxMode -> (RIdent, RIdent) -> ApproxState ->
                  ApproxState

--
-- Hamming Rewrites
--
rewriteEpsilon Hamming _ (rid,_) (AS n m c) =
  AS (HM.insert rid (RSeq []) n) m c

--
-- LCS and Levenshtein Rewrites
--
rewriteEpsilon _ Explicit (rid,frid) (AS n m c) =
  AS (HM.union newDecls n) m $ c + 5
  where
    newDecls = HM.fromList [(rid, RSum   [c+3, c+4])
                           ,(c  , RConst (Left 68)) -- Delete
                           ,(c+1, RSeq   [c+2, frid])
                           ,(c+2, RRead  RS.universe True)
                           ,(c+3, RSeq   [c, c+1])
                           ,(c+4, RSum   [])]
rewriteEpsilon _ mode (rid,frid) (AS n m c) =
  AS (HM.union newDecls n) m $ c + 3
  where
    newDecls = HM.fromList [(rid, RSum  [c+1, c+2])
                           ,(c  , RRead RS.universe (mode == Matching))
                           ,(c+1, RSeq  [c, frid])
                           ,(c+2, RSeq  [])]

---------------
-- READ TERM --
---------------

rewriteRead :: ApproxMetric -> ApproxMode -> (RIdent, RIdent, RIdent, RIdent) ->
               RTermAct -> ApproxState -> ApproxState
--
-- LCS Rewrites
--
rewriteRead LCS Explicit (rid,rid',frid,frid') rt@(RRead rs _) (AS n m c) =
  AS (HM.union newElems n) m $ c+12
  where
    newElems = HM.fromList
      [ (rid, RSum [c, c+1])
      , (c,   RSeq [c+11, rid'] )
      , (c+1, RSum [c+2, c+6])
      , (c+2, RSeq [c+4,c+8])
      , (c+3, RRead RS.universe True)
      , (c+4, RConst (Left 73))    -- Insert
      , (c+5, RConst (Left 68))    -- Delete
      , (c+6, RSeq [c+5, c+10])
      , (c+7, RSeq [c+4, c+8 ])
      , (c+8, RSeq [c+9, frid'])
      , (c+9, RConst (Left (RS.findMin rs))) -- Some char to insert..
      , (c+10,RSeq [c+3, frid])
      , (c+11,rt)]
rewriteRead LCS Correction (rid,rid',frid,frid') rt@(RRead rs True) (AS n m c) =
  AS (HM.union newElems n) m $ c+7
  where
    newElems = HM.fromList
      [ (rid, RSum [c, c+2])
      , (c  , RSeq [c+1, rid'])
      , (c+1, rt)
      , (c+2, RSum [c+3, c+5])
      , (c+3, RSeq [c+4, frid])
      , (c+4, RRead RS.universe False)
      , (c+5, RSeq [c+6, frid'] )
      , (c+6, RConst (Left (RS.findMin rs)))]
rewriteRead LCS mode (rid,rid',frid,frid') rt@(RRead rs out) (AS n m c) =
  AS (HM.union newElems n) m $ c+5
  where
    newElems = HM.fromList
      [ (rid, RSum [c, c+2])
      , (c  , RSeq [c+1, rid'])
      , (c+1, rt)
      , (c+2, RSum [c+3, frid'])
      , (c+3, RSeq [c+4, frid])
      , (c+4, RRead RS.universe ((mode == Matching) && out))]
--
-- Hamming Rewrites
--
rewriteRead Hamming Explicit (rid,rid',_,frid') rt@(RRead rs _) (AS n m c) =
  AS (HM.union newElems n) m $ c+8
  where
    newElems = HM.fromList
      [ (rid, RSum [c, c+2])
      , (c  , RSeq [c+1, rid'])
      , (c+1, rt)
      , (c+2, RSeq [c+3, c+4])
      , (c+3, RRead RS.universe True)
      , (c+4, RSeq [c+5, c+6])
      , (c+5, RConst (Left 82)) -- Replace
      , (c+6, RSeq [c+7, frid'])
      , (c+7, RConst (Left (RS.findMin rs))) ]
rewriteRead Hamming Correction (rid,rid',_,frid') rt@(RRead rs True) (AS n m c) =
  AS (HM.union newElems n) m $ c+6
  where
    newElems = HM.fromList
      [ (rid, RSum [c, c+2])
      , (c  , RSeq [c+1, rid'])
      , (c+1, rt)
      , (c+2, RSeq [c+3, c+4])
      , (c+3, RRead RS.universe False)
      , (c+4, RSeq [c+5, frid'])
      , (c+5, RConst (Left (RS.findMin rs)))]
rewriteRead Hamming mode (rid,rid',_,frid') rt@(RRead rs out) (AS n m c) =
  AS (HM.union newElems n) m $ c+4
  where
    newElems = HM.fromList
      [ (rid, RSum [c, c+2])
      , (c  , RSeq [c+1, rid'])
      , (c+1, rt)
      , (c+2, RSeq [c+3, frid'])
      , (c+3, RRead RS.universe ((mode == Matching) && out))]
--
-- Levenshtein Rewrites
--
rewriteRead Levenshtein Explicit (rid,rid',frid,frid') rt@(RRead rs _) (AS n m c) =
  AS (HM.union newElems n) m $ c+18
  where
    newElems = HM.fromList
      [ (rid, RSum [c, c+2])
      , (c,   RSeq [c+1, rid'] )
      , (c+1, rt)
      , (c+2, RSum [c+3,c+9])
      , (c+3, RSeq [c+4,c+5])
      , (c+4, RRead RS.universe True)
      , (c+5, RSeq [c+6, c+7])
      , (c+6, RConst (Left 82)) -- Replace
      , (c+7, RSeq [c+8, frid'])
      , (c+8, RConst (Left (RS.findMin rs)))
      , (c+9, RSum [c+10, c+14])
      , (c+10,RSeq [c+11, c+12])
      , (c+11,RConst (Left 68)) -- Delete
      , (c+12,RSeq [c+13, frid])
      , (c+13,RRead RS.universe True)
      , (c+14,RSeq [c+15,c+16])
      , (c+15,RConst (Left 73)) -- Insert
      , (c+16,RSeq [c+17, frid'])
      , (c+17,RConst (Left (RS.findMin rs)))]
rewriteRead Levenshtein Correction (rid,rid',frid,frid') rt@(RRead rs True) (AS n m c) =
  AS (HM.union newElems n) m $ c+12
  where
    newElems = HM.fromList
      [ (rid, RSum [c, c+2])
      , (c,   RSeq [c+1, rid'] )
      , (c+1, rt)
      , (c+2, RSum [c+3,c+7])
      , (c+3, RSeq [c+4,c+5])
      , (c+4, RRead RS.universe False)
      , (c+5, RSeq [c+6, frid'])
      , (c+6, RConst (Left (RS.findMin rs)))
      , (c+7, RSum [c+8, c+10])
      , (c+8, RSeq [c+9, frid])
      , (c+9, RRead RS.universe False)
      , (c+10,RSeq [c+11, frid'])
      , (c+11,RConst (Left (RS.findMin rs)))]
rewriteRead Levenshtein mode (rid,rid',frid,frid') rt@(RRead rs out) (AS n m c) =
  AS (HM.union newElems n) m $ c+8
  where
    newElems = HM.fromList
     [ (rid, RSum [c, c+2])
     , (c,   RSeq [c+1, rid'])
     , (c+1, rt)
     , (c+2, RSum [c+3, c+5])
     , (c+3, RSeq [c+4, frid'])
     , (c+4, RRead RS.universe ((mode == Matching) && out))
     , (c+5, RSum [c+6, frid'])
     , (c+6, RSeq [c+7, frid])
     , (c+7, RRead RS.universe ((mode == Matching) && out))]
