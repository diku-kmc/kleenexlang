module KMC.Kleenex.Approximation( approxProg
                                , approxProgIt
                                , module AM) where

import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import           KMC.Kleenex.ApproximationMetrics as AM
import           KMC.Kleenex.Core
import           KMC.Kleenex.Syntax

-- | Makes a Reduced Program capable of approximate matching using the k-fold
-- approach
approxProg :: RProgAct -> Int -> Int -> ApproxMetric -> ApproxMode -> RProgAct
approxProg p k offset m mode = prog
  where
    prog = RProg [offset] $ M.fromList $ HM.toList $ newDecls finalState
    finalState = approxStms m mode startState plid decls k
    decls = HM.fromList $ M.toList $ rprogDecls p
    startState = initializeState plid k m mode offset
    plid = head $ rprogPipeline p

-- | Makes a Reduced Program capable of approximation matching using the
-- iterative approach
approxProgIt :: RProgAct -> Int -> Int -> ApproxMetric -> ApproxMode -> RProgAct
approxProgIt p k offset m mode = foldl (\s _ -> approx s) p [1..k]
  where approx s = approxProg s 1 offset m mode

-------------
-- Utility --
-------------

-- | Looks up a mapping
lookupMapping :: RIdent -> Int -> ApproxState -> RIdent
lookupMapping rid k (AS _ m _) =
  case HM.lookup (rid, k) m of
    Just v  -> v
    Nothing -> error $
      "Internal error: Could not find mapping: " ++ (show (rid, k))

-- | Translates a list of points, if mapping already exists, this is returned,
-- otherwise, new pointer
translatePointers :: [RIdent] -> ApproxState -> Int -> ([RIdent], ApproxState)
translatePointers [] s _ = ([],s)
translatePointers (x:xs) s k =
  case HM.lookup (x,k) m of
    Just rid -> (rid:rids, s')
    Nothing  -> (c  :rids, AS n (HM.insert (x, k) c m) (c+1))
  where
    (rids,s'@(AS n m c)) = translatePointers xs s k

-- | Used to create the start RSums between each k paths
addStartSums :: RIdent -> ApproxState -> Int -> ApproxState
addStartSums rid (AS n m c) k = AS n_ m_ $ c + 2
  where
    n_ = HM.insert c (RSum [c+1,c+2]) n
    m_ = HM.insert (rid,k) (c+1) m

-- | Created the k-1 start sums and adds the final mapping
initializeState :: RIdent -> Int -> ApproxMetric -> ApproxMode -> Int -> ApproxState
initializeState rid k metric mode offset = foldl (addEpsEnds metric mode) s [0..k]
  where
    s = AS n (HM.insert (rid, k) c m) $ c + 1
    (AS n m c) = foldl (addStartSums rid) ( AS HM.empty HM.empty offset) [0..k-1]

--------------------
-- Term insertion --
--------------------

-- | Loops through statements and produces the corresponding approximate RProgAct
approxStms :: ApproxMetric -> ApproxMode -> ApproxState -> RIdent -> Decls -> Int -> ApproxState
approxStms m mode s r oldDecls k =
  if HM.member (lookupMapping r 0 s) (newDecls s) then s else
  case t of
    RConst _  -> foldl (insertConst (r, -1) t) s [0..k]
    RRead _ _ -> foldl (insertRead m mode (r, -1) t) s [0..k]
    RSeq [] -> foldl (insertEps r) s [0..k]
    RSeq (rid1:rid2:[]) -> case getDecl rid1 oldDecls of
      term@(RRead _ _) ->
          approxStms m mode (foldl (insertRead m mode (r,rid2) term) s [0..k])
            rid2 oldDecls k
      term@(RConst _ ) ->
          approxStms m mode (foldl (insertConst (r,rid2) term) s [0..k])
            rid2 oldDecls k
      _ -> error $ "Approximation: Sequence must start with const or read. Term: "
           ++ show t ++ " not allowed"
    RSum ids  -> foldl (\st rid -> approxStms m mode st rid oldDecls k)
                 (foldl (insertSum r t) s [0..k]) ids
    _ -> error $ "Cannot Approximation on term like " ++ show t
  where
    t = getDecl r oldDecls

-- | General insertion of term
insertTerm :: RIdent -> RTermAct -> ApproxState -> Int -> ApproxState
insertTerm rids rterm s@(AS n m c) k = AS n_ m c
  where
    n_ = HM.insert point rterm n
    point = lookupMapping rids k s

-- | Insert const term
insertConst :: (RIdent,RIdent) -> RTermAct -> ApproxState -> Int -> ApproxState
insertConst (rid1,rid2) t s k = AS (HM.union newElems n) m $ c+1
  where
    newElems = HM.fromList [ (st, RSeq[c, aft]), (c, t)]
    (st:aft:[], (AS n m c)) = translatePointers [rid1,rid2] s k

-- | Insert Sum
insertSum :: RIdent -> RTermAct -> ApproxState -> Int -> ApproxState
insertSum rid (RSum xs) s k = insertTerm rid (RSum rids') s_ k
  where
    (rids', s_) = translatePointers xs s k
insertSum _ _ _ _ = error "Illegal use of insertSum"

-- | Creates a pointer to the produced epsilon ending
insertEps :: RIdent -> ApproxState -> Int -> ApproxState
insertEps rid s k = insertTerm rid (RSeq [lookupMapping (-1) k s]) s k

-- | Insert a read term
insertRead :: ApproxMetric -> ApproxMode -> (RIdent,RIdent) -> RTermAct -> ApproxState -> Int -> ApproxState
insertRead _ _ (rid1,rid2) rread s 0 = AS (HM.union newElems n) m $ c+1
  where
    newElems = HM.fromList $ [ (st, RSeq [c, aft]), (c , rread)]
    (st:aft:[], (AS n m c)) = translatePointers [rid1, rid2] s 0
insertRead metric mode (rid1,rid2) rread s k =
  let (rid:rid':[], s') = translatePointers [rid1, rid2] s k
      frid = lookupMapping rid1 (k-1) s'
      frid' = lookupMapping rid2 (k-1) s'
  in rewriteRead metric mode (rid,rid',frid,frid') rread s'

-- | Creates k end states, such that error can occur after input sing has
-- finished
addEpsEnds :: ApproxMetric -> ApproxMode -> ApproxState -> Int -> ApproxState
addEpsEnds _ _ s 0 = insertTerm (-1) (RSeq []) (snd (translatePointers [-1] s 0)) 0
addEpsEnds metric mode s k =
  let (rid:[], s') = translatePointers [-1] s k
      frid = lookupMapping (-1) (k-1) s
  in rewriteEpsilon metric mode (rid,frid) s'
