{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module KMC.Simulization where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Word
import           Data.Maybe (fromMaybe)
import           Text.Printf

import           KMC.SymbolicFST.Transducer
import           KMC.Kleenex.Actions
import           KMC.Kleenex.Syntax
import           KMC.Theories (Function(..))
import           KMC.RangeSet


data Out a =
  Sym [a]
  | Reg RegAction
  | Empty
  deriving (Show)


data TState st a b =
  Choice st st
  | Skip st (Out a)
  | Symbol st [(a, a)] (Maybe (Out b)) -- Nothing means to copy input. The Word8 in maybe not currently used
  | Accept
  | Set st Int
  | Test st
  deriving (Show)

type NFST st a b = (st, M.Map st (TState st a b))
type RProgNFST = NFST Int Word8 Word8

stateSizeNFST :: NFST st a b -> Int
stateSizeNFST = M.size . snd

condenseSkip :: (Ord st, Show st) => NFST st a b -> NFST st a b
condenseSkip nfst@(start, states) = (start, go states M.empty S.empty)
  where
    go sts nsts del
      | M.null sts = mDelete nsts del
      | ((k, _), sts') <- M.deleteFindMin sts, S.member k del
                                              = go sts' nsts del
      | ((k, e), sts') <- M.deleteFindMin sts =
          case e of
            (Skip t (Sym o)) ->
              let (o', t', del') = follow o t del in
              go sts' (M.insert k (Skip t' (Sym o')) nsts) del'
            (Skip t Empty) ->
              let (o', t', del') = follow [] t del
                  out = if (length o') == 0 then Empty else Sym o'
              in go sts' (M.insert k (Skip t' out) nsts) del'
            _ -> go sts' (M.insert k e nsts) del
    follow symOut i s =
      if (length $ from i) /= 1
      then (symOut, i, s)
      else case getState i of
             (Skip t (Sym o)) -> follow (symOut ++ o) t (S.insert i s)
             _ -> (symOut, i, s)

    getState i =
      fromMaybe (error $ "internal error: target without target state: " ++ show i)
        $ M.lookup i states
    from i =
      fromMaybe (error $ "internal error: target without target state: " ++ show i)
        $ M.lookup i reverseEdges
    reverseEdges = edgesFromNFST nfst
    mDelete m s = M.filterWithKey (\k _ -> not $ S.member k s) m

edgesFromNFST :: (Ord st) => NFST st a b -> M.Map st [st]
edgesFromNFST (_, states) = M.foldrWithKey f M.empty states
  where
    f k (Choice t1 t2) ac = M.unionWith (++) (M.fromList [(t1, [k]), (t2, [k])]) ac
    f k (Skip t _)     ac = ins t k ac
    f k (Symbol t _ _) ac = ins t k ac
    f k (Set t _)      ac = ins t k ac
    f k (Test t)       ac = ins t k ac
    f _ (Accept)       ac = ac

    ins t k ac = M.insertWith (++) t [k] ac

-- | Substitute state type by any enumerable type.
enumerateNFST :: (Ord st, Enum i, Ord i) => NFST st a b -> NFST i a b
enumerateNFST (initial, states) = (aux initial, M.mapKeys aux (M.map saux states))
  where
    statesMap = M.fromList (zip (M.keys states) [toEnum 0..])
    aux q = statesMap M.! q

    saux (Choice t1 t2) = Choice (aux t1) (aux t2)
    saux (Skip t o)     = Skip (aux t) o
    saux (Symbol t r o) = Symbol (aux t) r o
    saux (Accept)       = Accept
    saux (Set t k)      = Set (aux t) k
    saux (Test t)       = Test (aux t)

constructNFST :: (Rng (CopyFunc a [Either a RegAction]) ~ [Either a RegAction], Show a)
                => RProg a (Either a RegAction)
                -> RIdent
                -> (NFST [RIdent] a a)
constructNFST rprog initial = (follow [initial], allTransitions)
  where
    (_, allTransitions) = go (S.singleton (follow [initial])) S.empty M.empty
    go ws states trans
      | S.null ws                         = (states, trans)
      | (q, ws') <- S.deleteFindMin ws, S.member q states
                                          = go ws' states trans
      | ([], ws') <- S.deleteFindMin ws   = go ws' (S.insert [] states) (M.insert [] Accept trans)
      | (i:is, ws') <- S.deleteFindMin ws =
          let states' = S.insert (i:is) states in
          case getDecl i of
          RConst y      ->
            case y of
            Left w ->
                let q' = follow is
                in go (S.insert q' ws') states' (M.insert (i:is) (Skip q' (Sym [w])) trans)
            _      -> error "RegAction not supported\n"
          RRead p False ->
            let q' = follow is
            in go (S.insert q' ws') states'
                  (M.insert (i:is) (Symbol q' (ranges p) (Just Empty)) trans)
          RRead p True  ->
            let q' = follow is
            in go (S.insert q' ws') states' (M.insert (i:is) (Symbol q' (ranges p) Nothing) trans)
          RSeq js       ->
            let q' = follow (js ++ is)
            in go (S.insert q' ws') states' (M.insert (i:is) (Skip q' Empty) trans)
          RSum (js : [jss])       ->
            let q1 = follow (js:is)
                q2 = follow (jss:is)
                -- indexed epsilons are implicitly represented by the transition order
                trans' = M.singleton (i:is) (Choice q1 q2)
            in go (S.union (S.fromList (q1 : [q2])) ws') states' (trans' `M.union` trans)
          RSum _ -> error "Not normalized"
          -- Temporarily using RegWrite to indicate the Set or Test states, should be removed later.
          RSet k        ->
            let q' = follow is
            in go (S.insert q' ws') states' (M.insert (i:is) (Set q' k) trans)
          RTest         ->
            let q' = follow is
            in go (S.insert q' ws') states' (M.insert (i:is) (Test q') trans)
      | otherwise = error "impossible"

    -- Optimization: Reduce number of generated states by contracting
    -- non-deterministic edges with no output. This is done by "skipping" states
    -- whose head nonterminal is declared to be a Seq term, or an RSum with only
    -- one successor.
    follow [] = []
    follow (i:is) =
      case getDecl i of
      RSeq js -> follow (js ++ is)
      RSum [j] -> follow (j:is)
      _ -> i:is

    getDecl i =
      fromMaybe (error $ "internal error: identifier without declaration: " ++ show i)
        $ M.lookup i (rprogDecls rprog)

stToString :: Int -> TState Int Word8 Word8 -> String
stToString i st =
  case st of
    Choice t1 t2 -> printf "%i C %i %i\n" i t1 t2
    Skip t a       -> let pe = printf "%i S %i " i t in
                        case a of
                            (Sym w) -> pe ++ printf "W %i %s\n" (length w) (show w)
                            (Reg r) -> pe ++ printf "R %s\n" (show r)
                            Empty   -> pe ++ "E\n"
    Symbol t p a -> let pe = printf "%i R %i %i %s " i t (length p) (show p) in
                        case a of
                            Nothing -> pe ++ "C\n"
                            (Just (Sym w)) -> pe ++ printf "W %i %s\n" (length w) (show w)
                            (Just (Reg r)) -> pe ++ printf "R %s\n" (show r)
                            (Just Empty)   -> pe ++ "E\n"
    Accept       -> printf "%i A\n" i
    Set t k      -> printf "%i I %i %i\n" i t k
    Test t       -> printf "%i T %i\n" i t

nfstsToCSV :: [RProgNFST] -> String
nfstsToCSV nfsts = let
  line1 = printf "%i\n" (length nfsts) :: String
  line2 = (foldl (\str (start, nfst) -> str ++
    (printf "%i %i " (length nfst) start)) "" nfsts) ++ "\n"
  nfstStr = concat $ map nfstToCSV nfsts
  in line1 ++ line2 ++ nfstStr

nfstToCSV :: RProgNFST -> String
nfstToCSV (_, nfst') = let
  sts   = M.mapWithKey stToString nfst'
  in concat sts
