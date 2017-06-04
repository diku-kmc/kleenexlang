{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module KMC.Simulization where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Word
import           Data.Maybe (fromMaybe)
import           Text.Printf

import           KMC.SymbolicFST
import           KMC.SymbolicFST.Transducer
import           KMC.Kleenex.Actions
import           KMC.Kleenex.Syntax
import           KMC.Theories (Function(..))
import           KMC.RangeSet
import Debug.Trace


data Out =
  Sym Word8
  | Reg RegAction
  | Empty
  deriving (Show)


data TState st =
  Choice st st
  | Skip st Out
  | Symbol st [(Word8, Word8)] (Maybe Out) -- Nothing means to copy input. The Word8 in maybe not currently used
  | Accept
  | Set st Int
  | Test st
  deriving (Show)

type IState st = (st, TState st)

type NFST st = (st, [IState st])

type Tr = Transducer Int Word8 (Either Word8 RegAction)

debug = flip trace

-- | Substitute state type by any enumerable type.
enumerateNFST :: (Ord st, Enum a, Show st, Show a) => NFST st -> NFST a
enumerateNFST (initial, states) = (aux initial, [ (aux q, saux state) | (q, state) <- states ])
  where
    statesMap = M.fromList (zip (map fst states) [toEnum 0..])
    aux q = statesMap M.! q

    saux (Choice t1 t2) = Choice (aux t1) (aux t2)
    saux (Skip t o)     = Skip (aux t) o
    saux s@(Symbol t r o) = Symbol (aux t) r o
    saux (Accept)       = Accept
    saux (Set t k)      = Set (aux t) k
    saux (Test t)       = Test (aux t)

constructNFST
  :: (Rng (CopyFunc a [Either Word8 RegAction]) ~ [Either Word8 RegAction], a ~ Word8) => RProg a (Either Word8 RegAction) -> RIdent -> ([RIdent], [([RIdent], TState [RIdent])])
constructNFST rprog initial = ([initial], allTransitions)
  where
    (allStates, allTransitions) = go (S.singleton [initial]) S.empty []
    go ws states trans
      | S.null ws                         = (states, trans)
      | (q, ws') <- S.deleteFindMin ws, S.member q states
                                          = go ws' states trans
      | ([], ws') <- S.deleteFindMin ws   = go ws' (S.insert [] states) (([], Accept):trans)
      | (i:is, ws') <- S.deleteFindMin ws =
          let states' = S.insert (i:is) states in
          case getDecl i of
          RConst y      ->
            case y of
            Left w ->
                let q' = follow is
                in go (S.insert q' ws') states' ((i:is, Skip q' (Sym w)):trans)
            _      -> error "RegAction not supported\n"
          RRead p False ->
            let q' = follow is
            in go (S.insert q' ws') states' ((i:is, Symbol q' (ranges p) (Just Empty)):trans)
          RRead p True  ->
            let q' = follow is
            in if q' == [] then go (S.insert q' ws') states' ((i:is, Symbol q' (ranges p) Nothing):trans)
                else go (S.insert q' ws') states' ((i:is, Symbol q' (ranges p) Nothing):trans)
          RSeq js       ->
            let q' = follow (js ++ is)
            in go (S.insert q' ws') states' ((i:is, Skip q' Empty):trans)
          RSum (js : jss : _)       ->
            let q1 = follow (js:is)
                q2 = follow (jss:is)
                -- iNDEXED EPSILONS ARE IMPLICITLY REPRESENTED BY THE TRANSITION order
                trans' = [(i:is, Choice q1 q2)]
            in go (S.union (S.fromList (q1 : [q2])) ws') states' (trans' ++ trans)
          RSum _ -> error "Not normalized"
          -- Temporarily using RegWrite to indicate the Set or Test states, should be removed later.
          RSet k        ->
            let q' = follow is
            in go (S.insert q' ws') states' ((i:is, Set q' k):trans)
          RTest         ->
            let q' = follow is
            in go (S.insert q' ws') states' ((i:is, Test q'):trans)
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

convState :: Tr -> Int -> IState Int
convState fst' k = let i = fromEnum k in
  case M.lookup k (eForward (fstE fst')) of
    Just ((p, CopyArg, t) : _) -> (i, Symbol t (ranges p) (Nothing))
    Just ((p, CopyConst (Left w : _), t) : _) -> (i, Symbol t (ranges p) (Just (Sym w)))
    Just ((p, CopyConst (Right r : _), t) : _) -> (i, Symbol t (ranges p) (Just (Reg r)))
    Just ((p, CopyConst [], t) : _) -> (i, Symbol t (ranges p) (Just Empty))
    Just []              -> error "No edges for state"
    Nothing -> case M.lookup k (eForwardEpsilon (fstE fst')) of
      Just (((Left w : _), t) : [])  -> (i, Skip t (Sym w))
      Just (((Right r : _), t) : []) -> (i, Skip t (Reg r))
      Just (([], t) : [])            -> (i, Skip t Empty)
      Just ((_, t1) : (_, t2) : _)   -> (i, Choice t1 t2)
      Just []                        -> error "No epsilon edges for state"
      Nothing                        -> (i, Accept) -- Should be guranteed


fstToNFST :: Tr -> NFST Int
fstToNFST fst' =
  let eFST = enumerateStates fst'
      sts  = map (convState eFST) (S.toList (fstS eFST))
  in (fstI eFST, sts)


stToString :: IState Int -> String
stToString (i, st) =
  case st of
    Choice t1 t2 -> printf "%i C %i %i\n" i t1 t2
    Skip t a       -> let pe = printf "%i S %i " i t in
                        case a of
                            (Sym w) -> pe ++ (printf "W %i\n" w)
                            (Reg r) -> pe ++ (printf "R %s\n" (show r))
                            Empty   -> pe ++ "E\n"
    Symbol t p a -> let pe = printf "%i R %i %i %s " i t (length p) (show p) in
                        case a of
                            Nothing -> pe ++ "C\n"
                            (Just (Sym w)) -> pe ++ (printf "W %i\n" w)
                            (Just (Reg r)) -> pe ++ (printf "R %s\n" (show r))
                            (Just Empty)   -> pe ++ "E\n"
    Accept       -> printf "%i A\n" i
    Set t k      -> printf "%i I %i %i\n" i t k
    Test t       -> printf "%i T %i\n" i t

nfstToCsv :: NFST Int -> String
nfstToCsv (i, nfst') = let
  line1 = printf "%i %i\n" (length nfst') i
  sts   = map stToString nfst'
  in line1 ++ foldl (++) "" sts
