{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
module KMC.SymbolicAcceptor where

import qualified Data.Set as S
import qualified Data.Map as M

import           KMC.SymbolicFST
import           KMC.OutputTerm (NullFun(..))

-- | An `Acceptor` is a finite automaton, i.e., a machine that either
-- accepts its input or fails.  It could equivalently be formulated
-- as a special instance of the FST data type (in KMC.SymbolicFST)
-- with a special "null funcion" as its edge function.  This way is
-- simpler, and the overhead is minimal.
-- The acceptor takes the `edgeset` as a parameter, allowing for different
-- types of edge representations including or excluding epsilon edges.
-- The type parameter `init` lets one express that the initial state
-- can be a set of states or not.
data Acceptor init edgeset st pred = Acceptor { accS :: S.Set st
                                              , accE :: edgeset st pred
                                              , accI :: init st
                                              , accF :: S.Set st
                                              }
-- | NFA edges either have a predicate or not, the latter being an epsilon edge.
-- The order of transitions does not matter in an acceptor, so we keepthem
-- in a Data.Set.
data NFAEdgeSet st pred =
    NFAEdgeSet { nfaForward         :: M.Map st (S.Set (pred, st))
               , nfaBackward        :: M.Map st (S.Set (pred, st))
               , nfaForwardEpsilon  :: M.Map st (S.Set st) -- [st]
               , nfaBackwardEpsilon :: M.Map st (S.Set st) -- [st]
               }
-- | DFA edges must all have a predicate.  Note that we do not check that
-- the predicates are non-overlapping.  This is ensured by the construction
-- nfaToDFA by the use of `coarsestPredicateSet`.
-- As for the NFA, the transitions are unordered.
data DFAEdgeSet st pred =
    DFAEdgeSet { dfaForward  :: M.Map st (S.Set (pred, st))
               , dfaBackward :: M.Map st (S.Set (pred, st))
               }

deriving instance (Eq q, Eq p)                   => Eq (NFAEdgeSet q p)
deriving instance (Ord q, Ord p)                 => Ord (NFAEdgeSet q p)
deriving instance (Show q, Show p)               => Show (NFAEdgeSet q p)
         
deriving instance (Eq q, Eq p)                   => Eq (DFAEdgeSet q p)
deriving instance (Ord q, Ord p)                 => Ord (DFAEdgeSet q p)
deriving instance (Show q, Show p)               => Show (DFAEdgeSet q p)
         
deriving instance (Eq (i q), Eq (e q p), Eq q, Eq p)       => Eq (Acceptor i e q p)
deriving instance (Ord (i q), Ord (e q p), Ord q, Ord p)    => Ord (Acceptor i e q p)
deriving instance (Show (i q), Show (e q p), Show q, Show p) => Show (Acceptor i e q p)

newtype Single a = Single { unSingle :: a } deriving (Eq, Ord, Show)

-- | An NFA has a set of initial states, and it uses the NFAEdgeSet.
newtype NFA st pred = NFA (Acceptor S.Set NFAEdgeSet st pred)
    deriving (Eq, Ord, Show)
-- | A DFA may only have one single initial state, and it uses the DFAEdgeSet.
newtype DFA st pred = DFA (Acceptor Single DFAEdgeSet st pred)
    deriving (Eq, Ord, Show)

-- | An edge in an NFA either requires a predicate on the next input
-- symbol or not, depending on whether it is an epsilon-edge.
type NFAEdge st pred = (st, Maybe pred, st)

-- | An edge in a DFA must always apply a predicate on the next input symbol.
type DFAEdge st pred = (st, pred, st)

toMap :: (Ord k, Ord vs) => [(k, S.Set vs)] -> M.Map k (S.Set vs)
toMap = M.fromListWith S.union
    
nfaEdgesFromList :: (Ord pred, Ord st) => [NFAEdge st pred] -> NFAEdgeSet st pred
nfaEdgesFromList es =
  NFAEdgeSet
  { nfaForward         = toMap [ (q, sing (p, q')) | (q, Just p,  q') <- es ]
  , nfaBackward        = toMap [ (q', sing (p, q)) | (q, Just p,  q') <- es ]
  , nfaForwardEpsilon  = toMap [ (q, sing q')      | (q, Nothing, q') <- es ]
  , nfaBackwardEpsilon = toMap [ (q', sing q)      | (q, Nothing, q') <- es ]
  }
    where
      sing = S.singleton


nfaEdgesToList :: (Ord st) => NFAEdgeSet st pred -> [NFAEdge st pred]
nfaEdgesToList es =
    [ (q, Just p, q')  | (q, ns) <- M.toList (nfaForward es)
                       , (p, q') <- S.toList ns ]
    ++
    [ (q, Nothing, q') | (q, ns) <- M.toList (nfaForwardEpsilon es)
                       , q'      <- S.toList ns ]

dfaEdgesFromList :: (Ord pred, Ord st) => [DFAEdge st pred] -> DFAEdgeSet st pred
dfaEdgesFromList es =
  DFAEdgeSet
  { dfaForward  = toMap [ (q, sing (p, q')) | (q, p, q') <- es ]
  , dfaBackward = toMap [ (q', sing (p, q)) | (q, p, q') <- es ]
  }
    where
      sing = S.singleton

dfaEdgesToList :: (Ord st) => DFAEdgeSet st pred -> [DFAEdge st pred]
dfaEdgesToList es = [ (q, p, q') | (q, ns) <- M.toList (dfaForward es)
                                 , (p, q') <- S.toList ns ]

dfaEdgesToNFAEdges :: (Ord pred, Ord st)
                   => DFAEdgeSet st pred -> NFAEdgeSet st pred
dfaEdgesToNFAEdges es =
    nfaEdgesFromList [ (q, Just p, q') | (q, p, q') <- dfaEdgesToList es ]

flipNFAEdges :: (Ord st) => NFAEdgeSet st pred -> NFAEdgeSet st pred
flipNFAEdges nes =
  NFAEdgeSet
  { nfaForward         = nfaBackward nes
  , nfaBackward        = nfaForward nes
  , nfaForwardEpsilon  = nfaBackwardEpsilon nes
  , nfaBackwardEpsilon = nfaForwardEpsilon nes
  }

-- | Reversing a DFA yields an NFA.
reverseDFA :: (Ord pred, Ord st) => DFA st pred -> NFA st pred
reverseDFA (DFA dfa) =
  NFA $ Acceptor
  { accS = accS dfa
  , accE = flipNFAEdges $ dfaEdgesToNFAEdges $ accE dfa
  , accI = accF dfa -- The final states become initial states
  , accF = S.singleton $ unSingle $ accI dfa
  }

enumerateDFAStatesFrom :: (Ord pred, Ord a, Enum a, Ord st)
                       => a -> DFA st pred -> DFA a pred
enumerateDFAStatesFrom enumFrom (DFA dfa) =
  DFA $ Acceptor
  { accS = S.fromList (M.elems stm)
  , accE = conv (accE dfa)
  , accI = Single $ no (unSingle $ accI dfa)
  , accF = S.map no (accF dfa)
  }
    where
      conv = dfaEdgesFromList . map (\(q,p,q') -> (no q, p, no q')) . dfaEdgesToList
      stm = M.fromList $ zip (S.toList (accS dfa)) [enumFrom..]
      no q = maybe (error "Could not find state in statemap; something's wrong!")
                   id $ M.lookup q stm

-- | Rename DFA states from 0.
enumerateDFAStates :: (Ord pred, Ord a, Enum a, Ord st)
                   => DFA st pred -> DFA a pred
enumerateDFAStates = enumerateDFAStatesFrom (toEnum 0)
    
dfaAsFST :: (Enum st, Ord st) => DFA st pred -> FST st pred (NullFun a b)
dfaAsFST (DFA dfa) = 
    FST { fstS = accS dfa
        , fstE = edgeSet 
        , fstI = unSingle $ accI dfa
        , fstF = accF dfa
        }
    where
      toMap   = M.fromListWith (++)
      edges   = dfaEdgesToList (accE dfa)
      edgeSet =
          OrderedEdgeSet { eForward  = toMap [ (q, [(p, NullFun, q')]) | (q, p, q') <- edges ]
                         , eBackward = toMap [ (q', [(p, NullFun, q)]) | (q, p, q') <- edges ]
                         , eForwardEpsilon  = M.empty
                         , eBackwardEpsilon = M.empty
                         }
