{-# LANGUAGE DeriveFunctor #-}
module KMC.Automata.StreamingStringTransducer
(NFATree
,Var(..)
,Term(..)
,SST(..)
,SSTConf(..)
,HeapUpdate
,TransMap
,PathTree
,sstAnalyze
,genSstConstruct
,genSstConstruct'
,sstEnumerate
,sstVars
,sstClasses
,sstPretty
,sstOptimize
,sstApplyEnvironment
,resume'
,eof
,eof'
,visit

,Closure
,abstract
,abstract'
,stepTree'
,closure
,runClosure
) where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.TreeWriter
import           Data.List (sort,intercalate)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import qualified Data.Set as S
import           KMC.Automata.AugmentedSymbolicThompson hiding (step)
import           KMC.Syntax.Bitcode
import           KMC.Syntax.Internal

type NFATree = Tree [Term]

{------------------------------------------------------------------------------}
{-- Output data --}

-- | A variable uniquely identifies a specific edge in a path tree.
data Var = VNil | VLeft Var | VRight Var
  deriving (Eq, Ord)

data Term = TVar Var | TConst Bool | TCode ByteClass
  deriving (Eq, Ord, Show)

codeInput :: ByteClass -> ByteClass -> [Term]
codeInput bc bcIn
    | bcSize bcIn == 1 =
        let i = bcEncode bc (bcRepresentative bcIn)
            bits = unpack $ codeInt (fromIntegral i) (bitWidth $ bcSize bc)
        in map TConst bits
    | otherwise = [TCode bc]

instance Show Var where
  showsPrec _ VNil s = '*':s
  showsPrec _ (VLeft v) s = shows v ('<':s)
  showsPrec _ (VRight v) s = shows v ('>':s)

{------------------------------------------------------------------------------}
{-- Streaming string transducers --}

type HeapUpdate = M.Map Var [Term]
type TransMap s = M.Map (s, Maybe ByteClass) (HeapUpdate, s)
type PathTree a = Maybe (Tree [Term] a)
data SST s =
  SST { sstStates        :: S.Set s
      , sstInitialState  :: s
      , sstAcceptStates  :: S.Set s
      , sstInitialHeap   :: HeapUpdate
      , sstTrans         :: TransMap s
      }
  deriving (Eq, Ord, Show)

{------------------------------------------------------------------------------}
{-- Closure monad --}

-- | The Closure monad models a bunch of parallel threads with a shared map of
-- visited states.
type Closure s = TreeWriterT [Term] (State (S.Set s))
type NFAClosure = Closure NFAState

-- | Marks a state as visited, but fails if the state is already marked.
-- visit :: NFAState -> Closure NFAState
visit :: (Ord s) => s -> Closure s s
visit q = do { s <- get; when (S.member q s) zero; modify (S.insert q); return q }

-- | Resume a closure computation
resume :: Tree [Term] a -> Closure s a
resume (Tip w x) = tell w >> return x
resume (Fork w t1 t2) = tell w >> plus (resume t1) (resume t2)

-- | Resume a possibly failing closure computation
resume' :: Maybe (Tree [Term] a) -> Closure s a
resume' = maybe zero resume

-- | Interpret the current closure state as a path tree
-- It should hold that resume' . runClosure === runClosure . resume' === id
runClosure :: Closure s a -> Maybe (Tree [Term] a)
runClosure x = evalState (evalTreeWriterT x) S.empty

{------------------------------------------------------------------------------}
{-- Computing path trees --}

{- With all of the machinery for handling failures and early output captured in
the Closure monad, the closure algorithms turn out to be pretty
straightforward! -}

-- | Non-deterministically follow all non-input transitions, writing output
-- bits.
closure :: NFA -> NFAState -> NFAClosure NFAState
closure nfa q =
  case sort <$> M.lookup q (nfaTrans nfa) of
    Just [(LLog _, q')] -> visit q' >> closure nfa q'
    Just [(LWrite b1, q1), (LWrite b2, q2)] ->
        plus (tell [TConst b1] >> closure nfa q1)
             (tell [TConst b2] >> closure nfa q2)
    _ -> return q

-- | Consume a character and compute the closure for the remaining active
-- threads.
step :: NFA
     -> ByteClass -- ^ Abstract representation of a set of possible characters
                      -- to be consumed. The semantics are that a transition is only
                      -- made if the input set is contained in the transition set of
                      -- the current state.
     -> NFAState
     -> NFAClosure NFAState
step nfa bcIn q =
  case M.lookup q (nfaTrans nfa) of
    Just [(LRead bc, q')] | bcSub bcIn bc ->
         tell (codeInput bc bcIn) >> closure nfa q'
    _ -> zero

eof' :: (s -> Bool) -> s -> Closure s s
eof' f q = unless (f q) zero >> return q
           
eof :: NFA -> NFAState -> NFAClosure NFAState
eof nfa q = eof' ((==) (nfaFinal nfa)) q
    --when (nfaFinal nfa /= q) zero >> return q

-- | Consume a character or end of input
step' :: NFA -> Maybe (ByteClass) -> NFAState -> NFAClosure NFAState
step' nfa Nothing = eof nfa
step' nfa (Just bcIn) = step nfa bcIn

stepTree' :: (Maybe a -> s -> Closure s s)
          -> Maybe a
          -> PathTree s
          -> PathTree s
stepTree' stp inp t = runClosure (resume' t >>= stp inp)
                      
{------------------------------------------------------------------------------}
{-- Abstracting path trees --}

{- In general, the number of reachable path trees is not finite. This occurs in
those situations where a constant amount of buffering is not sufficient to
disambiguate between input prefixes of arbitrary length. The number of ordered
state sets is finite, however, and can thus be tabulated. The path tree
structure that is not part of this tabulation is maintained in the heap, and we
equip each transition between ordered state sets with instructions for how to
maintain the heap so the underlying path tree can be recovered. -}

-- | Take a path tree with terms for outputs and replace it with a path tree
-- with variables for outputs and a map describing a simultaneous update of
-- variables in the heap.
abstract :: Tree [Term] a -> (HeapUpdate, Tree [Term] a)
abstract t = go VNil t
  where
  go v (Tip w a) = (M.singleton v w, Tip [TVar v] a)
  go v (Fork w t1 t2) =
    let (m1', t1') = go (VLeft v) t1
        (m2', t2') = go (VRight v) t2
    in (M.insert v w (M.union m1' m2'), Fork [TVar v] t1' t2')

abstract' :: PathTree a -> (HeapUpdate, PathTree a)
abstract' Nothing = (M.empty, Nothing)
abstract' (Just t) = let (m', t') = abstract t in (m', Just t')

data SSTConf s b = SSTConf { scAcceptor :: s -> Bool
                           , scStep     :: Maybe b -> s -> Closure s s
                           , scClosure  :: s -> Closure s s
                           , scAlphabet :: [s] -> [b]
                           , scConvert  :: M.Map (PathTree s, Maybe b) (HeapUpdate, PathTree s)
                                        -> TransMap (PathTree s)
                           }

genSstConstruct :: (Ord a, Ord s, Show s) => SSTConf s a -> s -> SST (PathTree s)
genSstConstruct conf initState = sstOptimize $
  SST { sstStates        = states'
      , sstInitialState  = initTree
      , sstAcceptStates  = S.filter isFinalPathTree states'
      , sstInitialHeap   = initHeap
      , sstTrans         = (scConvert conf) trans'
      }
  where
    isFinalPathTree (Just (Tip _ q)) = (scAcceptor conf) q
    isFinalPathTree _                = False
    
    (initHeap, initTree) = abstract' $ runClosure ((scClosure conf) initState)
    (states', trans')    = saturate (S.singleton initTree) S.empty M.empty

    absStep t inp = abstract' $ stepTree' (scStep conf) inp t
    saturate ws states trans
      | S.null ws = (states, trans)
      | otherwise = 
          let (t, ws') = S.deleteFindMin ws
              qs       = tflat' t
              ts       = [ (inp, m', t')
                         | inp <- Nothing:map Just ((scAlphabet conf) qs)
                         , let (m', t') = absStep t inp
                         , isJust t' ]
              wl'      = S.fromList [ t' | (_, _, t') <- ts ]
              trans''  = M.fromList [ ((t, inp), (m', t'))
                                    | (inp, m', t') <- ts ]
          in if S.member t states then
                 saturate ws' states trans
             else
                 saturate (S.union ws' wl')
                          (S.insert t states)
                          (M.union trans trans'')

genSstConstruct' :: (Ord a, Ord s, Show s) => SSTConf s a -> s -> SST Int
genSstConstruct' c = sstEnumerate . genSstConstruct c

nfa2sst :: NFA -> SSTConf NFAState ByteClass
nfa2sst nfa = SSTConf { scAcceptor = (== nfaFinal nfa)
                      , scStep     = step' nfa
                      , scClosure  = closure nfa
                      , scAlphabet = enumDeterministicClasses nfa
                      , scConvert  = id
                      }

sstConstruct :: NFA -> SST (PathTree NFAState)
sstConstruct nfa = genSstConstruct (nfa2sst nfa) (nfaInitial nfa)


sstConstruct' :: NFA -> SST Int
sstConstruct' nfa = genSstConstruct' (nfa2sst nfa) (nfaInitial nfa)

sstEnumerate :: (Ord s) => SST s -> SST Int
sstEnumerate sst =
  sst { sstStates = S.fromList $ M.elems statesMap
      , sstInitialState = statesMap M.! sstInitialState sst
      , sstAcceptStates = S.map (statesMap M.!) (sstAcceptStates sst)
      , sstTrans = M.fromList trans'
      }
  where
    statesMap = M.fromList $ zip (S.toList (sstStates sst)) [0..]

    trans' = [ ((statesMap M.! src, bc), (kappa, statesMap M.! dst))
               | ((src,bc),(kappa,dst)) <- M.toList (sstTrans sst) ]

sstVars :: SST s -> S.Set Var
sstVars sst = S.unions $ map (S.fromList . M.keys) heapUpdates
  where
    heapUpdates = sstInitialHeap sst : (fst <$> M.elems (sstTrans sst))

sstClasses :: SST s -> S.Set ByteClass
sstClasses sst = S.fromList [ bc | TCode bc <- terms ]
    where
      terms =
        concat (M.elems (sstInitialHeap sst))
        ++
        do { (u, _) <- M.elems (sstTrans sst); concat (M.elems u) }

{------------------------------------------------------------------------------}
{-- Optimization by constant propagation --}

data Val a = Exact a | Ambiguous
  deriving (Eq, Ord, Show, Functor)

isExact :: Val a -> Bool
isExact (Exact _) = True
isExact _ = False

instance Applicative Val where
  pure = Exact
  (Exact f) <*> (Exact x) = Exact (f x)
  _ <*> _ = Ambiguous

lubVal :: (Eq a) => Val a -> Val a -> Val a
lubVal (Exact x) (Exact y) = if x == y then Exact x else Ambiguous
lubVal _ _ = Ambiguous

type Valuation = M.Map Var (Val [Bool])
type Environment s = M.Map s Valuation

lubValuations :: [Valuation] -> Valuation
lubValuations = M.unionsWith lubVal

liftValuation :: Valuation -> [Term] -> Maybe (Val [Bool])
liftValuation _   [] = return (pure [])
liftValuation rho (TVar v:xs) = liftA2 (++) <$> (M.lookup v rho) <*> liftValuation rho xs
liftValuation rho (TConst b:xs) = liftA (b:) <$> liftValuation rho xs
liftValuation _   (TCode _:_) = return Ambiguous

updateValuation :: Valuation -> HeapUpdate -> Valuation
updateValuation rho kappa = M.union (M.mapMaybe (liftValuation rho) kappa) rho

applyValuation :: Valuation -> [Term] -> [Term]
applyValuation _   [] = []
applyValuation rho (TVar v:xs) = case M.lookup v rho of
                                   Just (Exact bs) -> map TConst bs ++ applyValuation rho xs
                                   _ -> TVar v:applyValuation rho xs
applyValuation rho (TConst b:xs) = TConst b:applyValuation rho xs
applyValuation rho (TCode bc:xs) = TCode bc:applyValuation rho xs

updateEnvironment :: (Show s, Ord s) => SST s -> [s] -> Environment s -> (Environment s, S.Set s)
updateEnvironment sst states gamma =
  (M.union updates gamma
  ,S.unions (map (succs M.!) (M.keys updates)))
  where
    predTrans = M.fromListWith (++)
                [ (dst, [(kappa, src)])
                  | ((src, _), (kappa, dst)) <- M.toList (sstTrans sst) ]
    succs = M.fromListWith S.union
            [ (src, S.singleton dst)
              | ((src, _), (_, dst)) <- M.toList (sstTrans sst) ]

    updates = M.unions $ do
      s <- states
      let rho_s = maybe M.empty id (M.lookup s gamma)
      let rho_s' = lubValuations $
                   (if s == sstInitialState sst then
                       updateValuation M.empty (sstInitialHeap sst)
                    else M.empty)
                   :[ maybe M.empty id (M.lookup r gamma) `updateValuation` kappa
                      | (kappa, r) <- maybe [] id (M.lookup s predTrans) ]
      guard (rho_s' /= rho_s)
      return (M.singleton s rho_s')

sstAnalyze :: (Show s, Ord s) => SST s -> Environment s
sstAnalyze sst = go (S.toList $ sstStates sst) M.empty
    where
      go [] gamma = gamma
      go states gamma = let (gamma', states') = updateEnvironment sst states gamma
                        in go (S.toList states') gamma'

sstApplyEnvironment :: (Show s, Ord s) => Environment s -> SST s -> SST s
sstApplyEnvironment gamma sst =
  sst { sstTrans = M.mapWithKey aux (sstTrans sst)
      -- FIXME This also causes immediate output actions to be deleted, as in "Out True :*: In True".
      , sstInitialHeap = foldr M.delete (sstInitialHeap sst) (exactKeys' (sstInitialState sst))
      }
    where
      aux (src, _) (kappa, dst) =
        let srcRho = maybe M.empty id (M.lookup src gamma)
            kappa' = M.map (applyValuation srcRho) $ foldr M.delete kappa (exactKeys dst)
        in (kappa', dst)

      exactKeys s = M.keys $ M.filter isExact $ maybe M.empty id (M.lookup s gamma)
      exactKeys' = filter (/= VNil) . exactKeys

sstOptimize :: (Show s, Ord s) => SST s -> SST s
sstOptimize sst = sstApplyEnvironment (sstAnalyze sst) sst

{------------------------------------------------------------------------------}
{-- "Pretty" printing streaming string transducers (or more like, intelligible
    printing, definitely not pretty). --}

sstPretty :: (Ord s, Show s) => SST s -> String
sstPretty sst = initialString ++ transString
    where
      initialString = "initial: " ++ show (sstInitialState sst) ++ "\n  "
                      ++ (intercalate "  " $ map showUpdate (M.toList $ sstInitialHeap sst))
                      ++ "\n"
      
      transString = intercalate "\n" $ map prettyTrans $ M.toList $ sstTrans sst

      prettyTrans ((s, bc), (u, q)) =
        show s ++ " --" ++ show bc ++ "--> " ++ show q ++ ":\n" ++
        "  " ++ (intercalate "  " $ map showUpdate $ M.toList u)

      showUpdate (v, t) = show v ++ " := " ++ showTerms t ++ "\n"

      showTerms = concatMap showTerm
      showTerm (TConst False) = "0"
      showTerm (TConst True) = "1"
      showTerm x = "(" ++ show x ++ ")"
