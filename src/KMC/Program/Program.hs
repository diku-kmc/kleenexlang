{-# LANGUAGE RankNTypes #-}

module KMC.Program.Program where


-- import           TreeWriter -- Control.Monad.Tree
import           Data.Char (ord)
import           Data.List (sortBy)
import qualified Data.Map as M
import           Data.Ord (comparing)
import qualified Data.Set as S
import           Data.Word (Word8, Word64)
import           Numeric (showHex)
-- import           KMC.Automata.AugmentedSymbolicThompson
import           SymbolicSST

import           KMC.Syntax.ByteClass
import           KMC.Syntax.Bitcode


{------------------------------------------------------------------------------}
{-- Programs --}

type BufferId = Int
type BlockId  = Int
type ClassIx  = Int

data Prog = Prog { progBuffers :: [BufferId]
                 , progBlocks  :: M.Map BlockId [PInstr]
                 , progInit    :: BlockId
                 , progClasses :: [(ClassIx, M.Map Word8 Word8)]
                 }
  deriving (Eq, Ord, Show)

data PInstr = PNext (Maybe BlockId) -- (Either BlockId Bool)
            | PResetBuf BufferId
            | PAppendBuf BufferId BufferId
            | PAppendConst BufferId
                           Word64 -- ^ Data
                           Int    -- ^ Number of bits
            | PAppendCode BufferId ClassIx
            | PWriteBuf BufferId
            | PWriteCode ClassIx
            | PWriteConst Word64 Int
            | PCJump [(Word8, Word8)] BlockId
            | PJump BlockId
            | PFail
  deriving (Eq, Ord, Show)

-- type PathTree a = Maybe (Tree [Term] a)
-- data SST s =
--   SST { sstStates        :: S.Set s
--       , sstInitialState  :: s
--       , sstAcceptStates  :: S.Set s
--       , sstInitialHeap   :: HeapUpdate
--       , sstTrans         :: TransMap s
--       }
--   deriving (Eq, Ord, Show)
-- {------------------------------------------------------------------------------}
-- {------------------------------------------------------------------------------}

sstVars :: SST st pred func var delta -> S.Set Var
sstVars = sstV

-- | The initial heap action of an SST is the heap where all registers are empty.
sstInitialHeapAction :: SST st pred func var delta -> RegisterUpdate var func
sstInitialHeapAction sst = M.fromList [ (x, const []) | x <- S.toList (sstV sst) ]

sstInitialState :: SST st pred func var delta -> st
sstInitialState = sstI

sstTrans :: SST st pred func var delta -> M.Map st [(pred, RegisterUpdate var func, st)]
sstTrans = sstE
-- sstClasses :: SST s -> S.Set ByteClass
-- sstClasses sst = S.fromList [ bc | TCode bc <- terms ]
--     where
--       terms =
--         concat (M.elems (sstInitialHeap sst))
--         ++
--         do { (u, _) <- M.elems (sstTrans sst); concat (M.elems u) }

          
sstToProg :: (Ord st, Ord var, EffBoolean pred dom, Function func dom [Either var delta])
          => SST st pred func var delta -> Prog
sstToProg sst =
  Prog { progBuffers = M.elems buffersMap
       , progInit    = blocksMap M.! initAction
       , progBlocks  = M.fromList [ (bid, compileAction a) | (a, bid) <- M.toList blocksMap ]
       , progClasses = classes
       }
  where
    initAction = (sstInitialHeapAction sst, sstInitialState sst)

    buffersMap = M.fromList $
                 zip ( tail -- Strip away the first (smallest = output) buffer
                     $ sort $ S.toList $ sstVars sst
                     )
                 [0..]
--    buffersMap = M.fromList $ zip (filter (/=VNil) $ S.toList $ sstVars sst) [0..]
    
    classesMap :: M.Map ByteClass ClassIx
    classesMap = M.fromList $ zip (S.toList $ sstClasses sst) [0..]

    classes :: [(ClassIx, M.Map Word8 Word8)]
    classes = map snd $ sortBy (comparing fst)
              [ (cid, (bitWidth $ bcSize bc, tbl))
              | (bc, cid) <- M.toList classesMap
              , let tbl = M.fromList
                          [ (c, r)
                          | c <- [0..255]
                          , let r = if inByteClass bc c then
                                      (bcEncode bc c) * 2^(8 - bitWidth (bcSize bc))
                                    else 0]]

    --actions :: [(HeapUpdate, s)]
    --actions = S.toList $ S.fromList (initAction : M.elems (sstTrans sst))

    -- the first one is always the initial one
    actions :: [(RegisterUpdate var func, st)]
    actions = initAction : map (\(_,a,b) -> (a,b)) $ M.elems (sstE sst)
    
    blocksMap :: M.Map (RegisterUpdate var func, st) BlockId
    blocksMap = M.fromList $ zip actions [0..]

    compileAction :: (RegisterUpdate var func, st) -> [PInstr]
    compileAction (m, s) = concatMap compileAssignment (M.toList m)
                           ++ compileTest s

    compileAssignment (VNil, TVar v' : xs)
      | v' /= VNil = compileAssignment (VNil, TVar VNil : TVar v' : xs)
    compileAssignment (v, TVar v' : xs)
      | v /= v' = PResetBuf (buffersMap M.! v)
                : compileAssignment (v, TVar v : TVar v' : xs)
    compileAssignment (_, [TVar _]) = []
    compileAssignment (v, TVar _:TVar v':_)
      | v == v' = error $ "Prepend or copy expression unsupported."

    compileAssignment (VNil, TVar VNil:TVar v':xs) =
      PWriteBuf (buffersMap M.! v')
      : compileAssignment (VNil, TVar VNil:xs)
    compileAssignment (v, TVar _:TVar v':xs) =
      PAppendBuf (buffersMap M.! v) (buffersMap M.! v')
      : compileAssignment (v, TVar v:xs)

    compileAssignment (VNil, TVar VNil:TCode bc:xs) =
      PWriteCode (classesMap M.! bc)
      : compileAssignment (VNil, TVar VNil:xs)
    compileAssignment (v, TVar _:TCode bc:xs) =
      PAppendCode (buffersMap M.! v) (classesMap M.! bc)
      : compileAssignment (v, TVar v:xs)

    compileAssignment (VNil, TVar VNil : xs) =
      let (bs, xs') = bitPrefix [] xs
      in [ PWriteConst cnst l | (cnst, l) <- toConstants bs ]
         ++ compileAssignment (VNil, TVar VNil : xs')
    compileAssignment (v, TVar _ : xs) =
      let (bs, xs') = bitPrefix [] xs
      in [ PAppendConst (buffersMap M.! v) cnst l | (cnst, l) <- toConstants bs ]
         ++ compileAssignment (v, TVar v : xs')

    compileAssignment (VNil, xs) =
      compileAssignment (VNil, TVar VNil:xs)
    compileAssignment (v, xs) =
      PResetBuf (buffersMap M.! v)
      : compileAssignment (v, TVar v:xs)

    bitPrefix bs (TConst b : xs) = bitPrefix (b:bs) xs
    bitPrefix bs xs = (reverse bs, xs)

    toConstants :: [Bool] -> [(Word64, Int)]
    toConstants [] = []
    toConstants xs = let (bs, xs') = splitAt 64 xs
                         n = fst $ foldr (\b (acc, e) -> (acc + if b then 2^e else 0, e+1)) (0,0::Int) bs
                     in (n * 2^(64 - length bs), length bs):toConstants xs'

    testsMap :: M.Map s [(ByteClass, HeapUpdate, s)]
    testsMap = M.fromListWith (++)
                 [ (s, [(bc, u, s')]) | ((s, Just bc), (u, s')) <- M.toList (sstTrans sst) ]

    compileTest :: s -> [PInstr]
    compileTest s = [PNext $
                     --if S.member s (sstAcceptStates sst) then
                     case M.lookup s (sstF sst) of
                       Just upd -> maybe Nothing Just $
                                   (M.lookup s (sstE sst)
                                   >>= flip M.lookup blocksMap)
                    ]
                    ++ (reverse $ fst $ foldl aux ([], byteClass [])
                                                  (maybe [] id (M.lookup s testsMap)))
                    ++ [PFail]
        where
          aux (ps, bcAcc) (bc, u, s') =
            let bc' = bcUnion bcAcc bc
                rs = if length (bcRanges bc') < length (bcRanges bc) then
                       bcRanges bc'
                     else
                       bcRanges bc
                bid = blocksMap M.! (u, s')
            in (PCJump rs bid:ps, bc')





-- -- | Run a streaming string transducer.
-- run :: forall s. (Show s, Ord s) => SST s -> String -> Bitcode
-- run sst inp =
--   Bitcode $ go (sstInitialState sst) (update Nothing M.empty (sstInitialHeap sst)) inp
--   where
--     go :: s -> M.Map Var [Bool] -> String -> [Bool]
--     go q s [] =
--       case M.lookup (q, Nothing) (sstTrans sst) of
--         Nothing -> if S.member q (sstAcceptStates sst) then
--                       s M.! VNil
--                    else
--                       error "reject: not an accept state"
--         Just (u, q') -> if q' == q then s M.! VNil else go q' (update Nothing s u) []
--     go q s (x:xs) =
--       case findTrans q (fromIntegral $ ord x) of
--         Nothing -> error $ "reject: no transition from " ++ show q
--         Just (u, q') -> let w = Just $ fromIntegral $ ord x in go q' (update w s u) xs

--     findTrans :: s -> Word8 -> Maybe (HeapUpdate, s)
--     findTrans qs w = listToMaybe $ do
--       ((q, Just bc), r) <- M.toList (sstTrans sst)
--       guard $ qs == q
--       guard $ inByteClass bc w
--       return r

--     update :: Maybe Word8 -> M.Map Var [Bool] -> M.Map Var [Term] -> M.Map Var [Bool]
--     update w s u = M.union (M.map (flip (valuate w s) []) u) s

--     valuate :: Maybe Word8 -> M.Map Var [Bool] -> [Term] -> [Bool] -> [Bool]
--     valuate _ _ [] c = c
--     valuate w s (TConst b:xs) c = b:valuate w s xs c
--     valuate w s (TVar v:xs) c = s M.! v ++ valuate w s xs c
--     valuate (Just w) s (TCode bc:xs) c = 
--         let i = bcEncode bc w
--             bits = unpack $ codeInt (fromIntegral i) (bitWidth $ bcSize bc)
--         in bits ++ valuate (Just w) s xs c
--     valuate Nothing _ (TCode _:_) _ = error $ "Code expression occurred in empty input transition"
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{-- Testing --}

-- initTree nfa = abstract' nfa $ runClosure (closure nfa (nfaInitial nfa))
-- sgl a = let x = fromIntegral (ord a) in byteClass [(x, x)]

-- anNFA = unsafeConstructNFA "a*b|a*c"

{-
anNFA2 = unsafeConstructNFA "(aaa|aa)*"

anNFA3 = unsafeConstructNFA "((\n)?([a-zA-Z]*|[0-9]*))*"

anNFA4 = unsafeConstructNFA "[a-z]"

anNFA5 = unsafeConstructNFA "(auto|break|case|char|const|continue|default|do|double|else|enum|extern|float|for|goto|if|int|long|register|return|short|signed|sizeof|static|struct|switch|typedef|union|unsigned|void|volatile|while|.*)*"



-------
(u0,o0,t0) = abstract' anNFA3  (initTree anNFA3)
(u1,o1,t1) = abstract' anNFA3 $ stepTree anNFA3 (sgl 'a') t0
(u2,o2,t2) = abstract' anNFA3 $ stepTree anNFA3 (sgl 'b') t1
(u3,o3,t3) = abstract' anNFA3 $ stepTree anNFA3 (sgl 'c') t2
(u4,o4,t4) = abstract' anNFA3 $ stepTree' anNFA3 Nothing t3

t0' = initTree anNFA3
t1' = stepTree anNFA3 (sgl 'a') t0'
t2' = stepTree anNFA3 (sgl 'b') t1'
t3' = stepTree anNFA3 (sgl 'c') t2'
t4' = stepTree' anNFA3 Nothing t3'
-}
