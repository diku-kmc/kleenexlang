{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module KMC.Kleenex.Desugaring (desugarProg,desugarRegex) where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString as BS
import           Data.Char (ord)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Word()
import qualified Data.List as L
import           KMC.Kleenex.Actions
import           KMC.Kleenex.Approximation
import           KMC.Kleenex.Core
import           KMC.Kleenex.Syntax
import qualified KMC.RangeSet as RS
import qualified KMC.Syntax.External as E
import           KMC.Util.UTF8

-------------------
-- Desugaring monad
-------------------

data DesugarState = DS { dsDecls    :: M.Map RIdent RTermAct
                       , dsRevDecls :: M.Map RTermAct RIdent
                       , dsFresh    :: RIdent
                       , dsApprox   :: [(RIdent, RIdent, Int)]
                       }

data DesugarContext = DC { dcIdents   :: M.Map (Ident, Bool) RIdent }

type Desugar = ReaderT DesugarContext (State DesugarState)

getFresh :: MonadState DesugarState m => m RIdent
getFresh = do
  i <- gets dsFresh
  modify $ \ds -> ds { dsFresh = dsFresh ds + 1 }
  return i

insertDecl :: MonadState DesugarState m => RIdent -> RTermAct -> m RIdent
insertDecl i t = do modify $ \ds -> ds { dsDecls    = M.insert i t (dsDecls ds)
                                       , dsRevDecls = M.insert t i (dsRevDecls ds)
                                       }
                    return i

insertApproxDecl :: MonadState DesugarState m => RIdent -> RIdent -> Int -> m RIdent
insertApproxDecl i1 i2 k = do
  _ <- insertDecl i2 $ RSeq [i1]
  modify $ \ds -> ds { dsApprox = (i1,i2,k):(dsApprox ds) }
  return i2

decl :: MonadState DesugarState m => RTermAct -> m RIdent
decl t = do
  rds <- gets dsRevDecls
  case M.lookup t rds of
    Just i -> return i
    Nothing -> do
      i <- getFresh
      insertDecl i t

lookupIdent :: MonadReader DesugarContext m => Ident -> Bool -> m RIdent
lookupIdent ident out =
  asks (M.lookup (ident, out) . dcIdents)
  >>= maybe (error $ "lookupIdent invariant broken: " ++ fromIdent ident) return

--------------------------------
-- Regular expression desugaring
--------------------------------

desugarRE :: Bool        -- ^ Indicates if input symbols are to be written as actions
          -> E.Regex     -- ^ Regular expression to desugar
          -> Int
          -> Desugar RIdent -- ^ Identifier of nonterminal representing desugared RE
desugarRE out re k =
  case re of
  E.One            -> decl $ RSeq []
  E.Dot            -> decl $ RRead RS.universe out
  E.Chr a          -> sequence [ approx_read (RS.singleton b) | b <- encodeChar a ] >>= decl . RSeq
  E.Group _ e      -> desugarRE out e k
  E.Concat e1 e2   -> sequence [ desugarRE out e1 k, desugarRE out e2 k ] >>= decl . RSeq
  E.Branch e1 e2   -> sequence [ desugarRE out e1 k, desugarRE out e2 k ] >>= decl . RSum
  E.Class pos ers  -> let rs  = (if pos then id else RS.complement)
                                $ RS.rangeSet [ (toEnum (ord lo), toEnum (ord hi)) | (lo, hi) <- ers ]
                      in approx_read rs -- decl $ RRead rs out
  E.Star e         -> star e
  E.LazyStar e     -> star e
  E.Plus e         -> do ie <- desugarRE out e k
                         istar <- desugarRE out (E.Star e) k
                         decl $ RSeq [ie, istar]
  E.LazyPlus e     -> do ie <- desugarRE out e k
                         istar <- desugarRE out (E.LazyStar e) k
                         decl $ RSeq [ie, istar]
  E.Question e     -> do ie <- desugarRE out e k
                         ieps <- decl $ RSeq []
                         decl $ RSum [ie, ieps]
  E.LazyQuestion e -> do ie <- desugarRE out e k
                         ieps <- decl $ RSeq []
                         decl $ RSum [ieps, ie]
  E.Range e n m    -> do
    ie <- desugarRE out e k
    case m of
      Nothing -> do istar <- desugarRE out (E.Star e) k
                    decl $ RSeq (replicate n ie ++ [istar])
      Just m' -> if n == m' then
                   decl $ RSeq (replicate n ie)
                 else
                   do iquest <- desugarRE out (E.Question e) k
                      decl $ RSeq (replicate n ie ++ replicate m' iquest)
  E.Suppress e     -> desugarRE False e k
  E.NamedSet _ _   -> error "named sets not supported"
  E.LazyRange{}    -> error "lazy ranges not supported"
  where
    star e = do ie     <- desugarRE out e k
                ieps   <- decl $ RSeq []
                i      <- getFresh
                iloop  <- decl $ RSeq [ie, i]
                insertDecl i $ RSum [ieps, iloop]
    approx_read rs = if k == 0 then
                          decl $ RRead rs out
                     else
                          do end      <- decl $ RSeq []
                             fchoice  <- getFresh
                             test_del <- decl $ RTest
                             delete   <- decl $ RRead RS.universe False
                             r        <- decl $ RRead rs out
                             readc    <- decl $ RSeq [r, end]
                             ff       <- decl $ RSeq [test_del, delete, fchoice]
                             testi    <- decl $ RTest
                             i        <- decl $ RConst (Left $ RS.findMin rs)
                             insert   <- decl $ RSeq [testi, i, end]
                             schoice  <- decl $ RSum [ff, insert]
                             insertDecl fchoice $ RSum [readc, schoice]


---------------------
-- Kleenex desugaring
---------------------

desugarTerm :: Bool           -- ^ Indicates if input symbols are to be written as actions
            -> Bool           -- ^ Indicates use of counters
            -> Int            -- ^ Current allowed approximation error
            -> Term i         -- ^ Term to be desugared
            -> Desugar RIdent -- ^ Identifier of nonterminal representing desugared term
desugarTerm out counters k t =
  case t of
  (Var ident)   -> lookupIdent ident out
  (Constant bs) -> mapM (decl . RConst . Left) (if out then BS.unpack bs else []) >>= decl . RSeq
  (RE e)        -> desugarRE out e k
  (Seq _ _)     -> mapM (desugarTerm out counters k) (flattenSeq t) >>= decl . RSeq
  (Sum _ _)     -> mapM (desugarTerm out counters k) (flattenSum t) >>= decl . RSum
  (Star t1)     -> do it <- desugarTerm out counters k t1
                      ieps <- decl $ RSeq [] -- Contiunue or accept?
                      i <- getFresh
                      iloop <- decl $ RSeq [it, i]
                      insertDecl i $ RSum [iloop, ieps]
  (Plus t1)     -> do it <- desugarTerm out counters k t1
                      istar <- desugarTerm out counters k (Star t1)
                      decl $ RSeq [it, istar]
  (Question t1) -> do it <- desugarTerm out counters k t1
                      ieps <- decl $ RSeq []
                      decl $ RSum [it, ieps]
  (Approx e t1)  -> case (counters, k) of
                        (True, 0)  -> do
                                        i   <- desugarTerm out counters e t1
                                        set <- decl $ RSet e
                                        decl $ RSeq [set, i]
                        (True, _)  -> error "Non!"
                        -- For some reason don't output if outer term is
                        -- SuppressOutput
                        (False, _) -> if not out
                                      then desugarTerm out counters k t1
                                      else do i1 <- desugarTerm out counters k t1
                                              i2 <- getFresh
                                              insertApproxDecl i1 i2 e
  (Range m' n' t1) -> do it <- desugarTerm out counters k t1
                         let m = fromMaybe 0 m'
                         case n' of
                           Nothing -> do
                             istar <- desugarTerm out counters k (Star t1)
                             decl $ RSeq (replicate m it ++ [istar])
                           Just n | n < m -> error $ "invalid range: {" ++ show m ++ "," ++ show n ++ "}"
                                  | n == m -> decl $ RSeq (replicate m it)
                                  | otherwise -> do
                                      iquest <- desugarTerm out counters k (Question t1)
                                      decl $ RSeq (replicate m it ++ replicate (n-m) iquest)
  (SuppressOutput t1) -> desugarTerm False counters k t1
  One                 -> decl $ RSeq []
  UpdateReg r str     -> mapM (decl . RConst) (Right Push
                                               :(str >>= dsUpdateSym)
                                               ++ [Right $ Pop r]) >>= decl . RSeq
  WriteReg r          -> decl $ RConst (Right (Write r))
  RedirectReg r t1    -> do it <- desugarTerm out counters k t1
                            [ipush, ipop] <- mapM (decl . RConst . Right) [Push, Pop r]
                            decl $ RSeq [ipush,it,ipop]
  TermInfo _ t1       -> desugarTerm out counters k t1
  where
    flattenSeq (Seq t1 t2) = flattenSeq t1 ++ flattenSeq t2
    flattenSeq t'          = [t']
    flattenSum (Sum t1 t2) = flattenSum t1 ++ flattenSum t2
    flattenSum t'          = [t']
    dsUpdateSym (Left r)   = [Right $ Write r]
    dsUpdateSym (Right bs) = map Left $ BS.unpack bs


desugarProg :: (Show i) => Prog i -> ApproxMetric -> ApproxMode -> Bool -> Bool -> RProgAct
desugarProg prog m mode ite counters =
  RProg { rprogPipeline = [ lu ident | ident <- progPipeline prog ]
          , rprogDecls    = applyApproximation ds m mode ite
          }
  where
    ds        = execState (runReaderT (go (progDecls prog)) initDC) initDS
    identMap  = M.fromList $ zip [ (declIdent d, out) | d <- progDecls prog, out <- [True, False] ] [0..]
    nextFresh = M.size identMap + 1
    initDS    = DS { dsDecls = M.empty, dsRevDecls = M.empty, dsFresh = nextFresh, dsApprox = []}
    initDC    = DC { dcIdents = identMap }

    declIdent (Decl ident _) = ident
    declIdent (DeclInfo _ d) = declIdent d

-- Looks up terms with output or without, desugars and inserts Decl for the
-- exsisting one.
    go [] = return ()
    go (DeclInfo _ d:decls) = go (d:decls)
    go (Decl ident t:decls) = do
      i <- lookupIdent ident True
      j <- lookupIdent ident False
      i' <- desugarTerm True counters 0 t
      j' <- desugarTerm False counters 0 t
      _ <- insertDecl i (RSeq [i'])
      _ <- insertDecl j (RSeq [j'])
      go decls

    lu ident = fromMaybe (error $ "identifier in pipeline with no declaration: " ++ fromIdent ident)
                     (M.lookup (ident, True) identMap)

-- Goes through the list of ApproxDecls and applies the approximation
-- techniques
applyApproximation :: DesugarState -> ApproxMetric -> ApproxMode -> Bool -> M.Map RIdent RTermAct
applyApproximation ds m mode ite = fst $ foldl go (dsDecls ds, dsFresh ds) $ dsApprox ds
  where
    applyOnce decls pl k c = rprogDecls $
      (if ite then approxProgIt else approxProg) (stdToCore (RProg [pl] decls)) k c m mode
    addApproxStms dold c i dnew = (M.insert i (RSeq [c]) (M.union dold dnew) , c + M.size dnew)
    approxIds = map (\(_,i,_) -> i) $ dsApprox ds

    -- c is dsFresh aka first available decl "index"
    go (decls,c) (i1,i2,k) = if k < 1 then (decls,c) else
      if null (L.intersect approxIds (calculateReach i1 decls []))
      then addApproxStms decls c i2 (applyOnce decls i1 k c)
      else error "Approximated sub-programs cannot contain approximation terms"

calculateReach :: RIdent -> M.Map RIdent RTermAct -> [RIdent] -> [RIdent]
calculateReach r terms rs =
  case M.findWithDefault (RSeq []) r terms of
    RSum ids -> foldl go rs ids
    RSeq ids -> foldl go rs ids
    _        -> rs
  where
    go rs' r' = if (elem r' rs') then rs
                else calculateReach r' terms (r':rs')

desugarRegex :: E.Regex -> RProgAct
desugarRegex re =
  RProg { rprogPipeline = [ initIdent ]
          , rprogDecls    = dsDecls ds
          }
  where
    (initIdent, ds) = runState (runReaderT (desugarRE True re 0) initDC) initDS
    initDC = DC { dcIdents = M.empty }
    initDS = DS { dsDecls = M.empty, dsRevDecls = M.empty, dsFresh = 0, dsApprox = [] }
