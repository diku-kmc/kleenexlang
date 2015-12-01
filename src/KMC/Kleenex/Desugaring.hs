{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module KMC.Kleenex.Desugaring(desugarProg) where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString as BS
import           Data.Char (ord)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Word
import           KMC.Kleenex.Actions
import qualified KMC.Kleenex.Syntax as S
import           KMC.Kleenex.Syntax hiding (RTerm, RProg)
import qualified KMC.RangeSet as RS
import qualified KMC.Syntax.External as E
import           KMC.Util.UTF8

type RTerm = S.RTerm Word8 (Either Word8 RegAction)
type RProg = S.RProg Word8 (Either Word8 RegAction)

-------------------
-- Desugaring monad
-------------------

data DesugarState = DS { dsDecls    :: M.Map RIdent RTerm
                       , dsRevDecls :: M.Map RTerm RIdent
                       , dsFresh    :: RIdent 
                       }

data DesugarContext = DC { dcIdents   :: M.Map Ident RIdent }

type Desugar = ReaderT DesugarContext (State DesugarState)

getFresh :: MonadState DesugarState m => m RIdent
getFresh = do
  i <- gets dsFresh
  modify $ \ds -> ds { dsFresh = dsFresh ds + 1 }
  return i

insertDecl :: MonadState DesugarState m => RIdent -> RTerm -> m RIdent
insertDecl i t = do modify $ \ds -> ds { dsDecls    = M.insert i t (dsDecls ds)
                                       , dsRevDecls = M.insert t i (dsRevDecls ds)
                                       }
                    return i

decl :: MonadState DesugarState m => RTerm -> m RIdent
decl t = do
  rds <- gets dsRevDecls
  case M.lookup t rds of
    Just i -> return i
    Nothing -> do
      i <- getFresh
      insertDecl i t

lookupIdent :: MonadReader DesugarContext m => Ident -> m RIdent
lookupIdent ident =
  asks (M.lookup ident . dcIdents)
  >>= maybe (error $ "lookupIdent invariant broken: " ++ fromIdent ident) return

--------------------------------
-- Regular expression desugaring
--------------------------------

desugarRE :: Bool        -- ^ Indicates if input symbols are to be written as actions
          -> E.Regex     -- ^ Regular expression to desugar
          -> Desugar RIdent -- ^ Identifier of nonterminal representing desugared RE
desugarRE out re =
  case re of
  E.One            -> decl $ RSeq []
  E.Dot            -> decl $ RRead RS.universe out
  E.Chr a          -> sequence [ decl $ RRead (RS.singleton b) out | b <- encodeChar a ] >>= decl . RSeq
  E.Group _ e      -> desugarRE out e
  E.Concat e1 e2   -> sequence [ desugarRE out e1, desugarRE out e2 ] >>= decl . RSeq
  E.Branch e1 e2   -> sequence [ desugarRE out e1, desugarRE out e2 ] >>= decl . RSum
  E.Class pos ers  -> let rs  = (if pos then id else RS.complement)
                                $ RS.rangeSet [ (toEnum (ord lo), toEnum (ord hi)) | (lo, hi) <- ers ]
                      in decl $ RRead rs out
  E.Star e         -> do ie <- desugarRE out e
                         ieps <- decl $ RSeq []
                         i <- getFresh
                         iloop <- decl $ RSeq [ie, i]
                         insertDecl i $ RSum [iloop, ieps]
  E.LazyStar e     -> do ie <- desugarRE out e
                         ieps <- decl $ RSeq []
                         i <- getFresh
                         iloop <- decl $ RSeq [ie, i]
                         insertDecl i $ RSum [ieps, iloop]
  E.Plus e         -> do ie <- desugarRE out e
                         istar <- desugarRE out (E.Star e)
                         decl $ RSeq [ie, istar]
  E.LazyPlus e     -> do ie <- desugarRE out e
                         istar <- desugarRE out (E.LazyStar e)
                         decl $ RSeq [ie, istar]
  E.Question e     -> do ie <- desugarRE out e
                         ieps <- decl $ RSeq []
                         decl $ RSum [ie, ieps]
  E.LazyQuestion e -> do ie <- desugarRE out e
                         ieps <- decl $ RSeq []
                         decl $ RSum [ieps, ie]
  E.Range e n m    -> do
    ie <- desugarRE out e
    case m of
      Nothing -> do istar <- desugarRE out (E.Star e)
                    decl $ RSeq (replicate n ie ++ [istar])
      Just m' -> if n == m' then
                   decl $ RSeq (replicate n ie)
                 else
                   do iquest <- desugarRE out (E.Question e)
                      decl $ RSeq (replicate n ie ++ replicate m' iquest)
  E.Suppress e     -> desugarRE False e
  E.NamedSet _ _   -> error "named sets not supported"
  E.LazyRange _ _ _ -> error "lazy ranges not supported"


---------------------
-- Kleenex desugaring
---------------------

desugarTerm :: Bool        -- ^ Indicates if input symbols are to be written as actions
            -> Term i      -- ^ Term to be desugared
            -> Desugar RIdent -- ^ Identifier of nonterminal representing desugared term
desugarTerm out t =
  case t of
  (Var ident)   -> lookupIdent ident
  (Constant bs) -> mapM (decl . RConst . Left) (if out then BS.unpack bs else []) >>= decl . RSeq
  (RE e)        -> desugarRE out e
  (Seq _ _)     -> mapM (desugarTerm out) (flattenSeq t) >>= decl . RSeq
  (Sum _ _)     -> mapM (desugarTerm out) (flattenSum t) >>= decl . RSum
  (Star t1)     -> do it <- desugarTerm out t1
                      ieps <- decl $ RSeq []
                      i <- getFresh
                      iloop <- decl $ RSeq [it, i]
                      insertDecl i $ RSum [iloop, ieps]
  (Plus t1)     -> do it <- desugarTerm out t1
                      istar <- desugarTerm out (Star t1)
                      decl $ RSeq [it, istar]
  (Question t1) -> do it <- desugarTerm out t1
                      ieps <- decl $ RSeq []
                      decl $ RSum [it, ieps]
  (Range m' n' t1) -> do it <- desugarTerm out t1
                         let m = fromMaybe 0 m'
                         case n' of
                           Nothing -> do
                             istar <- desugarTerm out (Star t1)
                             decl $ RSeq (replicate m it ++ [istar])
                           Just n | n < m -> error $ "invalid range: {" ++ show m ++ "," ++ show n ++ "}"
                                  | n == m -> decl $ RSeq (replicate m it)
                                  | otherwise -> do
                                      iquest <- desugarTerm out (Question t1)
                                      decl $ RSeq (replicate m it ++ replicate (n-m) iquest)
  (SuppressOutput t1) -> desugarTerm False t1
  One                 -> decl $ RSeq []
  UpdateReg r str     -> mapM (decl . RConst) (Right Push
                                               :(str >>= dsUpdateSym)
                                               ++ [Right $ Pop r]) >>= decl . RSeq
  WriteReg r          -> decl $ RConst (Right (Write r))
  RedirectReg r t1    -> do it <- desugarTerm out t1
                            [ipush, ipop] <- mapM (decl . RConst . Right) [Push, Pop r]
                            decl $ RSeq [ipush,it,ipop]
  TermInfo _ t1       -> desugarTerm out t1
  where
    flattenSeq (Seq t1 t2) = flattenSeq t1 ++ flattenSeq t2
    flattenSeq t'          = [t']
    flattenSum (Sum t1 t2) = flattenSum t1 ++ flattenSum t2
    flattenSum t'          = [t']
    dsUpdateSym (Left r)   = [Right $ Write r]
    dsUpdateSym (Right bs) = map Left $ BS.unpack bs


desugarProg :: Prog i -> RProg
desugarProg prog =
  S.RProg { rprogPipeline = [ lu ident | ident <- progPipeline prog ]
          , rprogDecls    = dsDecls ds
          }
  where
    ds        = execState (runReaderT (go (progDecls prog)) initDC) initDS
    identMap  = M.fromList $ zip [ declIdent d | d <- progDecls prog ] [0..]
    nextFresh = M.size identMap + 1
    initDS    = DS { dsDecls = M.empty, dsRevDecls = M.empty, dsFresh = nextFresh }
    initDC    = DC { dcIdents = identMap }

    declIdent (Decl ident _) = ident
    declIdent (DeclInfo _ d) = declIdent d

    go [] = return ()
    go (DeclInfo _ d:decls) = go (d:decls)
    go (Decl ident t:decls) = do
      i <- lookupIdent ident
      j <- desugarTerm True t
      _ <- insertDecl i (RSeq [j])
      go decls

    lu ident = maybe (error $ "identifier in pipeline with no declaration: " ++ fromIdent ident) id
                     (M.lookup ident identMap)
