{-# LANGUAGE FlexibleContexts #-}

module KMC.Kleenex.Core (stdToCore, getDecl) where

import           Control.Monad.State
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import           KMC.Kleenex.ApproximationMetrics (Decls, RTermAct, RProgAct)
import           KMC.Kleenex.Syntax

data StdToCoreState = SC { scDecls   :: M.Map RIdent RTermAct
                         , scFresh   :: RIdent
                         , scVisited :: M.Map [RIdent] RIdent
                         }
type StdToCore = State StdToCoreState

-- | Gets next free id
getFresh :: MonadState StdToCoreState m => [RIdent] -> m RIdent
getFresh st = do
  i <- gets scFresh
  modify $ \sc -> sc { scFresh = scFresh sc + 1
                     , scVisited = M.insert st i (scVisited sc)}
  return i

-- | Inserts a deceleration in the final declaration map, given a id.
insertDecl :: MonadState StdToCoreState m => RIdent -> RTermAct -> m RIdent
insertDecl i t = do modify $ \sc -> sc { scDecls = M.insert i t (scDecls sc) }
                    return i

-- | Gets a new id and adds the deceleration to that id
decl :: MonadState StdToCoreState m => RTermAct -> [RIdent] -> m RIdent
decl t st = do
  fresh <- getFresh st
  insertDecl fresh t

-- | Returns next relevant declaration
getDecl :: RIdent -> Decls -> RTermAct
getDecl rid decls = case decls HM.! rid of
    (RSeq (x:[])) -> getDecl x decls
    t             -> t

-- | Looks if the current id stack has already been visited
visited :: MonadState StdToCoreState m => [RIdent] -> m RIdent
visited ids = do
  vis <- gets scVisited
  return $ M.findWithDefault (-1) ids vis

-- | Rewrite each declaration
rewrite :: Decls -> [RIdent] -> StdToCore RIdent
rewrite _ [] = error "Empty stack during declaration rewrite"
rewrite decls (stack@(s:s')) =
  do vis <- visited stack
     if  vis > 0 then return vis else
      case t of
        RSum ids -> do fresh <- getFresh stack
                       ids'  <- mapM (\x -> rewrite decls (x:s')) ids
                       insertDecl fresh $ RSum ids'
        RSeq ids -> if null s' && null ids then decl t stack else do
                       fresh <- getFresh stack
                       nid   <- rewrite decls $ ids ++ s'
                       insertDecl fresh $ RSeq [nid]
        _        -> if null s' then decl t stack else do
                       fresh <- getFresh stack
                       id1   <- decl t stack
                       id2   <- rewrite decls $ s'
                       insertDecl fresh $ RSeq [id1, id2]
  where t = getDecl s decls

-- | Its assumed that pipeline only contains one element..
stdToCore :: RProgAct -> RProgAct
stdToCore p = RProg [0] $ scDecls sc
  where
    sc = execState (rewrite decls pl) $ SC M.empty 0 M.empty
    decls = HM.fromList $ M.toList $ rprogDecls p
    pl = rprogPipeline p
