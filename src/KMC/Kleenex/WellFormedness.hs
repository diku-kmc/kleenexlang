{-# LANGUAGE FlexibleContexts #-}
module KMC.Kleenex.WellFormedness(PosInfo, CheckError(..), MetaData(..)
                                 ,checkWellFormedness, prettyPrintError) where

import           KMC.Kleenex.Parser (SourcePos, sourceName, sourceLine, sourceColumn)
import           KMC.Kleenex.Syntax

import           Control.Monad (when, foldM, forM_)
import           Control.Monad.State
import           Data.Map (Map, (!))
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Text.PrettyPrint
import           Prelude hiding ((<>))

--------
-- Types
--------

type PosInfo = (SourcePos, SourcePos)

data CheckError = MultiDeclError { mdeIdent :: Ident
                                 , mdeThisDeclPos :: PosInfo
                                 , mdePreviousDeclPos :: PosInfo
                                 }
                | EmptyPipeline
                | UndeclaredPipelineIdents [Ident]
                | StrictDependency (Set Ident) [(Ident, PosInfo, Map Ident [PosInfo])]
                deriving (Show)

data MetaData = MetaData { mdComponents :: [Set Ident]
                         , mdDeclMap :: Map Ident (PosInfo, Term PosInfo)
                         }
              deriving (Show)

------------------
-- Pretty Printing
------------------

prettyPrintError :: CheckError -> String
prettyPrintError = render . go
  where
    go EmptyPipeline = text "Error: Empty pipeline"
    go (MultiDeclError ident thisDeclPos prevDeclPos) =
      prettyPos thisDeclPos <> colon
      $$ nest 4 (text ("Error: Multiple declarations of nonterminal '" ++ fromIdent ident ++ "'.")
                $$ (text "It is previously declared at " <> shortPos (fst prevDeclPos)))
    go (UndeclaredPipelineIdents ids) = text "Error: Undeclared nonterminals in pipeline: "
                                        <> hsep (punctuate comma (map (text . fromIdent) ids))
    go (StrictDependency c occs) = vcat $ map (prettyStrictOccurrences c) occs

    prettyStrictOccurrences c (ident, declPos, idents) =
      prettyPos declPos <> colon
      $$ nest 4 (
          text "Error: Strict occurrences in mutually recursive definition of '"
          <> text (fromIdent ident)
          <> text "' involving nonterminals"
          $$ nest 4 (hsep $ punctuate comma $ map (quotes . text . fromIdent) $ S.toList c)
          $$ text "Occurrences:"
          $$ nest 4 (vcat $ map prettyStrictOccurrence $ M.toList idents)
         )

    prettyStrictOccurrence (ident, positions) =
      vcat [
        quotes (text (fromIdent ident)) <+> text "at:" <+> shortPos (fst pos) <> text "-" <> shortPos (snd pos)
        | pos <- positions ]

    shortPos pos = parens (int (sourceLine pos) <> comma <> int (sourceColumn pos))
    prettyPos (pos1, pos2)
      = text (sourceName pos1) <> colon <> shortPos pos1 <> text "-" <> shortPos pos2

--------------------
-- Utility functions
--------------------

-- | Successors. Note that due to desugaring, Star and Plus makes an identifier self-referential.
succs :: Ident -> Term i -> [Ident]
succs ident = S.toList . go
  where
    go (Var i)            = S.singleton i
    go (Seq t1 t2)        = S.union (go t1) (go t2)
    go (Sum t1 t2)        = S.union (go t1) (go t2)
    go (Star t)           = S.insert ident $ go t
    go (Plus t)           = S.insert ident $ go t
    go (Question t)       = go t
    go (Range _ _ t)      = go t
    go (SuppressOutput t) = go t
    go (RedirectReg _ t)  = go t
    go (TermInfo _ t)     = go t
    go _                  = S.empty

-- | Occurrences of term identifiers and their source positions
termIdents :: Term PosInfo -> Map Ident [PosInfo]
termIdents = go Nothing
  where
    go ti term = case term of
      Var i            -> let Just info = ti in M.singleton i [info]
      Seq t1 t2        -> M.unionWith (flip (++)) (go ti t1) (go ti t2)
      Sum t1 t2        -> M.unionWith (flip (++)) (go ti t1) (go ti t2)
      Star t           -> go ti t
      Plus t           -> go ti t
      Question t       -> go ti t
      Range _ _ t      -> go ti t
      SuppressOutput t -> go ti t
      RedirectReg _ t  -> go ti t
      TermInfo ti' t   -> go (Just ti') t
      _                -> M.empty

-- | Occurrences of identifiers in strict positions
strictDeps :: Term PosInfo -> Map Ident [PosInfo]
strictDeps = go
  where
    go term = case term of
      Seq t1 t2        -> M.unionWith (flip (++)) (termIdents t1) (go t2)
      Sum t1 t2        -> M.unionWith (flip (++)) (go t1) (go t2)
      Star t           -> go t
      Plus t           -> go t
      Question t       -> go t
      Range _ _ t      -> go t
      SuppressOutput t -> go t
      RedirectReg _ t  -> go t
      TermInfo _ t     -> go t
      _                -> M.empty

-- | Utility function to take elements until (and including) the point where a
-- given predicate holds. Undefined if the predicate never holds for any element
-- in the list.
splitUntil :: (t -> Bool) -> [t] -> ([t], [t])
splitUntil _ [] = error "no element satisfies predicate"
splitUntil p (x:xs) | p x = ([x], xs)
                    | otherwise = let (ys, xs') = splitUntil p xs
                                  in (x:ys, xs')

--------------------
-- Strong components
--------------------

-- | Tarjan's linear-time algorithm for finding strongly connected components
scc :: Map Ident (PosInfo, Term PosInfo) -> [Set Ident]
scc declMap = sccComponents $ execState scc' initState
  where
    initState = SCCState
                { sccIndex = 0
                , sccStack = []
                , sccProps = M.fromList $ zip (M.keys declMap) $ repeat (SCCProps False (-1) (-1))
                , sccComponents = []
                }

    scc' = forM_ (M.keys declMap) (\v -> do
      vIndex <- gets (\s -> pIndex $ sccProps s!v)
      when (vIndex == -1) $ go v)

    go v = do
      modify $ \s ->
        s { sccIndex = sccIndex s + 1
          , sccStack = v:sccStack s
          , sccProps = M.insert v (SCCProps True (sccIndex s) (sccIndex s))
                                  (sccProps s)
          }
      let (_, t) = declMap!v
      forM_ (succs v t) $ \w -> do
        wIndex <- gets (\s -> pIndex $ sccProps s!w)
        if wIndex == -1 then do
          go w
          wProps <- gets $ \s -> sccProps s!w
          vProps <- gets $ \s -> sccProps s!v
          modify $ \s -> s { sccProps = M.insert v
                                         (vProps { pLowLink = min (pLowLink vProps)
                                                                  (pLowLink wProps) })
                                         (sccProps s) }
         else do
          wProps <- gets $ \s -> sccProps s!w
          when (pOnStack wProps) $ do
            vProps <- gets $ \s -> sccProps s!v
            modify $ \s -> s { sccProps = M.insert v
                                           (vProps { pLowLink = min (pLowLink vProps)
                                                                    (pIndex wProps) })
                                           (sccProps s) }
      vProps <- gets $ \s -> sccProps s!v
      when (pLowLink vProps == pIndex vProps) $ do
        stack <- gets sccStack
        let (component, stack') = splitUntil (==v) stack
        modify $ \s -> s { sccStack = stack'
                         , sccComponents = (S.fromList component):sccComponents s
                         , sccProps = foldl (\props w -> M.insert w ((props!w) { pOnStack = False }) props)
                                            (sccProps s)
                                            component
                         }
data SCCProps = SCCProps { pOnStack :: Bool, pIndex :: Int, pLowLink :: Int }
data SCCState = SCCState { sccIndex :: Int
                         , sccStack :: [Ident]
                         , sccProps :: Map Ident SCCProps
                         , sccComponents :: [Set Ident]
                         }

-------------------
-- Program checking
-------------------

-- | Check that declarations and the pipeline is well-formed: pipeline must not be
-- empty and must not refer to undeclared identifiers; there must be position
-- information in every declaration; and every declaration must not refer to
-- undeclared identifiers.
checkDeclsAndPipeline :: Prog PosInfo -> Either CheckError (Map Ident (PosInfo, Term PosInfo))
checkDeclsAndPipeline (Kleenex { progPipeline = pipeline, progDecls = ds }) = do
  let ds' = map (decl Nothing) ds
  declMap <- foldM buildMap M.empty ds'
  when (null pipeline) $ Left EmptyPipeline
  let undecls = filter (not . flip M.member declMap) pipeline
  when (not $ null undecls) $ Left $ UndeclaredPipelineIdents undecls
  -- TODO: Check for undeclared identifiers in declarations.
  return declMap
  where
    decl (Just i) (Decl ident term) = (ident, (i, term))
    decl Nothing (Decl _ _) = error "no source position for declaration"
    decl _ (DeclInfo i d) = decl (Just i) d

    buildMap declMap (ident, (i, term))
      | Just (i', _) <- M.lookup ident declMap = Left (MultiDeclError ident i i')
      | otherwise = return $ M.insert ident (i, term) declMap

-- | Check that the grammar is non-self embedding by verifying that every strong
-- component does not contain a strict dependency.
checkNSE :: Map Ident (PosInfo, Term PosInfo) -> [Set Ident] -> Either CheckError ()
checkNSE declMap = mapM_ checkComponent
  where
    checkComponent c =
      let allStrictDeps = [ (ident, i, strictDeps t) | ident <- S.toList c, let (i, t) = declMap!ident ]
          localStrictDeps = [ (ident, i, deps') | (ident, i, deps) <- allStrictDeps
                                                , let deps' = M.filterWithKey (\k _ -> S.member k c) deps
                                                , not $ M.null deps' ]
      in when (not $ null localStrictDeps) $ Left $ StrictDependency c localStrictDeps

-- | Check well-formedness of a program.
checkWellFormedness :: Prog PosInfo -> Either CheckError MetaData
checkWellFormedness prog = do
  declMap <- checkDeclsAndPipeline prog
  let sccs = scc declMap
  checkNSE declMap sccs
  return $ MetaData { mdComponents = sccs
                    , mdDeclMap = declMap
                    }
