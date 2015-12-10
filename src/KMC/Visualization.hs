{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module KMC.Visualization where

import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GA
import qualified Data.GraphViz.Commands as GC
import qualified Data.Map as M
import qualified Data.Set as S

import           Control.Concurrent (forkIO)
import           Data.Char (chr, isPrint)
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.List (intercalate)
import           Data.Text.Lazy (pack)
import           Data.Word (Word8)

import           KMC.Kleenex.Actions
import           KMC.Kleenex.Syntax hiding (Ident)
import           KMC.RangeSet
import           KMC.Determinization
import           KMC.SymbolicFST
import           KMC.SymbolicSST
import           KMC.Theories

import           KMC.SymbolicFST.OracleMachine
import           KMC.SymbolicFST.ActionMachine
import           KMC.SymbolicFST.Transducer
import qualified KMC.SymbolicSST.ActionSST as ASST

class Pretty a where
  pretty :: a -> String

instance Pretty RegAction where
  pretty Push    = "<push>"
  pretty (Pop r) = "<pop." ++ show (fromRegIdent r) ++ ">"
  pretty (Write r) = "<wr." ++ show (fromRegIdent r) ++ ">"

instance (Pretty e, Pretty dom, Pretty digit) => Pretty (CodeFunc e dom digit) where
  pretty (CodeArg e) = "CODE(" ++ pretty e ++ ")"
  pretty (CodeConst bs) = concatMap pretty bs

instance Pretty f => Pretty (EpsFunc f) where
  pretty EpsFunc = "ϵ"
  pretty (JustFunc f) = pretty f

instance Pretty b => Pretty (CodeInputLab b) where
  pretty (InputAny k) = "ANY(" ++ show k ++")"
  pretty (InputConst bs) = pretty bs

instance (Pretty c) => Pretty (DecodeFunc enum digit c) where
  pretty (DecodeArg _) = "DECODE"
  pretty (DecodeConst c) = pretty c

instance Pretty c => Pretty (CopyFunc a c) where
  pretty CopyArg = "COPY"
  pretty (CopyConst c) = pretty c

instance Pretty c => Pretty (ASST.ConstOrAnyLab c) where
  pretty (ASST.ConstLab x) = pretty x
  pretty ASST.AnyLab = "∙"

instance Pretty c => Pretty (Identity c) where
  pretty = pretty . runIdentity

instance (Eq a, Pretty a) => Pretty (RangeSet a) where
  pretty rs | [(l,h)] <- ranges rs, l == h = "[" ++ pretty l ++ "]"
  pretty rs =
    "[" ++ intercalate "," [ pretty l ++ "-" ++ pretty h | (l,h) <- ranges rs ] ++ "]"

instance Pretty Word8 where
  pretty a = let c = chr $ fromEnum a
             in if isPrint c then [c] else '\\':show a

instance Pretty Bool where
  pretty False = "0"
  pretty True = "1"

instance Pretty Int where
  pretty = show

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left x) = "(" ++ pretty x ++ ")"
  pretty (Right y) = pretty y

instance Pretty Var where
  pretty (KMC.Determinization.Var xs) = "x" ++ concatMap show xs

instance Pretty a => Pretty [a] where
  pretty [] = "ε"
  pretty xs = concatMap pretty xs

instance (Pretty var, Pretty func, Pretty (Rng func)) => Pretty (Atom var func) where
  pretty (VarA v) = "(" ++ pretty v ++ ")"
  pretty (ConstA x) = pretty x
  pretty (FuncA f) = "{" ++ pretty f ++ "}"

instance (Pretty var, Pretty func, Pretty (Rng func)) => Pretty (RegisterUpdate var func) where
  pretty m = "[" ++ intercalate "\\l," [ pretty v ++ ":=" ++ pretty f | (v,f) <- M.toList m ] ++ "]"

-- | Quick and dirty way to force shallow evaluation of a DotGraph
graphSize :: GV.DotGraph st -> Int
graphSize = stmtsSize . GV.graphStatements
  where
    stmtsSize ss =
      sum [length (GV.attrStmts ss)
          ,length (GV.nodeStmts ss)
          ,length (GV.edgeStmts ss)
          ,sum (map subSize (GV.subGraphs ss))]
    subSize sub = stmtsSize (GV.subGraphStmts sub)

fstGlobalAttrs :: [GV.GlobalAttributes]
fstGlobalAttrs = [GV.GraphAttrs [GA.RankDir GA.FromLeft]
                 ,GV.NodeAttrs [GA.Shape GA.Circle]]

formatNode :: (Ord st) => (st -> Bool) -> (st -> Bool) -> (st, st) -> GA.Attributes
formatNode isFinal isInitial (q, _) =
    case (isInitial q, isFinal q) of
      (True, True) -> [ GA.Shape GA.DoubleOctagon ]
      (True, _   ) -> [ GA.Shape GA.BoxShape      ]
      (_,    True) -> [ GA.Shape GA.DoubleCircle  ]
      _            -> []

formatFSTEdge :: (Ord st, Pretty pred, Pretty delta, Pretty func)
              => (st, st, Either (pred, func) delta)
              -> GA.Attributes
formatFSTEdge (_, _, l) =
  case l of
    Left (p, f)  -> [ GV.textLabel $ pack (pretty p ++ "/" ++ pretty f) ]
    Right l' -> [ GV.textLabel $ pack ("ε/" ++ pretty l') ]

fstToDot :: (Ord st, Pretty pred, Pretty (Rng func), Pretty func)
         => FST st pred func -> GV.DotGraph st
fstToDot fst' = GV.graphElemsToDot params nodes edges
    where
      params = GV.nonClusteredParams
               { GV.globalAttributes = fstGlobalAttrs
               , GV.fmtNode = formatNode (\q -> S.member q (fstF fst')) (== fstI fst')
               , GV.fmtEdge = formatFSTEdge
               }
      nodes = map (\x -> (x,x)) (S.toList (fstS fst'))
      edges = [ (q, q', l) | (q, l, q') <- KMC.SymbolicFST.edgesToList (fstE fst') ]

sstToDot :: (Pretty var, Pretty func, Pretty pred, Ord st, Pretty (Rng func), Pretty (Dom func))
         => SST st pred func var -> GV.DotGraph Int
sstToDot sst = GV.graphElemsToDot params nodes edges
    where
      params = GV.nonClusteredParams
               { GV.globalAttributes = fstGlobalAttrs
               , GV.fmtNode = formatNode (\q -> M.member (statesMap' M.! q) (sstF sst))
                                         (\q -> (statesMap' M.! q) == sstI sst)
               , GV.fmtEdge = formatSSTEdge
               }
      nodes = map (\x -> (statesMap M.! x, statesMap M.! x)) (S.toList (sstS sst))
      edges = [ (statesMap M.! q, statesMap M.! q', (p,k))
              | (q, xs) <- M.toList (sstE sst), (p, k, q') <- xs ]

      formatSSTEdge (_,_, (p,k)) = [ GV.textLabel $ pack (pretty p ++ " /\\l" ++ pretty k) ]
      statesMap = M.fromList (zip (S.toList (sstS sst)) [(0::Int)..])
      statesMap' = M.fromList (zip [(0::Int)..] (S.toList (sstS sst)))

mkViz :: (GV.PrintDotRepr dg n) => (a -> dg n) -> a -> IO ()
mkViz f x = do
    _ <- forkIO $ GC.runGraphvizCanvas GC.Dot (f x) GC.Xlib
    return ()

mkVizToFile :: (GV.PrintDotRepr dg n) => (a -> dg n) -> a -> FilePath -> IO ()
mkVizToFile f x p = do
    fp <- GC.runGraphvizCommand GC.Dot (f x) GC.Pdf p
    putStrLn $ "Wrote file " ++ fp
