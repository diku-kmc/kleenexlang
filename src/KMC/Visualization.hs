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
import           Data.List (intercalate)
import           Data.Text.Lazy (pack)
import           Data.Word (Word8, Word16)

import           KMC.Kleenex.Actions
import           KMC.Kleenex.Syntax hiding (Ident)
import           KMC.RangeSet
import           KMC.SSTConstruction
import           KMC.SymbolicAcceptor
import           KMC.SymbolicFST
import           KMC.SymbolicSST
import           KMC.Theories
import           KMC.Util.Bits

import           KMC.FSTConstruction2

class Pretty a where
  pretty :: a -> String

instance Pretty RegAction where
  pretty Push    = "<push>"
  pretty (Pop r) = "<pop." ++ show (fromRegIdent r) ++ ">"
  pretty (Write r) = "<wr." ++ show (fromRegIdent r) ++ ">"

instance (Pretty e, Pretty dom, Pretty digit) => Pretty (CodeFunc e dom digit) where
  pretty (CodeArg e) = "CODE(" ++ pretty e ++ ")"
  pretty (CodeConst bs) = concatMap pretty bs

instance Pretty b => Pretty (CodeInputLab b) where
  pretty (InputAny k) = "ANY(" ++ show k ++")"
  pretty (InputConst bs) = pretty bs

instance (Pretty c) => Pretty (DecodeFunc enum digit c) where
  pretty (DecodeArg _) = "DECODE"
  pretty (DecodeConst c) = pretty c

instance Pretty c => Pretty (CopyFunc a c) where
  pretty CopyArg = "COPY"
  pretty (CopyConst c) = pretty c

instance (Eq a, Pretty a) => Pretty (RangeSet a) where
  pretty rs | [(l,h)] <- ranges rs, l == h = pretty l
  pretty rs =
    "[" ++ intercalate "," [ pretty l ++ "-" ++ pretty h | (l,h) <- ranges rs ] ++ "]"

instance Pretty Word8 where
  pretty a = let c = chr $ fromEnum a
             in if isPrint c then [c] else '\\':show a

instance Pretty Word16 where
    pretty a = let (c1, c2) = split a :: (Word8, Word8)
               in pretty c1 ++ pretty c2

instance Pretty Bool where
  pretty False = "0"
  pretty True = "1"

instance Pretty Int where
  pretty = show

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left x) = "(" ++ pretty x ++ ")"
  pretty (Right y) = pretty y

instance Pretty Var where
  pretty (KMC.SSTConstruction.Var xs) = "x" ++ concatMap show xs

instance Pretty a => Pretty [a] where
  pretty = concatMap pretty

instance (Pretty var, Pretty func, Pretty (Rng func)) => Pretty (Atom var func) where
  pretty (VarA v) = "(" ++ pretty v ++ ")"
  pretty (ConstA x) = pretty x
  pretty (FuncA f i) = "{" ++ pretty f ++ "(" ++ show i ++ ")}"

instance (Pretty var, Pretty func, Pretty (Rng func)) => Pretty (RegisterUpdate var func) where
  pretty m = "[" ++ intercalate "\\l," [ pretty v ++ ":=" ++ pretty f | (v,f) <- M.toList m ] ++ "]"

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
    Left (p, f)  -> [ GV.textLabel $ pack (pretty p ++ " / " ++ pretty f) ]
    Right l' -> [ GV.textLabel $ pack ("/ " ++ pretty l') ]

dfaToDot :: (Ord st, Pretty pred)
         => DFA st pred -> GV.DotGraph st
dfaToDot (DFA dfa) = GV.graphElemsToDot params nodes edges
    where
      params = GV.nonClusteredParams
               { GV.globalAttributes = fstGlobalAttrs
               , GV.fmtNode = formatNode (\q -> S.member q (accF dfa)) (== unSingle (accI dfa))
               , GV.fmtEdge = formatDFAEdge
               }
      nodes = map (\x -> (x,x)) (S.toList (accS dfa))
      edges = [ (q, q', p) | (q, p, q') <- dfaEdgesToList (accE dfa) ]
      formatDFAEdge (_, _, p) = [ GV.textLabel $ pack $ pretty p ]

nfaToDot :: (Ord st, Pretty pred)
         => NFA st pred -> GV.DotGraph st
nfaToDot (NFA nfa) = GV.graphElemsToDot params nodes edges
    where
      params = GV.nonClusteredParams
               { GV.globalAttributes = fstGlobalAttrs
               , GV.fmtNode = formatNode (flip S.member (accF nfa)) (flip S.member (accI nfa))
               , GV.fmtEdge = formatNFAEdge
               }
      nodes = map (\x -> (x,x)) (S.toList (accS nfa))
      edges = [ (q, q', p) | (q, p, q') <- nfaEdgesToList (accE nfa) ]
      -- Unicode point 949 is 'GREEK SMALL LETTER EPSILON'
      formatNFAEdge (_, _, Nothing) = [ GV.textLabel $ pack $ [chr 949] ]
      formatNFAEdge (_, _, Just p)  = [ GV.textLabel $ pack $ pretty p ]


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
