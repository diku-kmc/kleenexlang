{-# LANGUAGE TypeSynonymInstances #-}
module KMC.Visualization where

import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GA
import qualified Data.GraphViz.Commands as GC
import qualified Data.Map as M
import qualified Data.Set as S

import           Control.Concurrent (forkIO)
import           Data.Char (chr)
import           Data.List (intercalate)
import           Data.Text.Lazy (pack)
import           Data.Word (Word8)

import           KMC.OutputTerm
import           KMC.RangeSet
import           KMC.SymbolicFST
import           KMC.SymbolicSST
import           KMC.FSTConstruction
import           KMC.SSTConstruction
import           KMC.Expression

import           KMC.Syntax.Parser
import           KMC.Syntax.Config

class Pretty a where
  pretty :: a -> String

instance (Pretty a) => Pretty (RangeSet a) where
  pretty rs =
    "[" ++ intercalate "," [ pretty l ++ "-" ++ pretty h | (l,h) <- ranges rs ] ++ "]"

instance Pretty Word8 where
  pretty a = [chr $ fromEnum a]

instance Pretty Bool where
  pretty False = "0"
  pretty True = "1"

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left x) = "(" ++ pretty x ++ ")"
  pretty (Right y) = pretty y

instance Pretty SSTConstruction.Var where
  pretty (SSTConstruction.Var xs) = "x" ++ concatMap show xs

instance Pretty a => Pretty [a] where
  pretty = concatMap pretty

instance (Pretty p, Pretty a) => Pretty (OutputTerm p a) where
  pretty (OutputTerm xs) = concatMap aux xs
      where
        aux (Const b) = pretty b
        aux (Code p) = "{" ++ pretty p ++ "}"

instance (Pretty var, Pretty func) => Pretty (RegisterUpdate var func) where
  pretty m = "[" ++ intercalate "\\l," [ pretty v ++ ":=" ++ pretty f | (v,f) <- M.toList m ] ++ "]"

fstGlobalAttrs :: [GV.GlobalAttributes]
fstGlobalAttrs = [GV.GraphAttrs [GA.RankDir GA.FromLeft]
                 ,GV.NodeAttrs [GA.Shape GA.Circle]]

formatNode :: (Ord st) => (st -> Bool) -> (st -> Bool) -> (st, st) -> GA.Attributes
formatNode isFinal isInitial (q, _) =
  concat
  [ [ GA.Shape GA.DoubleCircle | isFinal q ]
  , [ GA.Shape GA.BoxShape     | isInitial q ]
  ]

formatFSTEdge :: (Ord st, Pretty pred, Pretty delta, Pretty func)
              => (st, st, Either (pred, func) delta)
              -> GA.Attributes
formatFSTEdge (_, _, l) =
  case l of
    Left (p, f)  -> [ GV.textLabel $ pack (pretty p ++ " / " ++ pretty f) ]
    Right l' -> [ GV.textLabel $ pack ("/ " ++ pretty l') ]

fstToDot :: (Ord st, Pretty pred, Pretty delta, Pretty func) => FST st pred func delta -> GV.DotGraph st
fstToDot fst' = GV.graphElemsToDot params nodes edges
    where
      params = GV.nonClusteredParams
               { GV.globalAttributes = fstGlobalAttrs
               , GV.fmtNode = formatNode (\q -> S.member q (fstF fst')) (== fstI fst')
               , GV.fmtEdge = formatFSTEdge
               }
      nodes = map (\x -> (x,x)) (S.toList (fstS fst'))
      edges = [ (q, q', l) | (q, l, q') <- SymbolicFST.edgesToList (fstE fst') ]

sstToDot :: (Pretty var, Pretty func, Pretty pred, Ord st) => SST st pred func var delta -> GV.DotGraph Int
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
              | (q, xs) <- M.toList (SymbolicSST.eForward $ sstE sst), (p, k, q') <- xs ]

      formatSSTEdge (_,_, (p,k)) = [ GV.textLabel $ pack (pretty p ++ " /\\l" ++ pretty k) ]
      statesMap = M.fromList (zip (S.toList (sstS sst)) [(0::Int)..])
      statesMap' = M.fromList (zip [(0::Int)..] (S.toList (sstS sst)))

fancyToDot :: String -> GV.DotGraph Int
fancyToDot str =
  case parseRegex fancyRegexParser str of
    Left e -> error e
    Right (_, e) -> fstToDot (fromMu (fromRegex e))

fancyToSSTDot :: String -> GV.DotGraph Int
fancyToSSTDot str =
  case parseRegex fancyRegexParser str of
    Left e -> error e
    Right (_, e) -> let sst = sstFromFST (fromMu (fromRegex e))
                              :: SST (PathTree Var Int)
                                     (RangeSet Word8)
                                     (OutputTerm (RangeSet Word8) (Either Var Bool))
                                     Var
                                     Bool
                    in sstToDot (optimize $ enumerateStates sst)

mkViz :: (GV.PrintDotRepr dg n) => (a -> dg n) -> a -> IO ()
mkViz f x = do
    _ <- forkIO $ GC.runGraphvizCanvas GC.Dot (f x) GC.Xlib
    return ()

vizFancyAsFST :: String -> IO ()
vizFancyAsFST str = mkViz fancyToDot str

vizFancyAsSST :: String -> IO ()
vizFancyAsSST str = mkViz fancyToSSTDot str

pngFancyAsSST :: String -> String -> IO FilePath
pngFancyAsSST file str = GC.runGraphviz (fancyToSSTDot str) GC.Png file
