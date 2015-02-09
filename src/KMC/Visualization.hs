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
import           Data.Char (chr)
import           Data.List (intercalate)
import           Data.Text.Lazy (pack)
import           Data.Word (Word8)

import           KMC.Expression
import           KMC.FSTConstruction
import           KMC.OutputTerm
import           KMC.RangeSet
import           KMC.SSTConstruction
import           KMC.SymbolicFST
import           KMC.SymbolicSST
import           KMC.Theories

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

instance Pretty KMC.SSTConstruction.Var where
  pretty (KMC.SSTConstruction.Var xs) = "x" ++ concatMap show xs

instance Pretty a => Pretty [a] where
  pretty = concatMap pretty

instance (Pretty p, Pretty a) => Pretty (Join (Const dom a :+: Enumerator p dom rng) [rng]) where
  pretty (Join xs) = concatMap aux xs
      where
        aux (Inl (Const x)) = pretty x
        aux (Inr (Enumerator p)) = "{" ++ pretty p ++ "}"

instance (Pretty var, Pretty func, Pretty (Rng func)) => Pretty (Atom var func) where
  pretty (VarA v) = "(" ++ pretty v ++ ")"
  pretty (ConstA x) = pretty x
  pretty (FuncA f) = "{" ++ pretty f ++ "}"

instance (Pretty var, Pretty func, Pretty (Rng func)) => Pretty (RegisterUpdate var func) where
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

fstToDot :: (Ord st, Pretty pred, Pretty (Rng func), Pretty func) => FST st pred func -> GV.DotGraph st
fstToDot fst' = GV.graphElemsToDot params nodes edges
    where
      params = GV.nonClusteredParams
               { GV.globalAttributes = fstGlobalAttrs
               , GV.fmtNode = formatNode (\q -> S.member q (fstF fst')) (== fstI fst')
               , GV.fmtEdge = formatFSTEdge
               }
      nodes = map (\x -> (x,x)) (S.toList (fstS fst'))
      edges = [ (q, q', l) | (q, l, q') <- KMC.SymbolicFST.edgesToList (fstE fst') ]

sstToDot :: (Pretty var, Pretty func, Pretty pred, Ord st, Pretty (Rng func)) => SST st pred func var -> GV.DotGraph Int
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
              | (q, xs) <- M.toList (KMC.SymbolicSST.eForward $ sstE sst), (p, k, q') <- xs ]

      formatSSTEdge (_,_, (p,k)) = [ GV.textLabel $ pack (pretty p ++ " /\\l" ++ pretty k) ]
      statesMap = M.fromList (zip (S.toList (sstS sst)) [(0::Int)..])
      statesMap' = M.fromList (zip [(0::Int)..] (S.toList (sstS sst)))

fancyToDot :: String -> GV.DotGraph Int
fancyToDot str =
  case parseRegex fancyRegexParser str of
    Left e -> error e
    Right (_, e) -> let fst' = fromMu (fromRegex e)
                              :: FST Int
                                     (RangeSet Word8)
                                     (Join
                                      (Const Word8 [Bool] :+: Enumerator (RangeSet Word8) Word8 Bool)
                                      [Bool])
                    in fstToDot fst'

fancyToSSTDot :: String -> GV.DotGraph Int
fancyToSSTDot str =
  case parseRegex fancyRegexParser str of
    Left e -> error e
    Right (_, e) -> let sst = sstFromFST (fromMu (fromRegex e))
                              :: SST (PathTree Var Int)
                                     (RangeSet Word8)
                                     (Join
                                      (Const Word8 [Bool] :+: Enumerator (RangeSet Word8) Word8 Bool)
                                      [Bool])
                                     Var
                    in sstToDot (optimize $ enumerateStates sst)

mkViz :: (GV.PrintDotRepr dg n) => (a -> dg n) -> a -> IO ()
mkViz f x = do
    _ <- forkIO $ GC.runGraphvizCanvas GC.Dot (f x) GC.Xlib
    return ()

mkVizToFile :: (GV.PrintDotRepr dg n) => (a -> dg n) -> a -> FilePath -> IO ()
mkVizToFile f x p = do
    fp <- GC.runGraphvizCommand GC.Dot (f x) GC.Pdf p
    putStrLn $ "Wrote file " ++ fp
    

vizFancyAsFST :: String -> IO ()
vizFancyAsFST str = mkViz fancyToDot str

vizFancyAsSST :: String -> IO ()
vizFancyAsSST str = mkViz fancyToSSTDot str

pngFancyAsSST :: String -> String -> IO FilePath
pngFancyAsSST file str = GC.runGraphviz (fancyToSSTDot str) GC.Png file
