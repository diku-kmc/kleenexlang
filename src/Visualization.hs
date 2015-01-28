module Visualization where

import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GA
import qualified Data.GraphViz.Commands as GC
import qualified Data.Set as S

import           Control.Concurrent (forkIO)
import           Data.Char (chr)
import           Data.List (intercalate)
import           Data.Text.Lazy (pack)
import           Data.Word (Word8)

import           OutputTerm
import           RangeSet
import           SymbolicFST

import           FSTConstruction
import           Expression
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

instance Pretty a => Pretty [a] where
  pretty = concatMap pretty

instance (Pretty p, Pretty a) => Pretty (OutputTerm p a) where
  pretty (OutputTerm xs) = concatMap aux xs
      where
        aux (Const b) = pretty b
        aux (Code p) = "{" ++ pretty p ++ "}"

fstGlobalAttrs :: [GV.GlobalAttributes]
fstGlobalAttrs = [GV.GraphAttrs [GA.RankDir GA.FromLeft]
                 ,GV.NodeAttrs [GA.Shape GA.Circle]]

formatNode :: (Ord st) => FST st pred func delta -> (st, st) -> GA.Attributes
formatNode fst' (q, _) =
  concat
  [ [ GA.Shape GA.DoubleCircle | S.member q (fstF fst') ]
  , [ GA.Shape GA.BoxShape     | q == fstI fst' ]
  ]

formatEdge :: (Ord st, Pretty pred, Pretty delta, Pretty func)
              => FST st pred func delta
              -> (st, st, Either (pred, func) delta)
              -> GA.Attributes
formatEdge _ (_, _, l) =
  case l of
    Left (p, f)  -> [ GV.textLabel $ pack (pretty p ++ " / " ++ pretty f) ]
    Right l' -> [ GV.textLabel $ pack ("/ " ++ pretty l') ]

fstToDot :: (Ord st, Pretty pred, Pretty delta, Pretty func) => FST st pred func delta -> GV.DotGraph st
fstToDot fst' = GV.graphElemsToDot params nodes edges
    where
      params = GV.nonClusteredParams
               { GV.globalAttributes = fstGlobalAttrs
               , GV.fmtNode = formatNode fst'
               , GV.fmtEdge = formatEdge fst'
               }
      nodes = map (\x -> (x,x)) (S.toList (fstS fst'))
      edges = [ (q, q', l) | (q, l, q') <- edgesToList (fstE fst') ]

fancyToDot :: String -> GV.DotGraph Int
fancyToDot str =
  case parseRegex fancyRegexParser str of
    Left e -> error e
    Right (_, e) -> fstToDot (fromMu (fromRegex e))

mkViz :: (GV.PrintDotRepr dg n) => (a -> dg n) -> a -> IO ()
mkViz f x = do
    _ <- forkIO $ GC.runGraphvizCanvas GC.Dot (f x) GC.Xlib
    return ()

vizFancyAsFST :: String -> IO ()
vizFancyAsFST str = mkViz fancyToDot str
