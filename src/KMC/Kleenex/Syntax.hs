{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module KMC.Kleenex.Syntax
       ( Ident(..)
       , RegIdent(..)
       , Prog(..)
       , Decl(..)
       , Term(..)
       , RProg(..)
       , RTerm(..)
       , RIdent
       ) where

import           Data.ByteString (ByteString)
import qualified Data.Map as M
import           KMC.Syntax.External (Regex)
import           KMC.RangeSet

newtype Ident    = Ident    { fromIdent :: String }  deriving (Eq, Ord, Show)
newtype RegIdent = RegIdent { fromRegIdent :: String } deriving (Eq, Ord, Show)

---------------
-- Full Kleenex
---------------

data Prog i      = Kleenex  { progPipeline :: [Ident]
                            , progDecls :: [Decl i]
                            }
                 deriving (Eq, Ord, Show)

data Decl i     = Decl Ident (Term i)
                | DeclInfo i (Decl i)
                 deriving (Eq, Ord, Show)

data Term i = Var Ident
            | Constant ByteString
            | RE Regex
            | Seq (Term i) (Term i)
            | Sum (Term i) (Term i)
            | Star (Term i)
            | Plus (Term i)
            | Question (Term i)
            | Range (Maybe Int) (Maybe Int) (Term i)
            | SuppressOutput (Term i)
            | One
            | UpdateReg RegIdent [Either RegIdent ByteString]
            | WriteReg RegIdent
            | RedirectReg RegIdent (Term i)
            | TermInfo i (Term i) -- ^ constructor for storing meta-data from parser
          deriving (Eq, Ord, Show)


------------------
-- Reduced Kleenex
------------------

type RIdent = Int

data RProg a b = RProg { rprogPipeline :: [RIdent]
                       , rprogDecls :: M.Map RIdent (RTerm a b)
                       }
  deriving (Eq, Ord, Show)

data RTerm a b =
    RConst b
  | RRead (RangeSet a) Bool -- ^ Range set is an input predicate, boolean value
                            -- indicates whether input symbol is to be copied to
                            -- the output or not.
  | RSeq [RIdent]
  | RSum [RIdent]
  deriving (Eq, Ord, Show)
