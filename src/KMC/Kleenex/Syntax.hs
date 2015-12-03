{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module KMC.Kleenex.Syntax
       ( Ident(..)
       , RegIdent(..)
       , Prog(..)
       , Decl(..)
       , Term(..)
       , eraseInfoProg
       , eraseInfoDecl
       , eraseInfoTerm
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

eraseInfoProg :: Prog i -> Prog a
eraseInfoProg (Kleenex pipeline ds) = Kleenex pipeline (map eraseInfoDecl ds)

eraseInfoDecl :: Decl i -> Decl a
eraseInfoDecl (DeclInfo _ d) = eraseInfoDecl d
eraseInfoDecl (Decl ident t) = Decl ident (eraseInfoTerm t)

eraseInfoTerm :: Term i -> Term a
eraseInfoTerm (TermInfo _ t) = eraseInfoTerm t
eraseInfoTerm (RedirectReg ident t) = RedirectReg ident (eraseInfoTerm t)
eraseInfoTerm (SuppressOutput t) = SuppressOutput (eraseInfoTerm t)
eraseInfoTerm (Range m n t) = Range m n (eraseInfoTerm t)
eraseInfoTerm (Question t) = Question (eraseInfoTerm t)
eraseInfoTerm (Plus t) = Plus (eraseInfoTerm t)
eraseInfoTerm (Star t) = Star (eraseInfoTerm t)
eraseInfoTerm (Sum t1 t2) = Sum (eraseInfoTerm t1) (eraseInfoTerm t2)
eraseInfoTerm (Seq t1 t2) = Seq (eraseInfoTerm t1) (eraseInfoTerm t2)
eraseInfoTerm (Var x) = Var x
eraseInfoTerm (Constant c) = Constant c
eraseInfoTerm (RE e) = RE e
eraseInfoTerm One = One
eraseInfoTerm (UpdateReg ident es) = UpdateReg ident es
eraseInfoTerm (WriteReg ident) = WriteReg ident

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
