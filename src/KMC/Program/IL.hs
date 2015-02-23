{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module KMC.Program.IL where

import qualified Data.Map as M

-- | A representation of a full tabulation of a function 'f' with an enumerable
-- bounded domain. The table is represented as a list, with the ith entry
-- corresponding to f(e_i), where e_i is the i'th element of the domain.
-- Functions may be partial. In the case, the value at an undefined index is
-- arbitrary.
data Table delta = Table { tblTable :: [[delta]], tblDigitSize :: Int }
  deriving (Eq, Ord, Show)

newtype ConstId = ConstId { getConstId :: Int } deriving (Eq, Ord, Show)
newtype BlockId = BlockId { getBlockId :: Int } deriving (Eq, Ord, Show)
newtype TableId = TableId { getTableId :: Int } deriving (Eq, Ord, Show)
newtype BufferId = BufferId { getBufferId :: Int } deriving (Eq, Ord, Show)

data Expr =
    SymE Int          -- ^ next[i] (i less than value of AvailableSymbolsE)
  | AvailableSymbolsE -- ^ Number of available symbols
  | CompareE Int [Int] -- compare(&next[i], str, length(str))
  | ConstE Int
  | FalseE
  | TrueE
  | LteE Expr Expr
  | LtE  Expr Expr
  | GteE Expr Expr 
  | GtE  Expr Expr
  | EqE  Expr Expr
  | OrE  Expr Expr
  | AndE Expr Expr
  | NotE Expr
  deriving (Eq, Ord, Show)

data Instr delta =
    AcceptI                             -- ^ accept (Program stops)
  | FailI                               -- ^ fail   (Program stops)
  | AppendI    BufferId ConstId         -- ^ buf  := buf ++ bs
  | AppendTblI BufferId TableId Int     -- ^ bif ::= buf ++ tbl(id)(next[i])[0 .. sz(id) - 1]
  | ConcatI    BufferId BufferId        -- ^ buf1 := buf1 ++ buf2; reset(buf2)
  | ResetI     BufferId                 -- ^ buf1 := []
  | AlignI     BufferId BufferId        -- ^ align buf1 buf2. assert that buf1 is empty, and
                                        --   make sure that subsequent writes to buf1 are aligned
                                        --   such that concatenating buf2 and buf1 with the current
                                        --   contents of buf2 will be efficient.
                                        --   This instruction is a hint to the runtime, and does not
                                        --   affect the final result.
  | IfI        Expr (Block delta)       -- ^ if (e :: Bool) { ... }
  | GotoI      BlockId                  -- ^ goto b
  | NextI      Int Int (Block delta)    -- ^ if (!getChars(min,max)) { ... }
  | ConsumeI   Int                      -- next += i
  deriving (Eq, Ord, Show)

type Block delta = [Instr delta]

data Program delta =
  Program
  { progTables       :: M.Map TableId (Table delta)
  , progConstants    :: M.Map ConstId [delta]
  , progStreamBuffer :: BufferId
  , progBuffers      :: [BufferId]
  , progInitBlock    :: BlockId
  , progBlocks       :: M.Map BlockId (Block delta)
  }
