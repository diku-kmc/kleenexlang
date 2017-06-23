{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module KMC.Program.IL where

import KMC.Util.Coding (decodeEnum)
import qualified Data.Map as M

-- | A representation of a full tabulation of a function 'f' with an enumerable
-- bounded domain. The table is represented as a list, with the ith entry
-- corresponding to f(e_i), where e_i is the i'th element of the domain.
-- Functions may be partial. In the case, the value at an undefined index is
-- arbitrary.
data Table = Table { tblTable :: [[Int]], tblDigitSize :: Int }
  deriving (Eq, Ord, Show)

newtype ConstId = ConstId { getConstId :: Int } deriving (Eq, Ord, Show)
newtype BlockId = BlockId { getBlockId :: Int } deriving (Eq, Ord, Show)
newtype TableId = TableId { getTableId :: Int } deriving (Eq, Ord, Show)
newtype BufferId = BufferId { getBufferId :: Int } deriving (Eq, Ord, Show)

data Expr =
    SymE Int           -- ^ next[i] (i less than value of AvailableSymbolsE)
  | AvailableSymbolsE  -- ^ Number of available symbols
  | IsFinalChunk       -- ^ Are we in the final chunk so we can accept? Always true
                       --   for standard sequential Kleenex
  | CompareE Int [Int] -- ^ compare(&next[i], str, length(str))
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

data Instr =
    AcceptI                             -- ^ accept (Program stops)
  | FailI                               -- ^ fail due to end of input reached      (Program stops)
  | NoMoveI                             -- ^ fail due to no applicable transaction (Program stops)
  | AppendI    BufferId ConstId         -- ^ buf  := buf ++ bs
  | AppendTblI BufferId TableId Int     -- ^ buf  := buf ++ tbl(id)(next[i])[0 .. sz(id) - 1]
  | AppendSymI BufferId Int             -- ^ buf  := buf ++ next[i]
  | ConcatI    BufferId BufferId        -- ^ buf1 := buf1 ++ buf2; reset(buf2)
  | ResetI     BufferId                 -- ^ buf1 := []
  | AlignI     BufferId BufferId        -- ^ align buf1 buf2. assert that buf1 is empty, and
                                        --   make sure that subsequent writes to buf1 are aligned
                                        --   such that concatenating buf2 and buf1 with the current
                                        --   contents of buf2 will be efficient.
                                        --   This instruction is a hint to the runtime, and does not
                                        --   affect the final result.
  | IfI        Expr Block               -- ^ if (e :: Bool) { ... }
  | GotoI      BlockId                  -- ^ goto b
  | NextI      Int Int Block            -- ^ if (!getChars(min,max)) { ... }
  | ConsumeI   Int                      -- ^ next += i
  deriving (Eq, Ord, Show)

type Block = [Instr]

data Program =
  Program
  { -- | Number of bits required to code a single input symbol
    progInBits       :: Int
    -- ^ Number of bits required to code a single output symbol
  , progOutBits      :: Int
    -- ^ Function lookup tables
  , progTables       :: M.Map TableId Table
    -- ^ Constant tables (each constant is a sequence of output symbols)
  , progConstants    :: M.Map ConstId [Int]
    -- ^ The designated output buffer
  , progStreamBuffer :: BufferId
    -- ^ All buffers
  , progBuffers      :: [BufferId]
    -- ^ Initial block
  , progInitBlock    :: BlockId
    -- ^ Block map
  , progBlocks       :: M.Map BlockId Block
  }
  deriving (Eq, Show)

type Pipeline = Either [Program] [(Program, Program)]


-----------------------------------------------------
-- Program transformations and post-SST optimizations
-----------------------------------------------------

-- | Optimize a program by removing all tables that implement the identity
-- function and replace all lookups in them with a special instruction,
-- denoting that the current input character in the runtime should be output
-- or appended.
elimIdTables :: Program -> Program
elimIdTables prog = prog { progTables = rest
                         , progBlocks = M.map elimTables $ progBlocks prog
                         }
    where
      (idTables, rest) = M.partition isIdTable $ progTables prog
      isIdTable = all cmp . zip [0..] . tblTable
      cmp (ix, sym) = ix == (decodeEnum sym :: Integer)
      elimTables = map (instrElim (M.keys idTables))
      instrElim _ (IfI expr block) = IfI expr (elimTables block)
      instrElim _ (NextI min' max' block) = NextI min' max' (elimTables block)
      instrElim tids ins@(AppendTblI bid tid i) =
          if tid `elem` tids
          then AppendSymI bid i
          else ins
      instrElim _ ins = ins
