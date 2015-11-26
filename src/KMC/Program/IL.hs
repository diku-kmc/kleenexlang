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
  | IfI        Expr (Block delta)       -- ^ if (e :: Bool) { ... }
  | GotoI      BlockId                  -- ^ goto b
  | NextI      Int Int (Block delta)    -- ^ if (!getChars(min,max)) { ... }
  | ConsumeI   Int                      -- ^ next += i
    -- Buffer stack operations
  | PushI                               -- ^ push new empty buffer to stack
  | PopI BufferId                       -- ^ pop buffer at top of stack and save
                                        -- it in the specified buffer variable
  | WriteI BufferId                     -- ^ write the contents of the given
                                        -- buffer to the buffer at the top of
                                        -- the stack.
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
  deriving (Eq, Show)

type Pipeline delta gamma = Either [Program delta] [(Program delta, Program gamma)]


-----------------------------------------------------
-- Program transformations and post-SST optimizations
-----------------------------------------------------

-- | Optimize a program by removing all tables that implement the identity
-- function and replace all lookups in them with a special instruction,
-- denoting that the current input character in the runtime should be output
-- or appended.
elimIdTables :: (Bounded delta, Enum delta) => Program delta -> Program delta
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
