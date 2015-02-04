{-# LANGUAGE GADTs #-}
module KMC.Program.IL where

import Data.Word
import qualified Data.Map as M

-- | Identifiers for basic blocks
newtype BlockId  = BlockId { getBlockId :: Int } deriving (Eq, Ord, Show)
-- | Identifiers for lookup tables
newtype TableId  = TableId { getTableId :: Int } deriving (Eq, Ord, Show)
-- | Identifiers for buffers
newtype BufferId = BuferId { getBufferId :: Int } deriving (Eq, Ord, Show)

data Table inty tbty =
  Table
  { tblGenerator :: inty -> tbty
  , tblLowerBound :: inty
  , tblUpperBound :: inty
  }

data Cast a b where
  W8W16  :: Cast Word8  Word16
  W8W32  :: Cast Word8  Word32
  W8W64  :: Cast Word8  Word64
  W16W32 :: Cast Word16 Word32
  W16W64 :: Cast Word16 Word64
  W32W64 :: Cast Word32 Word64

data Expr inty tbty b where
  TrueE  :: Expr inty tbty Bool
  FalseE :: Expr inty tbty Bool
  SymE   :: Expr inty tbty inty
  ConstE :: Ty b -> b -> Expr inty tbty b
  TblE   :: TableId -> Expr inty tbty tbty
  CastE  :: Cast b b'           -> Expr inty tbty b    -> Expr inty tbty b'
  LteE   :: Expr inty tbty b    -> Expr inty tbty b    -> Expr inty tbty Bool
  LtE    :: Expr inty tbty b    -> Expr inty tbty b    -> Expr inty tbty Bool
  GteE   :: Expr inty tbty b    -> Expr inty tbty b    -> Expr inty tbty Bool 
  GtE    :: Expr inty tbty b    -> Expr inty tbty b    -> Expr inty tbty Bool
  EqE    :: Expr inty tbty b    -> Expr inty tbty b    -> Expr inty tbty Bool
  OrE    :: Expr inty tbty Bool -> Expr inty tbty Bool -> Expr inty tbty Bool
  AndE   :: Expr inty tbty Bool -> Expr inty tbty Bool -> Expr inty tbty Bool
  NotE   :: Expr inty tbty Bool -> Expr inty tbty Bool

data Ty t where
  W8  :: Ty Word8
  W16 :: Ty Word16
  W32 :: Ty Word32
  W64 :: Ty Word64

data Instr inty tbty where
  AcceptI :: Instr inty tbty                                         -- ^ accept (Program stops)
  FailI   :: Instr inty tbty                                         -- ^ fail   (Program stops)
  AppendI :: BufferId -> Ty t -> Expr inty tbty t -> Instr inty tbty -- ^ buf  := buf ++ (e :: t)
  ConcatI :: BufferId -> BufferId -> Instr inty tbty                 -- ^ buf1 := buf1 ++ buf2
  ResetI  :: BufferId -> Instr inty tbty                             -- ^ buf1 := []
  OffsetI :: BufferId -> Int -> Instr inty tbty                      -- ^ (assert buf1 == []) buf1.offset = i
  IfI     :: Expr inty tbty Bool -> BlockId -> Instr inty tbty       -- ^ if (e :: Bool) goto b
  GotoI   :: BlockId -> Instr inty tbty                              -- ^ goto b
  NextI   :: BlockId -> Instr inty tbty                              -- ^ if (!next()) goto b

type Block inty tbty = [Instr inty tbty]

-- | A program
data Program inty tbty =
  Program
  { progTables       :: M.Map TableId (Table inty tbty)
  , progStreamBuffer :: BufferId
  , progBuffers      :: [BufferId]
  , progInitBlock    :: BlockId
  , progBlocks       :: M.Map BlockId (Block inty tbty)
  }
