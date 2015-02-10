{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module KMC.Program.Backends.C where

import           Data.Bits
import qualified Data.Map as M
import           Numeric
import           System.Exit (ExitCode)
import           System.IO
import           System.Process
import           Text.PrettyPrint

import           KMC.Coding
import           KMC.Program.IL
import           KMC.Util.Heredoc
import           KMC.Util.List

{- Utility functions -}
-- | Chunk a list into list of lists of length n, starting from the left. The
-- last list in the result may be shorter than n.
chunk :: Int -> [a] -> [[a]]
chunk n xs
  | null xs = []
  | otherwise = let (l, r) = splitAt n xs in l:chunk n r

-- | Pad a string with spaces on the left
padL :: Int -> String -> String
padL width s = replicate (width - length s) ' ' ++ s

-- | Pad a string with spaces on the right
padR :: Int -> String -> String
padR width s = s ++ replicate (width - length s) ' '

-- | The source code template of the C runtime
crt :: String
crt = [fileQ|crt/crt.c|]

{- Types -}

-- | Supported C types
data CType = UInt8T | UInt16T | UInt32T | UInt64T
  deriving (Eq, Ord, Show)

-- | A prettyprinted C program. The program is divided into sections which are
-- spliced into the template.
data CProg =
  CProg
  { cTables       :: Doc
  , cDeclarations :: Doc
  , cProg         :: Doc
  , cInit         :: Doc
  }

-- | The number of bits each C type can hold
cbitSize :: CType -> Int
cbitSize UInt8T  = 8
cbitSize UInt16T = 16
cbitSize UInt32T = 32
cbitSize UInt64T = 64

-- | The smallest C type that can hold the specified number of bits (or Nothing,
-- if one such does not exist).
minUnsignedType :: Int -> Maybe CType
minUnsignedType n | n < 0   = Nothing
                  | n <= 8  = Just UInt8T
                  | n <= 16 = Just UInt16T
                  | n <= 32 = Just UInt32T
                  | n <= 64 = Just UInt64T
                  | otherwise = Nothing

-- | The prefix added to buffer identifiers.
bufferPrefix :: String
bufferPrefix = "b"

-- | Pretty print a buffer identifier (as reference)
buf :: BufferId -> Doc
buf (BufferId n) = text "&" <> text bufferPrefix <> int n

-- | Pretty print a C type
ctyp :: CType -> Doc
ctyp UInt8T = text "uint8_t"
ctyp UInt16T = text "uint16_t"
ctyp UInt32T = text "uint32_t"
ctyp UInt64T = text "uint64_t"

-- | Pretty print a left aligned cast
cast :: CType -- ^ Destination type
     -> CType -- ^ Source type
     -> Doc   -- ^ Inner pretty printed expression to apply the cast to
     -> Doc
cast ctypeto ctypefrom doc = parens (parens (ctyp ctypeto) <> parens doc)
                             <+> text "<<" <+> int (cbitSize ctypeto - cbitSize ctypefrom)

-- | Pretty print a table lookup with cast
tbl :: CType -- ^ Buffer unit type
    -> CType -- ^ Table unit type
    -> TableId -> Doc
tbl ctypectx ctypetbl (TableId n) = cast ctypectx ctypetbl (text "tbl" <> brackets (int n) <> brackets (text "next"))

-- | Pretty print a block identifier
blck :: BlockId -> Doc
blck (BlockId n) = text "l" <> int n

-- | Render a numeral value of some bith length as a C numeral, such that the
-- most significant bit of the value is the most significant bit of the C type.
-- For example, 0b1010 regarded as a 5-bit number will be represented as a
-- UInt8T as the value 0b0101 0000, and as an UInt16T as the value
-- 0b0101 0000 0000 0000
num :: (Integral a, Bits a, Show a) =>
       CType  -- ^ C type of the constant
    -> Int    -- ^ Bit width of value
    -> a      -- ^ Value
    -> String
num ctype width x
  | cbitSize ctype < width
      = error $ concat ["The requested type "
                        ,show ctype, " cannot represent "
                        ,show width, " bits of information"]
  | otherwise
      = let sh = cbitSize ctype - width
        in "0x" ++ showHex (x `shiftL` sh) ""

-- | Pretty print a table as a C array initializer with entries rendered as the given C type.
prettyTableExpr :: forall delta. (Enum delta, Bounded delta) => CType -> Table delta -> Doc
prettyTableExpr ctype table@(Table { tblTable = tableData }) =
  lbrace <> prettyLines <> rbrace
  where
    prettyLines = vcat
                  . punctuate comma
                  $ [ hsep . punctuate comma . map prettyCell $ line | line <- chunk 8 tableData ]

    prettyCell c = text (padL cellWidth (num ctype bwidth (decodeEnum c :: Integer)))

    bwidth = tblBitSize table
    cellWidth = case ctype of
                     UInt8T  -> 4
                     UInt16T -> 6
                     UInt32T -> 10
                     UInt64T -> 18

-- | Compute the number of bits required to represent a single entry from the table.
tblBitSize :: forall delta. (Enum delta, Bounded delta) => Table delta -> Int
tblBitSize (Table { tblDigitSize = digitSize }) = baseBits * digitSize
  where
    base = fromEnum (maxBound :: delta) - fromEnum (minBound :: delta) + 1
    baseBits = bitWidth 2 base

-- | Take a value to output/append and represent it as a list of integers which
-- each fit the given C type. The second component of each pair is the number of
-- bits of the first component that make up the value encoding.
splitAppends :: forall delta. (Enum delta, Bounded delta) => CType -> [delta] -> [(Integer, Int)]
splitAppends buftype digits = [ (decodeEnum bs, length bs * baseBits) | bs <- groups ]
  where
    base = fromEnum (maxBound :: delta) - fromEnum (minBound :: delta) + 1
    baseBits = bitWidth 2 base
    digitsPerAppend = cbitSize buftype `div` baseBits
    groups = split digits

    split [] = []
    split xs = let (l, r) = splitAt digitsPerAppend xs in l:split r

-- | Pretty print an instruction. Note that the C runtime currently
-- distinguishes between regular buffers and the output buffer, and hence the
-- pretty printer needs to handle this case specially.
prettyInstr :: (Enum delta, Bounded delta) =>
               CType          -- ^ The buffer unit type
            -> CType          -- ^ The table unit type
            -> Program delta  -- ^ The surrounding program
            -> Instr delta -> Doc
prettyInstr buftype tbltype prog instr =
  let streamBuf = progStreamBuffer prog in
  case instr of
    AcceptI            -> text "goto accept;"
    FailI              -> text "goto fail;"
    AppendI bid bs     -> vcat $ do
                            (val, len) <- splitAppends buftype bs
                            let valdoc = text (num buftype len val)
                            let lendoc = int len
                            if bid == streamBuf then
                                return (text "writeconst"
                                        <> parens (hcat [valdoc, comma, lendoc])
                                        <> semi)
                            else
                                return (text "append"
                                        <> parens (hcat [buf bid, comma, valdoc, comma, lendoc])
                                        <> semi)
    AppendTblI bid tid -> let arg = tbl buftype tbltype tid
                              lendoc = int (tblBitSize $ progTables prog M.! tid)
                          in if bid == streamBuf then
                                 text "writeconst" <> parens (hcat [arg, comma, lendoc]) <> semi
                             else
                                 text  "append"
                                 <> parens (hcat [buf bid, comma, arg, comma, lendoc])
                                 <> semi
    ConcatI bid1 bid2  -> if bid1 == streamBuf then
                              text "write" <> parens (buf bid2) <> semi
                          else
                              text "concat"
                              <> parens (hcat [buf bid1, comma, buf bid2])
                              <> semi
    ResetI bid         -> text "reset" <> parens (buf bid) <> semi
    AlignI bid1 bid2   -> text "align"
                          <> parens (hcat [buf bid1, comma, buf bid2])
                          <> semi
    IfI e is           -> text "if" <+> parens (prettyExpr e) $$
                          lbrace $+$
                          nest 3 (prettyBlock buftype tbltype prog is) $+$
                          rbrace
    GotoI blid         -> text "goto" <+> blck blid <> semi
    NextI is           -> text "if" <+> parens (text "!readnext()") $$
                          lbrace $+$
                          nest 3 (prettyBlock buftype tbltype prog is) $+$
                          rbrace

-- | Pretty print a list of instructions.
prettyBlock :: (Enum delta, Bounded delta) => CType -> CType -> Program delta -> Block delta -> Doc
prettyBlock buftype tbltype prog is = vcat $ map (prettyInstr buftype tbltype prog) is

-- | Pretty print a test expression.
prettyExpr :: Expr -> Doc
prettyExpr e =
  case e of
    SymE        -> text "next"
    ConstE n    -> int n
    FalseE      -> int 0
    TrueE       -> int 1
    LteE e1 e2  -> op "<=" e1 e2
    LtE e1 e2   -> op "<" e1 e2
    GteE e1 e2  -> op ">=" e1 e2
    GtE e1 e2   -> op ">" e1 e2
    EqE e1 e2   -> op "==" e1 e2
    OrE e1 e2   -> op "||" e1 e2
    AndE e1 e2  -> op "&&" e1 e2
    NotE e1     -> text "!" <> parens (prettyExpr e1)

  where
    op str e1 e2 = parens (prettyExpr e1 <+> text str <+> prettyExpr e2)

-- | Pretty print all table declarations.
prettyTableDecl :: (Enum delta, Bounded delta) =>
                   CType          -- ^ Table unit type
                -> Program delta  -- ^ Program
                -> Doc
prettyTableDecl tbltype prog =
  if null tables then text "/* no tables */" else
    ctyp tbltype <> brackets (int (length tables)) <> brackets (int tableSize) <+> text "="
    $$ lbrace <> hcat (punctuate comma (map (prettyTableExpr tbltype) tables)) <> rbrace <> semi
  where
    tables    = M.elems $ progTables prog
    tableSize = length . tblTable . head $ tables

-- | Pretty print all buffer declarations.
prettyBufferDecls :: Program delta -> Doc
prettyBufferDecls prog =
  vcat (map bufferDecl (progBuffers prog))
  where
    bufferDecl (BufferId n) = text "buffer_t" <+> text bufferPrefix <> int n <> semi

-- | Pretty print initialization code. This is just a call to init_buffer() for
-- each buffer in the program.
prettyInit :: Program delta -> Doc
prettyInit prog =
  vcat (map bufferInit $ filter (/= progStreamBuffer prog) $ progBuffers prog)
  where
    bufferInit bid = text "init_buffer" <> parens (buf bid) <> semi

prettyProg :: (Enum delta, Bounded delta) => CType -> CType -> Program delta -> Doc
prettyProg buftype tbltype prog =
  text "goto" <+> blck (progInitBlock prog) <> semi
  $$ vcat (map pblock (M.toList $ progBlocks prog))
  where
    pblock (blid, is) =
      blck blid <> char ':'
                <+> prettyBlock buftype tbltype prog is

programToC :: (Enum delta, Bounded delta) => Program delta -> CProg
programToC prog =
  CProg
  { cTables       = prettyTableDecl tbltype prog
  , cDeclarations = prettyBufferDecls prog
  , cInit         = prettyInit prog
  , cProg         = prettyProg buftype tbltype prog
  }
  where
    tbltype = UInt8T
    buftype = UInt64T

renderCProg :: CProg -> String
renderCProg cprog =
    replace "%%TABLES" (render $ cTables cprog)
  . replace "%%DECLS"  (render $ cDeclarations cprog)
  . replace "%%INIT"   (render $ cInit cprog)
  . replace "%%PROG"   (render $ cProg cprog)
  $ crt

renderProgram :: (Enum delta, Bounded delta) => Program delta -> String
renderProgram = renderCProg . programToC

compileProgram :: (Enum delta, Bounded delta) => Program delta -> FilePath -> IO ExitCode
compileProgram prog outPath = do
  let cstr = renderCProg . programToC $ prog
  (Just hin, _, _, hproc) <- createProcess (proc "gcc" ["-O3", "-xc", "-o", outPath, "-"])
                                           { std_in = CreatePipe }
  hPutStrLn hin cstr
  hClose hin
  waitForProcess hproc
