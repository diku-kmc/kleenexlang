{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module KMC.Program.Backends.C where

import           Control.Monad (when, join)
import           Data.Bits
import           Data.Char (ord, chr, isPrint, isAscii, isSpace)
import           Data.List (intercalate, maximumBy)
import           Data.Function (on)
import qualified Data.Map as M
import qualified Data.Set as S
import           Numeric
import           System.Exit (ExitCode(..))
import           System.IO
import           System.Process
import           Text.PrettyPrint

import           KMC.Coding
import           KMC.Program.IL
import           KMC.Util.Heredoc

import KMC.SSTCompiler
import Debug.Trace

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

progTemplate :: String -> String -> String -> String -> String -> [String] -> Bool -> String
progTemplate buString tablesString declsString infoString initString progStrings withActions =
 [strQ|
#define NUM_PHASES |] ++ show (if withActions then length progStrings else 2*length progStrings) ++ [strQ|
#define BUFFER_UNIT_T |] ++ buString ++ "\n"
  ++ [fileQ|crt/crt.c|] ++ "\n"
  ++ tablesString ++ "\n"
  ++ declsString ++ [strQ|
void printCompilationInfo()
{
  fprintf(stdout, |]++infoString++[strQ|);
}

void init()
{
|]++initString ++[strQ|
}
|]++concat (zipWith matchTemplate progStrings [1..])++[strQ|
void match(int phase)
{
  switch(phase) {
    |]++intercalate "\n" ["case " ++ show i ++ ": match" ++ show i ++ "(); break;"
                         | i <- [1..(length progStrings)] ]++
    [strQ|
    default:
      fprintf(stderr, "Invalid phase: %d given\n", phase);
      exit(1);
  }
}
|]

matchTemplate :: String -> Int -> String
matchTemplate progString n = [strQ|void match|] ++ show n ++ [strQ|()
{
  int i = 0;
|]++progString++[strQ|
  accept|]++show n++[strQ|:
    return;
  fail|]++show n++[strQ|:
    fprintf(stderr, "Match error at input symbol %zu!\n", count);
    exit(1);
}
|]
  

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
  , cProg         :: [Doc]
  , cInit         :: Doc
  , cBufferUnit   :: Doc
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
bufferPrefix = "buf_"

constPrefix :: String
constPrefix = "const_"

-- | Pretty print a buffer identifier (as reference)
buf :: BufferId -> Doc
buf (BufferId n) = text "&" <> text bufferPrefix <> int n

-- | Pretty print a constant identifier (as reference)
cid :: ConstId -> Int -> Doc
cid (ConstId n) phase = text constPrefix <> int phase <> text "_" <> int n

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
cast ctypeto ctypefrom doc
  | ctypeto == ctypefrom = doc
  | otherwise =
  parens (parens (ctyp ctypeto) <> parens doc)
             <+> text "<<" <+> int (cbitSize ctypeto - cbitSize ctypefrom)

-- | Pretty print a table lookup with cast
tbl :: CType        -- ^ Buffer unit type
    -> CType        -- ^ Table unit type
    -> TableId      -- ^ Table identifier
    -> Int          -- ^ Table offset
    -> Int          -- ^ Number of symbols
    -> Int          -- ^ Symbol size
    -> Maybe String -- ^ Optional dynamic offset
    -> Int
    -> Doc
tbl ctypectx ctypetbl (TableId n) i symbs symbSize mx phase =
  let offsetdoc = maybe (int i)
                        (\s -> int i <+> text "+" <+> text s)
                        mx
  in cast ctypectx ctypetbl
     $ hcat [text "tbl" <> int phase
            ,brackets $ int n
            ,brackets $ if symbSize == 8
                        then text "in_byteptr" <> brackets (offsetdoc)
                        else text "nextcache" <> brackets (offsetdoc)
            ]

-- | Pretty print a block identifier
blck :: BlockId -> Int -> Doc
blck (BlockId n) phase  = text "l" <> int phase <> text "_" <> int n

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


-- | Pretty print an append table instruction, taking into account whether the
-- destination buffer is the output buffer or a regular bufer. An optional
-- dynamic offset variable can be specified for printing append instructions
-- within a loop.
prettyAppendTbl :: (Bounded delta, Enum delta) =>
                   CType
                -> CType
                -> Program delta
                -> BufferId
                -> TableId
                -> Int
                -> Int
                -> Maybe String
                -> Int
                -> Doc
prettyAppendTbl buftype tbltype prog bid tid i symbs mx phase =
  let arg       = tbl buftype tbltype tid i symbs (symbolSize prog) mx phase
      lendoc    = int (tblBitSize $ progTables prog M.! tid)
      streamBuf = progStreamBuffer prog
  in if bid == streamBuf then
         text "outputconst" <> parens (hcat [arg, comma, lendoc]) <> semi
     else
         text  "append"
         <> parens (hcat [buf bid, comma, arg, comma, lendoc])
         <> semi

prettyAppendSym :: BufferId -> BufferId -> Int -> Int -> Int -> Maybe String -> Doc
prettyAppendSym bid outBuf i symbs symbSize mx =
    let offsetdoc = maybe (int i) (\s -> int i <+> text "+" <+> text s) mx
        symb = text "nextcache" <> brackets (offsetdoc)
    in if bid == outBuf then
        text "outputconst" <> parens (hcat [symb, comma, int 8]) <> semi
    else
        text "append"
        <> parens (hcat [buf bid, comma, symb, comma, int 8]) <> semi


-- | Pretty print an instruction. Note that the C runtime currently
-- distinguishes between regular buffers and the output buffer, and hence the
-- pretty printer needs to handle this case specially.
prettyInstr :: forall delta. (Enum delta, Bounded delta) =>
               CType          -- ^ The buffer unit type
            -> CType          -- ^ The table unit type
            -> Program delta  -- ^ The surrounding program
            -> Instr delta -> Int -> Doc
prettyInstr buftype tbltype prog instr phase =
  let streamBuf = progStreamBuffer prog
      symbSize = symbolSize prog
  in
  case instr of
    CacheNextI i symbs -> if symbSize == 8
                          then text ""
                          else  text "nextcache" <> brackets (int i) <+> text "=" <+>
                                text "next" <> parens (hcat [int i, comma, int $ symbSize * symbs])
                          <> semi
    FinishedI is       -> text "if (input_eof())" $$
                          lbrace $+$
                          nest 3 (prettyBlock buftype tbltype prog is phase) $+$
                          rbrace
    AcceptI            -> text "goto accept" <> int phase <> semi
    FailI              -> text "goto fail" <> int phase <> semi
    AppendI bid constid ->
      let base     = fromEnum (maxBound :: delta) - fromEnum (minBound :: delta) + 1
          baseBits = bitWidth 2 base
          lendoc   = text $ show $ length (progConstants prog M.! constid) * baseBits
      in if bid == streamBuf then
             text "outputarray"
               <> parens (hcat [cid constid phase, comma, lendoc])
               <> semi
         else
             text "appendarray"
               <> parens (hcat [buf bid, comma, cid constid phase, comma, lendoc])
               <> semi
    AppendTblI bid tid i symbs -> prettyAppendTbl buftype tbltype prog bid tid i symbs Nothing phase
    AppendSymI bid i symbs  -> prettyAppendSym bid streamBuf i symbs (symbolSize prog) Nothing
    ConcatI bid1 bid2  -> if bid1 == streamBuf then
                              text "output" <> parens (buf bid2) <> semi
                          else
                              text "concat"
                              <> parens (hcat [buf bid1, comma, buf bid2])
                              <> semi
    ResetI bid         -> text "reset" <> parens (buf bid) <> semi
    AlignI bid1 bid2   -> text "align"
                          <> parens (hcat [buf bid1, comma, buf bid2])
                          <> semi
    IfI e is           -> text "if" <+> parens (prettyExpr (symbolSize prog) e) $$
                          lbrace $+$
                          nest 3 (prettyBlock buftype tbltype prog is phase) $+$
                          rbrace
    GotoI blid         -> text "goto" <+> blck blid phase <> semi
    NextI minL maxL is -> text "if"
                            <+> parens (text "!readnext" <> parens (hcat [int $ minL*symbSize, comma, int $ maxL*symbSize])) $$
                          lbrace $+$
                          nest 3 (prettyBlock buftype tbltype prog is phase) $+$
                          rbrace
    ConsumeI i         -> text "consume" <> parens (int $ i*symbSize) <> semi
    ChangeOut bid      -> text "pushoutbuf" <> parens (buf bid) <> semi
    RestoreOut         -> text "popoutbuf()" <> semi

appendSpan :: BufferId -> TableId -> Int -> Block delta -> Maybe (Int, Block delta)
appendSpan bid tid i is =
  let (is1, is2) = span isAppendTbl is
  in if not (null is1)
        && and (zipWith (==) [ j | AppendTblI _ _ j _ <- is1 ] [i+1..])
     then
         Just (length is1, is2)
     else
         Nothing
    where
      isAppendTbl (AppendTblI bid' tid' _ _) = bid == bid' && tid == tid'
      isAppendTbl _ = False

appendSymSpan :: BufferId -> Int -> Block delta -> Maybe (Int, Block delta)
appendSymSpan bid i is =
    let (is1, is2) = span isAppendSym is
    in if not (null is1)
           && and (zipWith (==) [ j | AppendSymI _ j _ <- is1 ] [i+1..])
       then
           Just (length is1, is2)
       else
           Nothing
    where
      isAppendSym (AppendSymI bid' _ _) = bid == bid'
      isAppendSym _ = False

-- | Pretty print a list of instructions.
prettyBlock :: (Enum delta, Bounded delta) => CType -> CType -> Program delta -> Block delta -> Int -> Doc
prettyBlock buftype tbltype prog is phase = go is
  where
    go [] = empty
    go (app@(AppendSymI bid i symbs):is) =
        case appendSymSpan bid i is of
          Nothing -> prettyInstr buftype tbltype prog app phase $+$ go is
          Just (n, is') ->
              vcat [ hcat [ text "for"
                          , parens (text "i = 0; i < " <> int (n+1) <> text "; i++")
                          ]
                   , lbrace
                   , nest 3 (prettyAppendSym bid (progStreamBuffer prog) i symbs (symbolSize prog) (Just "i"))
                   , rbrace
                   , go is'
                   ]
    go (app@(AppendTblI bid tid i symbs):is) =
      case appendSpan bid tid i is of
        Nothing       -> prettyInstr buftype tbltype prog app phase $+$ go is
        Just (n, is') ->
          vcat [hcat [text "for"
                     ,parens (text "i = 0; i < " <> int (n+1) <> text "; i++")]
               ,lbrace
               ,nest 3
                  (prettyAppendTbl buftype tbltype prog bid tid i symbs (Just "i") phase)
               ,rbrace
               ,go is'
               ]
    go (instr:is) = prettyInstr buftype tbltype prog instr phase $+$ go is

prettyStr :: [Int] -> Doc
prettyStr = doubleQuotes . hcat . map prettyOrd
    where
      prettyOrd n = let c = chr n in
                    if and [isPrint c
                           ,isAscii c
                           ,not (isSpace c)
                           ,not (c `elem` "\"\\")] then
                      char c
                    else
                      doubleQuotes . doubleQuotes $ text "\\x" <> text (showHex n "")

-- | Pretty print a test expression.
prettyExpr :: Int -> Expr -> Doc
prettyExpr symbSize e =
  case e of
    SymE i symbs        -> if symbSize == 8
                           -- Assume that we have byte alignment and no variable length symbols
                           then text "in_byteptr" <> brackets (int i)
                           else text "nextcache" <> brackets (int i)
    AvailableSymbolsE n -> text "avail"<> parens (int $ n*symbSize)
    CompareE i str      -> text "cmp"
                             <> parens (hcat $ punctuate comma
                                          [text "NEXT" <> parens (int i)
                                          ,text "(unsigned char *)" <+> prettyStr str
                                          ,int (length str)])
    ConstE n            -> let c = chr n in
                           if isPrint c && isAscii c && c /= '\\' && c /= '\'' then
                               quotes (char c)
                           else
                               int n
    FalseE              -> int 0
    TrueE               -> int 1
    LteE e1 e2          -> op "<=" e1 e2
    LtE e1 e2           -> op "<" e1 e2
    GteE e1 e2          -> op ">=" e1 e2
    GtE e1 e2           -> op ">" e1 e2
    EqE e1 e2           -> op "==" e1 e2
    OrE e1 e2           -> op "||" e1 e2
    AndE e1 e2          -> op "&&" e1 e2
    NotE e1             -> text "!" <> parens (prettyExpr symbSize e1)

  where
    op str e1 e2 = parens (prettyExpr symbSize e1 <+> text str <+> prettyExpr symbSize e2)

-- | Pretty print all table declarations.
prettyTableDecl :: (Enum delta, Bounded delta, Enum gamma, Bounded gamma, Ord gamma, Ord delta) =>
                   CType                 -- ^ Table unit type
                -> Pipeline delta gamma  -- ^ Programs
                -> Doc
prettyTableDecl tbltype pipeline = case pipeline of
                                    Left  progs -> vcat $ zipWith tableDecl progs [1..]
                                    Right progs -> vcat $ zipWith combine progs [1,3..]
  where
    combine (p,a) i = tableDecl p i $+$ tableDecl a (i+1)
    tableDecl prog phase =
        if null tables
        then text "/* no tables */"
        else text "const" <+> ctyp tbltype <+> text "tbl" <> int phase
          <> brackets (int (length tables)) <> brackets (int tableSize) <+> text "="
          $$ lbrace <> vcat (punctuate comma (map (prettyTableExpr tbltype) tables))
          <> rbrace <> semi
      where
        tables    = M.elems $ progTables prog
        tableSize = length . tblTable . maximumBy (compare `on` (length . tblTable)) $ tables

-- | Pretty print all buffer declarations.
prettyBufferDecls :: Pipeline delta gamma -> Doc
prettyBufferDecls progs =
  vcat (map bufferDecl $ neededBuffers progs)
  where
    bufferDecl (BufferId n) = text "buffer_t" <+> text bufferPrefix <> int n <> semi

prettyConstantDecls :: (Enum delta, Bounded delta, Enum gamma, Bounded gamma) =>
                        CType -- ^ Buffer unit type
                     -> Pipeline delta gamma
                     -> Doc
prettyConstantDecls buftype pipeline = case pipeline of
    Left progs -> vcat $ zipWith constantDecls progs [1..]
    Right progs -> vcat $ zipWith combine progs [1,3..]
  where
    constantDecls prog i = vcat $ map (constantDecl i) $ M.toList $ progConstants prog
    combine (p,a) i = constantDecls p i $+$ constantDecls a (i+1)
    constantDecl phase (ConstId n, deltas) =
      let chunks = splitAppends buftype deltas
          constdocs = map (\(c, nbits) -> text $ num buftype nbits c) chunks
          comment = join $ map escape $ map (chr . fromEnum) deltas
          escape c = if isPrint c && isAscii c && isSafe c then [c] else "\\x" ++ showHex (ord c) ""
          isSafe c = c /= '\\'
      in vcat [ text "//" <+> text comment
              , text "const" <+> text "buffer_unit_t" <+> text constPrefix
                <> int phase <> text "_" <> int n
                <> brackets (int $ length chunks)
                <+> text "=" <+>
                (braces $ hcat $ punctuate comma constdocs) <> semi
             ]

neededBuffers :: Pipeline delta gamma -> [BufferId]
neededBuffers pipeline = case pipeline of
        Left  progs -> deduplicate $ concatMap progBuffers progs
        Right progs -> deduplicate $ concatMap combine progs
    where
        combine (p,a) = progBuffers p ++ progBuffers a
        deduplicate = S.toList . S.fromList

neededNonStreamBuffers :: Pipeline delta gamma -> [BufferId]
neededNonStreamBuffers pipeline = S.toList . S.unions $ case pipeline of
    Left  progs -> map needed progs
    Right progs -> map (\(p,a) -> S.union (needed p) (needed a)) progs
  where
    needed prog = S.fromList $ filter (/= progStreamBuffer prog) $ progBuffers prog




-- | Pretty print initialization code. This is just a call to init_buffer() for
-- each buffer in the program.
prettyInit :: Pipeline delta gamma -> Doc
prettyInit progs =
  vcat (map bufferInit $ neededNonStreamBuffers progs)
  where
    bufferInit bid = text "init_buffer" <> parens (buf bid) <> semi

prettyProg :: (Enum delta, Bounded delta) => CType -> CType -> Program delta -> Int -> Doc
prettyProg buftype tbltype prog phase =
  text "goto" <+> blck (progInitBlock prog) phase <> semi
  $$ vcat (map pblock (M.toList $ progBlocks prog))
  where
    pblock (blid, is) =
      blck blid phase <>  char ':'
                      <+> prettyBlock buftype tbltype prog is phase

programsToC :: (Enum delta, Bounded delta, Enum gamma
               , Bounded gamma, Ord gamma, Ord delta) => CType -> Pipeline delta gamma -> CProg
programsToC buftype pipeline =
  CProg
  { cTables       = prettyTableDecl tbltype pipeline
  , cDeclarations = prettyBufferDecls pipeline
                    $+$ prettyConstantDecls buftype pipeline
  , cInit         = prettyInit pipeline
  , cProg         = prettyProgs
  , cBufferUnit   = ctyp buftype
  }
  where
    tbltype = UInt8T
    prettyProgs = case pipeline of
                    Left progs  -> zipWith (prettyProg buftype tbltype) progs [1..]
                    Right progs -> concat $ zipWith combine progs [1,3..]
    combine (p,a) i = [prettyProg buftype tbltype p i, prettyProg buftype tbltype a (i+1)]

renderCProg :: String -> Bool -> CProg -> String
renderCProg compInfo withActions cprog =
  progTemplate (render $ cBufferUnit cprog)
               (render $ cTables cprog)
               (render $ cDeclarations cprog)
               compInfo
               (render $ cInit cprog)
               (map render $ cProg cprog)
               withActions

ccVersion :: FilePath -> IO String
ccVersion comp = do
  -- gcc/clang prints version info on stderr
  (_, _, err, _) <- createProcess (proc comp ["-v"])
                    { std_err = CreatePipe }
  let hErr = maybe (error "ccVersion: bogus handle") id err
  errStr <- hGetContents hErr
  return $ intercalate "\\n" $ lines $ errStr

compileProgram :: (Enum delta, Bounded delta, Enum gamma, Bounded gamma, Ord gamma, Ord delta) =>
                  CType
               -> Int
               -> Bool
               -> Pipeline delta gamma
               -> Maybe String -- ^ Optional descriptor to put in program.
               -> FilePath     -- ^ Path to C compiler
               -> Maybe FilePath
               -> Maybe FilePath
               -> Bool -- ^ Use word alignment
               -> IO ExitCode
compileProgram buftype optLevel optQuiet pipeline desc comp moutPath cCodeOutPath wordAlign = do
  cver <- ccVersion comp
  let info = (maybe noOutInfo (outInfo cver) moutPath)
  let withActions = case pipeline of
                      Left  _ -> False
                      Right _ -> True
  let cstr = renderCProg info withActions . programsToC buftype $ pipeline
  case cCodeOutPath of
    Nothing -> return ()
    Just p  -> do
      when (not optQuiet) $ putStrLn $ "Writing C source to " ++ p
      writeFile p cstr
  case moutPath of
    Nothing -> return ExitSuccess
    Just outPath -> do
      when (not optQuiet) $
        putStrLn $ "Running compiler cmd: '" ++ intercalate " " (comp : compilerOpts outPath) ++ "'"
      (Just hin, _, _, hproc) <- createProcess (proc comp (compilerOpts outPath))
                                               { std_in = CreatePipe }
      hPutStrLn hin cstr
      hClose hin
      waitForProcess hproc
  where
    compilerOpts binPath = [ "-O" ++ show optLevel, "-xc"
                           , "-o", binPath
                           , "-lm"
                           -- , "-Wno-tautological-constant-out-of-range-compare"
                           ]
                           ++ (if wordAlign then ["-D FLAG_WORDALIGNED"] else [])
                           ++ ["-"]
    quote s = "\"" ++ s ++ "\""
    noOutInfo = quote $ intercalate "\\n"
                [ "No object file generated!"
                , maybe "No environment info available" id desc
                , "" -- adds newline at the end
                ]
    outInfo cver path = quote $ intercalate "\\n"
                        [ "Compiler info: "
                        , cver
                        , ""
                        , "CC cmd: "
                        , intercalate " " (comp : compilerOpts path)
                        , ""
                        , maybe "No environment info available" id desc
                        , "" -- adds newline at the end
                        ]
    
