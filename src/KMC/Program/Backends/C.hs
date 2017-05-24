{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module KMC.Program.Backends.C where

import           Control.Monad (join)
import           Control.Monad.Trans
import           Data.Bits
import           Data.Char (ord, chr, isPrint, isAscii, isSpace)
import           Data.List (intercalate, isInfixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import           Numeric
import           System.Exit (ExitCode(..))
import           System.IO
import           System.Process
import           Text.PrettyPrint
import           Text.Printf

import           KMC.Util.Coding
import           KMC.Program.IL
import           KMC.Util.Heredoc

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

progTemplate :: String -> String -> String -> String -> String -> String -> String -> [String] -> String
progTemplate buString tablesString declsString infoString initString stateTable stateCount progStrings =
 [strQ|
#define NUM_PHASES |] ++ show (length progStrings) ++ [strQ|
#define BUFFER_UNIT_T |] ++ buString ++ "\n"
--  ++ [fileQ|crt/crt.h|] ++ "\n"
  ++ [fileQ|crt/crt.c|] ++ "\n"
  ++ tablesString ++ "\n\n"
  ++ stateTable ++ "\n"
  ++ stateCount ++ "\n\n"
  ++ declsString ++ [strQ|
void printCompilationInfo()
{
  fprintf(stdout, |]++infoString++[strQ|);
}

transducer_state* init(unsigned char* input, size_t input_size)
{
|]++initString ++[strQ|
}

|]++concat (zipWith matchTemplate progStrings [1..])++[strQ|
int match(int phase, int start_state, transducer_state* tstate, void (*callback)(transducer_state*))
{
  switch(phase) {
    |]++intercalate "\n" ["case " ++ show i ++ ":\n\
    \      return match" ++ show i ++ "(phase, start_state, tstate, callback);\n\
    \      break;"
                         | i <- [1..(length progStrings)] ]++
    [strQ|
    default:
      fprintf(stderr, "Invalid phase: %d given\n", phase);
      exit(1);
  }
}
|]

matchTemplate :: String -> Int -> String
matchTemplate progString n =
  [strQ|int match|] ++ show n ++ [strQ|(int phase, int start_state, transducer_state* tstate, void (*callback)(transducer_state*))
{
|]++progString++[strQ|
  //accept|]++show n++[strQ|:
  //  return;
  //fail|]++show n++[strQ|:
  //  fprintf(stderr, "Match error at input symbol %zu, (next char: %u)!\n", count, next[0]);
  //  exit(1);
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
  { cTables         :: Doc
  , cDeclarations   :: Doc
  , cProg           :: [Doc]
  , cInit           :: Doc
  , cBufferUnit     :: Doc
  , cJumptable      :: Doc
  , cStateTableVar  :: Doc
  , cStateCountVar  :: Doc
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
bufferPrefix = "tstate->buffers"

constPrefix :: String
constPrefix = "const_"

-- | Pretty print a buffer identifier (as reference)
buf :: BufferId -> Doc
buf (BufferId n) = text bufferPrefix <> brackets (int n)

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
    -> Maybe String -- ^ Optional dynamic offset
    -> Int
    -> Doc
tbl ctypectx ctypetbl (TableId n) i mx phase =
  let offsetdoc = maybe (int i)
                        (\s -> int i <+> text "+" <+> text s)
                        mx
  in cast ctypectx ctypetbl
     $ hcat [text "tbl" <> int phase
            ,brackets $ int n
            ,brackets $ text "tstate->inbuf->next" <> brackets offsetdoc
            ]

-- | Pretty print a block identifier
blck :: BlockId -> Int -> Doc
blck (BlockId n) phase  = text "l" <> int phase <> text "_" <> int n

-- | Pretty print a block id (state only)
stateNr :: BlockId -> Doc
stateNr (BlockId n) = int n

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
prettyTableExpr :: Int -> CType -> Table -> Doc
prettyTableExpr outBits ctype (Table { tblTable = tableData, tblDigitSize = digitSize }) =
  lbrace <> prettyLines <> rbrace
  where
    prettyLines = vcat
                  . punctuate comma
                  $ [ hsep . punctuate comma . map prettyCell $ line | line <- chunk 8 tableData ]

    prettyCell c =
      let n    = decode (2 ^ outBits) (map toInteger c) :: Integer
          ndoc = num ctype bwidth n
      in text $ padL cellWidth ndoc

    bwidth = outBits * digitSize
    cellWidth = case ctype of
                     UInt8T  -> 4
                     UInt16T -> 6
                     UInt32T -> 10
                     UInt64T -> 18

-- | Take a value to output/append and represent it as a list of integers which
-- each fit the given C type. The second component of each pair is the number of
-- bits of the first component that make up the value encoding.
splitAppends :: Int -> CType -> [Int] -> [(Integer, Int)]
splitAppends outBits buftype digits = [ (decodeEnum bs, length bs * outBits) | bs <- groups ]
  where
    digitsPerAppend = cbitSize buftype `div` outBits
    groups = split digits

    split [] = []
    split xs = let (l, r) = splitAt digitsPerAppend xs in l:split r


-- | Pretty print an append table instruction, taking into account whether the
-- destination buffer is the output buffer or a regular bufer. An optional
-- dynamic offset variable can be specified for printing append instructions
-- within a loop.
prettyAppendTbl :: CType
                -> CType
                -> Program
                -> BufferId
                -> TableId
                -> Int
                -> Maybe String
                -> Int
                -> Doc
prettyAppendTbl buftype tbltype prog bid tid i mx phase =
  let arg       = tbl buftype tbltype tid i mx phase
      bwidth    = progOutBits prog * (tblDigitSize $ progTables prog M.! tid)
      lendoc    = int bwidth
      streamBuf = progStreamBuffer prog
  in  text  "append"
      <> parens (hcat [buf bid, comma, arg, comma, lendoc])
      <> semi
      $$ (if bid == streamBuf then
            text "//Call callback"
          else empty)

prettyAppendSym :: BufferId -> BufferId -> Int -> Maybe String -> Doc
prettyAppendSym bid outBuf i mx =
    let offsetdoc = maybe (int i) (\s -> int i <+> text "+" <+> text s) mx
        symb = text "tstate->inbuf->next" <> brackets offsetdoc
    in text "append"
        <> parens (hcat [buf bid, comma, symb, comma, int 8]) <> semi
        $$ (if bid == outBuf then
              text "//Call callback"
            else empty)


-- | Pretty print an instruction. Note that the C runtime currently
-- distinguishes between regular buffers and the output buffer, and hence the
-- pretty printer needs to handle this case specially.
prettyInstr :: CType          -- ^ The buffer unit type
            -> CType          -- ^ The table unit type
            -> Program        -- ^ The surrounding program
            -> Instr -> Int -> Doc
prettyInstr buftype tbltype prog instr phase =
  let streamBuf = progStreamBuffer prog in
  case instr of
    AcceptI            -> text "return state; //accept"
    FailI              -> text "return state; //fail"
    NoMoveI            -> text "return -1;"
    AppendI bid constid ->
      let lendoc   = text $ show $ length (progConstants prog M.! constid) * progOutBits prog
      in  text "appendarray"
          <> parens (hcat [buf bid, comma, cid constid phase, comma, lendoc])
          <> semi
          $$ (if bid == streamBuf then
                text "//Call callback"
              else empty)
    AppendTblI bid tid i -> prettyAppendTbl buftype tbltype prog bid tid i Nothing phase
    AppendSymI bid i   -> prettyAppendSym bid streamBuf i Nothing
    ConcatI bid1 bid2  -> text "concat"
                          <> parens (hcat [buf bid1, comma, buf bid2])
                          <> semi
                          $$ (if bid1 == streamBuf then
                                text "//Call callback"
                              else empty)
    ResetI bid         -> text "reset" <> parens (buf bid) <> semi
    AlignI bid1 bid2   -> text "align"
                          <> parens (hcat [buf bid1, comma, buf bid2])
                          <> semi
    IfI e is           -> text "if" <+> parens (prettyExpr e) $$
                          lbrace $+$
                          nest 3 (prettyBlock buftype tbltype prog is phase) $+$
                          rbrace
    GotoI blid         -> text "goto" <+> blck blid phase <> semi
    NextI minL _ is -> text "if"
                            <+> parens (text "left <" <+> int minL) $$
                          lbrace $+$
                          nest 3 (prettyBlock buftype tbltype prog is phase) $+$
                          rbrace
    ConsumeI i         -> text "consume" <> parens (hcat [text "tstate", comma, int i]) <> semi $$ text "left -=" <+> int i <> semi

appendSpan :: BufferId -> TableId -> Int -> Block -> Maybe (Int, Block)
appendSpan bid tid i is =
  let (is1, is2) = span isAppendTbl is
  in if not (null is1)
        && and (zipWith (==) [ j | AppendTblI _ _ j <- is1 ] [i+1..])
     then
         Just (length is1, is2)
     else
         Nothing
    where
      isAppendTbl (AppendTblI bid' tid' _) = bid == bid' && tid == tid'
      isAppendTbl _ = False

appendSymSpan :: BufferId -> Int -> Block -> Maybe (Int, Block)
appendSymSpan bid i is =
    let (is1, is2) = span isAppendSym is
    in if not (null is1)
           && and (zipWith (==) [ j | AppendSymI _ j <- is1 ] [i+1..])
       then
           Just (length is1, is2)
       else
           Nothing
    where
      isAppendSym (AppendSymI bid' _) = bid == bid'
      isAppendSym _ = False

-- | Pretty print a list of instructions.
prettyBlock :: CType -> CType -> Program -> Block -> Int -> Doc
prettyBlock buftype tbltype prog instrs phase = go instrs
  where
    go [] = empty
    go (app@(AppendSymI bid i):is) =
        case appendSymSpan bid i is of
          Nothing -> prettyInstr buftype tbltype prog app phase $+$ go is
          Just (n, is') ->
              vcat [ hcat [ text "for"
                          , parens (text "i = 0; i < " <> int (n+1) <> text "; i++")
                          ]
                   , lbrace
                   , nest 3 (prettyAppendSym bid (progStreamBuffer prog) i (Just "i"))
                   , rbrace
                   , go is'
                   ]
    go (app@(AppendTblI bid tid i):is) =
      case appendSpan bid tid i is of
        Nothing       -> prettyInstr buftype tbltype prog app phase $+$ go is
        Just (n, is') ->
          vcat [hcat [text "for"
                     ,parens (text "i = 0; i < " <> int (n+1) <> text "; i++")]
               ,lbrace
               ,nest 3
                  (prettyAppendTbl buftype tbltype prog bid tid i (Just "i") phase)
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
prettyExpr :: Expr -> Doc
prettyExpr e =
  case e of
    SymE i            -> text "tstate->inbuf->next" <> brackets (int i)
    AvailableSymbolsE -> text "left"
    CompareE i str    -> text "cmp"
                           <> parens (hcat $ punctuate comma
                                        [text "&next" <> brackets (int i)
                                        ,text "(unsigned char *)" <+> prettyStr str
                                        ,int (length str)])
    ConstE n          -> let c = chr n in
                         if isPrint c && isAscii c && c /= '\\' && c /= '\'' then
                             quotes (char c)
                         else
                             int n
    FalseE            -> int 0
    TrueE             -> int 1
    LteE e1 e2        -> op "<=" e1 e2
    LtE e1 e2         -> op "<" e1 e2
    GteE e1 e2        -> op ">=" e1 e2
    GtE e1 e2         -> op ">" e1 e2
    EqE e1 e2         -> op "==" e1 e2
    OrE e1 e2         -> op "||" e1 e2
    AndE e1 e2        -> op "&&" e1 e2
    NotE e1           -> text "!" <> parens (prettyExpr e1)

  where
    op str e1 e2 = parens (prettyExpr e1 <+> text str <+> prettyExpr e2)

-- | Pretty print all table declarations.
prettyTableDecl :: CType                 -- ^ Table unit type
                -> Pipeline              -- ^ Programs
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
          $$ lbrace <> vcat (punctuate comma (map (prettyTableExpr (progOutBits prog) tbltype) tables))
          <> rbrace <> semi
      where
        tables    = M.elems $ progTables prog
        tableSize = foldl max 0 (map (length . tblTable) tables)

-- | Pretty print all buffer declarations.
prettyBufferDecls :: Pipeline -> Doc
prettyBufferDecls progs =
  vcat (map bufferDecl $ neededBuffers progs)
  where
    bufferDecl (BufferId n) = empty -- text "buffer_t" <+> text bufferPrefix <> int n <> semi

prettyConstantDecls :: CType -- ^ Buffer unit type
                    -> Pipeline
                    -> Doc
prettyConstantDecls buftype pipeline = case pipeline of
    Left progs -> vcat $ zipWith constantDecls progs [1..]
    Right progs -> vcat $ zipWith combine progs [1,3..]
  where
    constantDecls prog i = vcat $ map (constantDecl (progOutBits prog) i) $ M.toList $ progConstants prog
    combine (p,a) i = constantDecls p i $+$ constantDecls a (i+1)
    constantDecl outBits phase (ConstId n, deltas) =
      let chunks = splitAppends outBits buftype deltas
          constdocs = map (\(c, nbits) -> text $ num buftype nbits c) chunks
          comment = join $ map escape $ map chr deltas
          escape c = if isPrint c && isAscii c && isSafe c then [c] else "\\x" ++ showHex (ord c) ""
          isSafe c = c /= '\\'
      in vcat [ text "//" <+> text comment
              , text "const" <+> text "buffer_unit_t" <+> text constPrefix
                <> int phase <> text "_" <> int n
                <> brackets (int $ length chunks)
                <+> text "=" <+>
                (braces $ hcat $ punctuate comma constdocs) <> semi
             ]

neededBuffers :: Pipeline -> [BufferId]
neededBuffers pipeline = case pipeline of
        Left  progs -> deduplicate $ concatMap progBuffers progs
        Right progs -> deduplicate $ concatMap combine progs
    where
        combine (p,a) = progBuffers p ++ progBuffers a
        deduplicate = S.toList . S.fromList

neededNonStreamBuffers :: Pipeline -> [BufferId]
neededNonStreamBuffers pipeline = S.toList . S.unions $ case pipeline of
    Left  progs -> map needed progs
    Right progs -> map (\(p,a) -> S.union (needed p) (needed a)) progs
  where
    needed prog = S.fromList $ filter (/= progStreamBuffer prog) $ progBuffers prog


-- | Pretty print initialization code, which should allocate all needed buffers
prettyInit :: Pipeline -> Doc
prettyInit progs = text $ printf [strQ|
  transducer_state* tstate = malloc(sizeof(transducer_state));

  // Init regular buffers
  tstate->buffers = malloc(%d * sizeof(buffer_t *));
  for (int i = 1; i < %d; i++) {
    tstate->buffers[i] = init_buffer(INITIAL_BUFFER_SIZE);
  }

  // Init in/out buffers

  if (input) {
    tstate->outbuf = init_buffer(input_size);
    tstate->buffers[0] = tstate->outbuf;
    tstate->inbuf  = init_input_buffer(input_size);
    memcpy(tstate->inbuf->data, input, input_size);
    tstate->inbuf->length = input_size;
  }

  return tstate;
|] ((length buffers) + 1) ((length buffers) + 1)
  where
    buffers = neededNonStreamBuffers progs

prettyProg :: Pipeline -> CType -> CType -> Program -> Int -> Doc
prettyProg pipeline buftype tbltype prog phase =
     text "int state = 0;"
  $$ text "long left = tstate->inbuf->length;"
  $$ text "if (start_state < 0) goto" <+> blck (progInitBlock prog) phase <> semi
  $$ prettyJumptable pipeline
  $$ vcat (map pblock (M.toList $ progBlocks prog))
  where
    pblock (blid, is) =
      blck blid phase <>  char ':' $+$
                      text "state = " <> stateNr blid <> semi
                      $$ prettyBlock buftype tbltype prog is phase

prettyJumptable :: Pipeline -> Doc
prettyJumptable progs =
  vcat [ text "switch" <+> parens (text "phase") <+> lbrace
       , nest 2 $ vcat (map switchPhase [1..pipelineLength])
       , rbrace
       ]
  where
    pipelineLength = case progs of
      Left xs  -> length xs
      Right xs -> length xs
    blockids = listBlockIds progs
    switchPhase phase =
         text "case" <+> int phase <> colon
      $$ nest 2 (text "switch" <+> parens (text "start_state") <+> lbrace)
      $$ nest 4 (vcat (map (switchState phase) (blockids !! (phase-1))))
      $$ nest 4 defaultCase
      $$ nest 2 rbrace
    switchState phase state =
         text "case" <+> (int $ getBlockId state) <> colon
      $$ nest 2 (text "goto" <+> blck state phase <> semi)
    defaultCase =
         text "default:"
      -- $$ nest 2 (text "fprintf(stdout, \"-p %i -s %i\", phase, -1)" <> semi)
      $$ nest 2 (text "return -1" <> semi)

prettyStateTable :: Program -> Doc
prettyStateTable prog =
  -- doubleQuotes (comma <> lbrack)
  -- $+$
  (vcat $ punctuate comma (map pblock (M.toList $ progBlocks prog)))
  -- $+$ doubleQuotes rbrack
  where
    pblock (blid, is) =
      stateRes is (stateNr blid)

-- | Pretty print a list of instructions.
-- | relies on check for eof is the appeas firs in each state.
stateRes :: Block -> Doc -> Doc
stateRes instrs state =
  braces (text ".num = " <+> state <> comma <+> go instrs)
  where
    go [] = empty
    go (instr:is) = case instr of
        AcceptI       -> text ".accepting = 1"
        FailI         -> text ".accepting = 0"
        NextI _ _ is' -> go is'
        _             -> go is

listBlockIds :: Pipeline -> [[BlockId]]
listBlockIds pipeline = case pipeline of
    Left  progs -> map (M.keys . progBlocks) progs
    Right progs -> map (M.keys . progBlocks) . unpack $ progs
  where
    unpack [] = []
    unpack ((a,b):xs) = [a,b] ++ unpack xs

programsToC :: CType -> Pipeline -> CProg
programsToC buftype pipeline =
  CProg
  { cTables         = prettyTableDecl tbltype pipeline
  , cDeclarations   = prettyBufferDecls pipeline
                      $+$ prettyConstantDecls buftype pipeline
  , cInit           = prettyInit pipeline
  , cProg           = map (nest 2) prettyProgs
  , cBufferUnit     = ctyp buftype
  , cJumptable      = prettyJumptable pipeline
  , cStateTableVar  = stateTableVar
  , cStateCountVar  = stateCountVar
  }
  where
    tbltype = UInt8T
    prettyProgs = case pipeline of
                    Left progs  -> zipWith (prettyProg pipeline buftype tbltype) progs [1..]
                    Right progs -> concat $ zipWith combine progs [1,3..]
    combine (p,a) i = [prettyProg pipeline buftype tbltype p i, prettyProg pipeline buftype tbltype a (i+1)]
    stateTableVar = case pipeline of
                    Left progs  ->  text "state state_table[] =" <+> lbrace
                                    -- <+> doubleQuotes (lbrack <> brackets empty)
                                    $+$ nest 2 (vcat $ map prettyStateTable progs)
                                    $+$ rbrace <> semi
                    Right _ -> error "tjoooo..."
    stateCountVar = case pipeline of
                    Left (p:_) -> text "int state_count =" <+> int (length (progBlocks p)) <> semi
                    Left _     -> empty -- multi phase not supported
                    Right _    -> error "tjoooo..."

renderCProg :: String -> CProg -> String
renderCProg compInfo cprog =
  progTemplate (render $ cBufferUnit cprog)
               (render $ cTables cprog)
               (render $ cDeclarations cprog)
               compInfo
               (render $ cInit cprog)
               (render $ cStateTableVar cprog)
               (render $ cStateCountVar cprog)
               (map render $ cProg cprog)

ccVersion :: (MonadIO m) => FilePath -> m String
ccVersion comp = do
  -- gcc/clang prints version info on stderr
  (_, _, err, _) <- liftIO $ createProcess (proc comp ["-v"]) { std_err = CreatePipe }
  let hErr = maybe (error "ccVersion: bogus handle") id err
  errStr <- liftIO $ hGetContents hErr
  return $ intercalate "\\n" $ lines $ errStr

compileProgram :: (MonadIO m)
               => CType            -- ^ Buffer unit type
               -> Int              -- ^ CC Optimization level
               -> (String -> m ()) -- ^ Info message callback
               -> Pipeline         -- ^ Pipeline of programs to compile
               -> Maybe String     -- ^ Optional descriptor to put in program.
               -> FilePath         -- ^ Path to C compiler
               -> Maybe FilePath   -- ^ Binary output path
               -> Maybe FilePath   -- ^ C code output path
               -> Bool             -- ^ Use word alignment
               -> m ExitCode
compileProgram buftype optLevel infoCallback pipeline desc comp moutPath cCodeOutPath wordAlign = do
  cver <- ccVersion comp
  let info = (maybe noOutInfo (outInfo cver) moutPath)
  let cprog = programsToC buftype $ pipeline
  let cstr = renderCProg info cprog
  let sourceLineCount = length (lines cstr)
  case cCodeOutPath of
    Nothing -> return ()
    Just p  -> do
      infoCallback $ "Writing C source to " ++ p
      liftIO $ writeFile p cstr
  case moutPath of
    Nothing -> return ExitSuccess
    Just outPath -> do
      infoCallback $ "Generated " ++ show sourceLineCount ++ " lines of C code."
      infoCallback $ "Running compiler cmd: '" ++ intercalate " " (comp : compilerOpts outPath) ++ "'"
      liftIO $ do
        (Just hin, _, _, hproc) <- liftIO $ createProcess (proc comp (compilerOpts outPath))
                                                          { std_in = CreatePipe }
        hPutStrLn hin cstr
        hClose hin
        waitForProcess hproc
  where
    compilerOpts binPath = [ "-O" ++ show optLevel, "-xc"
                           , "-o", binPath] ++
                           (if isInfixOf "clang" comp
                           then ["-Wno-tautological-constant-out-of-range-compare"]
                           else [])
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
