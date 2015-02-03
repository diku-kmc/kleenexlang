module KMC.Program.Backends.C where

import KMC.Program.Program
import KMC.List (replace)

class Backend b where
    build :: Prog -> b
    write :: b -> String

newtype CCode = CCode { unCCode :: [String] }

instance Backend CCode where
    build            = progToC
    write (CCode ls) = unlines ls

instance Monoid CCode where
    mempty = CCode []
    (CCode p1) `mappend` (CCode p2) = CCode (p1 ++ p2)

mconcatMap :: (Monoid b) => (a -> b) -> [a] -> b
mconcatMap f = mconcat . map

mkCLine :: String -> CCode
mkCLine s = CCode [s]

indentCBlock :: Int -> CCode -> CCode
indentCBlock n (CCode ls) = CCode (map (replicate n ' ' ++) ls)

progToC :: Prog -> CCode
progToC p =
  mkCLine ("  goto " ++ blck (progInit p) ++ ";") 
  <> mconcat (map showBlock $ M.toList $ progBlocks p)

  where
    blck x = "s" ++ show x
    buf x = "&b" ++ show x
    cls c = "SHIFT_TO_BUFFER_UNIT(cls[" ++ show c ++ "][next])"
    clsln c = show $ fst (progClasses p !! c)

    showBlock (bid, ps) = mkCLine ("  " ++ blck bid ++ ":")
                       <> indentCBlock 4 $ mconcat (map showInstr ps)

    conds rs = intercalate " || "
               $ map (\(w1, w2) ->
                       if w1 == w2 then
                         "(next == " ++ show w1 ++ ")"
                       else
                         "(" ++ show w1 ++ " <= next && next <= " ++ show w2 ++ ")") rs

    showInst :: PInstr -> CCode
    showInstr (PNext Nothing) = mkCLine "if (!readnext()) goto fail;"
    showInstr (PNext (Just bid)) = mkCLine $ "if (!readnext()) goto " ++ blck bid ++ ";"
    showInstr (PResetBuf b) = mkCLine $ "reset(" ++ buf b ++ ");"
    showInstr (PAppendConst b w n) = mkCLine $ "append(" ++ buf b ++ ", " ++ "0x" ++ showHex w "" ++ ", " ++ show n ++ ");"
    showInstr (PAppendCode b c) = mkCLine $ "append(" ++ buf b ++ ", " ++ cls c ++ ", " ++ clsln c ++ ");"
    showInstr (PAppendBuf b1 b2) = mkCLine $ "concat(" ++ buf b1 ++ ", " ++ buf b2 ++ ");"
    showInstr (PWriteBuf b) = mkCLine $ "write(" ++ buf b ++ ");"
    showInstr (PWriteCode c) = mkCLine $ "writeconst(" ++ cls c ++ ", " ++ clsln c ++ ");"
    showInstr (PWriteConst w n) = mkCLine $ "writeconst(0x" ++ showHex w "" ++ ", " ++ show n ++ ");"
    showInstr (PCJump rs bid) | not (null rs) = mkCLine $ 
      "if (" ++ conds rs ++ ") goto " ++ blck bid ++ ";"
    showInstr (PJump bid) = mkCLine $ "goto " ++ blck bid ++ ";"
    showInstr PFail = mkCLine $ "goto fail;"
    showInstr i = error $ "Cannot emit code for " ++ show i

progBufferDecls :: Prog -> CCode
progBufferDecls p = mconcatMap bufferDecl (progBuffers p)
    where
      bufferDecl b = mkCLine $ "buffer_t b" ++ show b ++ ";"

progBufferInits :: Prog -> CCode
progBufferInits p = mconcatMap bufferInit (progBuffers p)
    where
      bufferInit b = mkCLine $ "  init_buffer(&b" ++ show b ++ ");"

progTables :: Prog -> CCode
progTables p = if null (progClasses p) then mempty else clsTable
  where
    clsTable = mkCLine $ "uint8_t cls[" ++ show (length $ progClasses p) ++ "][256] =\n{ "
               ++ intercalate "\n\n, " (map showClassTable (progClasses p))
               ++ "};"

    showClassTable (_, m) = "{ " ++ intercalate "\n  , " (tabulate (enum m)) ++ " }"

    enum tbl = [ maybe 0 id (M.lookup c tbl) | c <- [0..255] ]

    pad s = replicate (3 - length s) ' ' ++ s

    tabulate [] = []
    tabulate xs = intercalate ", " (map (pad . show) $ take 8 xs):tabulate (drop 8 xs)

progSummary :: Prog -> String -> CCode
progSummary p re = mconcatMap mkCLine $ map ((++) "// ") [regex, numBuffers, numBlocks]
  where
    regex      = "Regex: " ++ re
    numBuffers = "Number of buffers: " ++ show (length $ progBuffers p)
    numBlocks  = "Number of blocks: " ++ show (length $ M.keys $ progBlocks p)


compile :: NFA -> Prog
compile nfa =
    -- TODO: Read in runtime-environment in inp.
    let p = sstToProg $ sstConstruct nfa
    let out =   replace "%%DECLS" (progBufferDecls p)
              $ replace "%%TABLES" (progTables p)
              $ replace "%%PROG" (progToC p)
              $ replace "%%INIT" (progBufferInits p)
              $ replace "%%SUMMARY" (progSummary p re)
              $ inp
       putStrLn out
       hClose c_h
    "run" -> do
      let sst = sstConstruct $ unsafeConstructNFA re
      inp <- getContents
      putStrLn . show $ run sst inp
    _ -> do
      putStrLn $ "Unknown flag: " ++ flag ++ ". Must be one of 'gen', 'run'."
      exitWith $ ExitFailure 1
