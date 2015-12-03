{-# LANGUAGE FlexibleContexts #-}
module KMC.Kleenex.Parser(Prog,parseKleenex, parseKleenexFromFile) where

import           Control.Monad.Identity (Identity)
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.Char
import qualified KMC.Kleenex.Syntax as S
import           KMC.Kleenex.Syntax hiding (Term,Decl,Prog)
import           KMC.Syntax.Config (fancyRegexParser, RegexParserConfig(..))
import           KMC.Syntax.Parser (anchoredRegexP)
import           Numeric (readHex)
import           Text.Parsec
import           Text.ParserCombinators.Parsec.Expr (Assoc(..), buildExpressionParser, Operator(..))

type Parser = ParsecT [Char] () Identity

----------------------
-- Lexing
----------------------

-- | Discard result of a parser
skip :: Parser a -> Parser ()
skip p = p *> pure ()

-- | Skip whitespace, including singleline and multiline comments
whiteSpace :: Parser ()
whiteSpace = skipMany (simpleSpace <|> singleLineComment <|> multiLineComment)
  where
    singleLineComment = skip (try (string "//") *> manyTill anyChar (skip newline <|> eof))
    multiLineComment = skip (try (string "/*") *> manyTill anyChar (try (string "*/")))
    simpleSpace = skipMany1 (satisfy isSpace) <?> ""

-- | A lexeme is a token followed by white space
lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

-- | Either parse a complete symbol, or fail and consume nothing. Consumes trailing whitespace.
symbol :: String -> Parser ()
symbol = lexeme . try . skip . string

-- | Surround parser by parentheses with interleaved white space
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Surround parser by brackets with interleaved white space
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | An identifier is one or more alphanumeric symbols, first symbol cannot be a number
identifier :: Parser String
identifier = (:) <$> letter <*> many (alphaNum <|> oneOf "_-")

-- | A positive integer literal
integer :: Parser Int
integer = read <$> many1 digit <?> "positive integer"

-- | A register identifier must begin with lowercase
regIdentifier :: Parser String
regIdentifier = (:) <$> lower <*> many (alphaNum <|> oneOf "_-")

-- | Mapping of escape codes to their underlying character
escapeCodes :: [(Char, Char)]
escapeCodes = [('\\', '\\')
              ,('"', '"')
              ,('n', '\n')
              ,('t', '\t')
              ,('v', '\v')
              ,('r', '\r')
              ,('f', '\f')]

-- | A single escape sequence
escapedChar :: Parser Char
escapedChar = char '\\' *> try (simpleEscape <|> hexcodeEscape <?> "escape sequence")
    where
      simpleEscape =
        choice [ replacement <$ char code | (code, replacement) <- escapeCodes ]
      hexcodeEscape = decodeHex <$> (char 'x' *> count 2 hexDigit)
      decodeHex = chr . fst . head . readHex

-- | A (possibly empty) sequence of verbatim symbols
stringConstant :: Parser String
stringConstant = many (noneOf ['"', '\\'] <|> escapedChar)

------------------
-- Parsing
------------------

-- Specialize the metadata of the AST to a pair of source positions
-- denoting the start and end of a given construct in the source text.
type Term = S.Term (SourcePos, SourcePos)
type Decl = S.Decl (SourcePos, SourcePos)
type Prog = S.Prog (SourcePos, SourcePos)

---------------------
---- Helper functions
---------------------

-- | Parse a value which may depend on its position in the source text.
pos :: Parser ((SourcePos, SourcePos) -> a) -> Parser a
pos p = do
  pos1 <- getPosition
  f <- p
  pos2 <- getPosition
  return $ f (pos1, pos2)

-- | Wrap position metadata around the result of a term parser
termPos :: Parser Term -> Parser Term
termPos p = pos (p >>= return . flip TermInfo)

-- | Like termPos, but for a Term-valued function.
termPos1 :: Parser (a -> Term) -> Parser (a -> Term)
termPos1 p = pos (p >>= \f -> return (\i x -> TermInfo i (f x)))

-- | Like termPos1, but for a function with two arguments.
termPos2 :: Parser (a -> b -> Term) -> Parser (a -> b -> Term)
termPos2 p = pos (p >>= \f -> return (\i x y -> TermInfo i (f x y)))

-- | Like termPos, but for declarations
declPos :: Parser Decl -> Parser Decl
declPos p = pos (p >>= return . flip DeclInfo)

--------------------
---- Kleenex parsers
--------------------
-- Parsers ending in "P" consume trailing white space

-- | Parse an identifier
identifierP :: Parser Ident
identifierP = Ident <$> lexeme identifier
              <?> "nonterminal"

-- | Parse a register identifier
regIdentifierP :: Parser RegIdent
regIdentifierP = RegIdent <$> lexeme regIdentifier
              <?> "register"

-- | Parse a constant
constantP :: Parser ByteString
constantP = pack <$> lexeme (between (char '"') (char '"') stringConstant)
            <?> "string constant"

-- | Parse a term
termP :: Parser Term
termP = buildExpressionParser table atomP
  where
    table =
      [ [ Prefix (SuppressOutput <$ symbol "~")
          -- backtracking lookahead necessary since register identifiers overlap with nonterminals
        , Prefix (RedirectReg <$> try (RegIdent <$> regIdentifier <* char '@'))
        ]
      , [ postfix
            (termPos1 (choice [ Star     <$ symbol "*"
                              , Question <$ symbol "?"
                              , Plus     <$ symbol "+"
                              , rangeP
                              ] <?> "repetition operator"))
        ]
      , [ Infix (termPos2 $ Seq <$ notFollowedBy (char '|')) AssocRight ]
      , [ Infix (termPos2 $ Sum <$ symbol "|") AssocRight ]
      ]

    -- {m,n} or {m,} or {,n} or {m}
    rangeP = char '{'
             *> choice [ try (Range <$> optionMaybe integer <* char ',' <*> optionMaybe integer)
                       , (\n -> Range (Just n) (Just n)) <$> integer ]
             <* symbol "}"

    -- Treat sequence of postfix operators as single operator. E.g. /a/*+?
    postfix p = Postfix $ chainl1 p (pure (flip (.)))

-- | Parse a term "atom" - that is, a term that does not require parsing term operators.
atomP :: Parser Term
atomP = termPos $ choice
        [ One      <$  symbol "1"
          -- Identifiers overlap with register names and declarations, so we need lookahead.
        , Var      <$> try (identifierP <* notFollowedBy (string ":=" <|> string "@"))
        , Constant <$> constantP
          -- Trailing slash is parsed as a "symbol" to consume white space
        , RE       <$> between (char '/') (symbol "/") regexP
        , WriteReg <$  char '!' <*> regIdentifierP
        , brackets registerUpdateP
        , parens termP
        ]
  where
    regexP = snd <$> (anchoredRegexP $ fancyRegexParser { rep_illegal_chars = "/"
                                                        , rep_freespacing = False })
    registerUpdateP = do
      ident <- regIdentifierP
      choice [ UpdateReg ident <$ symbol "<-" <*> updateAtomsP
             , UpdateReg ident <$ symbol "+=" <*> ((Left ident:) <$> updateAtomsP)
             ]
    updateAtomsP = many1 ((Left <$> regIdentifierP) <|> (Right <$> constantP))

progP :: Parser Prog
progP = Kleenex <$ whiteSpace <*> pipelineP <*> many1 declP
  where
    declP     = declPos (S.Decl <$> identifierP <* symbol ":=" <*> termP)
    pipelineP = try (symbol "start:") *> identifierP `sepBy1` (symbol ">>")
                <|> pure [Ident "main"]


------------
-- Interface
------------

-- | Parse a string as a Kleenex program
parseKleenex :: String -> Either ParseError Prog
parseKleenex = runParser (progP <* eof) () "<input string>"

-- | Parse a file as a Kleenex program
parseKleenexFromFile :: FilePath -> IO (Either ParseError Prog)
parseKleenexFromFile fp =
  readFile fp >>= return . runParser (progP <* eof) () fp
