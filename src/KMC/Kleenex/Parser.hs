{-# LANGUAGE FlexibleContexts #-}
module KMC.Kleenex.Parser where

import           Control.Monad.Identity (Identity)
import           Data.Char
import           Data.ByteString (ByteString, unpack)
import           Data.Hashable
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Text.Parsec hiding (parseTest)
import           Text.ParserCombinators.Parsec.Expr (Assoc(..), buildExpressionParser, Operator(..))
import           Numeric (readHex)

import           KMC.Kleenex.Action
import           KMC.SymbolicSST (Atom(..), ActionExpr(..))
import           KMC.Syntax.Config
import           KMC.Syntax.External (Regex)
import           KMC.Syntax.Parser (anchoredRegexP)

-- | An Identifier is a String that always starts with a lower-case char.
newtype Identifier = Identifier { fromIdent :: String } deriving (Eq, Ord, Show)

-- | A Kleenex program is a list of assignments.
data Kleenex            = Kleenex [Identifier] [KleenexAssignment] deriving (Eq, Ord, Show)

-- | Assigns the term to the name.
data KleenexAssignment  = HA (Identifier, KleenexTerm)
    deriving (Eq, Ord, Show)

-- | The terms describe how regexps are mapped to strings.
data KleenexTerm = Constant ByteString -- ^ A constant output.
                 | RE Regex
                 | Var Identifier
                 | Seq KleenexTerm KleenexTerm
                 | Sum KleenexTerm KleenexTerm
                 | Star KleenexTerm
                 | Plus KleenexTerm
                 | Question KleenexTerm
                 | Range (Maybe Int) (Maybe Int) KleenexTerm
                 | Ignore KleenexTerm -- ^ Suppress any output from the subterm.
                 | Action KleenexAction KleenexTerm
                 | One
  deriving (Eq, Ord, Show)

type KleenexParser a = Parsec String () a

separator :: KleenexParser ()
separator = spaceOrTab <|> ignore (try (lookAhead newline))

-- Parse one space or tab character.
spaceOrTab :: KleenexParser ()
spaceOrTab = ignore (char ' ' <|> char '\t')

ignore :: Parsec s u a -> Parsec s u ()
ignore p = p >> return ()

skipAround :: KleenexParser a -> KleenexParser a
skipAround = between skipped skipped

parens :: KleenexParser a -> KleenexParser a
parens = between (char '(') (char ')')

-- | Identifiers are only allowed to start with lower-case characters.
kleenexIdentifier :: KleenexParser Identifier
kleenexIdentifier = Identifier <$>
                  ((:) <$> legalStartChar <*> many legalChar)
                  <?> "identifier"
    where
      legalStartChar = lower
      legalChar = upper <|> lower <|> digit <|> oneOf "_-"

-- | Parses a character or an escaped double quote.
escapedChar :: Parsec String s Char
escapedChar = satisfy (not . mustBeEscaped)
              <|> escaped
    where
      mustBeEscaped c = c `elem` map snd cr
      escaped = char '\\' >> (try $ choice (map aux cr) <|> hexcode)
      aux (code, replacement) = replacement <$ char code
      cr = [('\\', '\\'), ('"', '"'), ('n', '\n'), ('t', '\t')]
      hexcode = do _ <- char 'x'
                   x <- count 2 hexDigit
                   return . chr . fst . head . readHex $ x

-- | A "constant" is a string enclosed in quotes.
kleenexConstant :: KleenexParser String
kleenexConstant = (char '"') *> (many escapedChar) <* (char '"')
                <?> "string constant"

kleenexBecomesToken :: KleenexParser ()
kleenexBecomesToken = skipAround (string ":=" >> return ())

kleenexAssignment :: KleenexParser KleenexAssignment
kleenexAssignment = do
  ident <- kleenexIdentifier
  kleenexBecomesToken
  term <- kleenexTerm
  return $ HA (ident, term)

varToInt :: String -> Int
varToInt = abs . hash

skipped :: KleenexParser ()
skipped = ignore $ many skipped1

skipped1 :: KleenexParser ()
skipped1 = ignore $ many1 (choice [ws, comment])
    where ws = ignore $ many1 space

comment :: KleenexParser ()
comment = ignore $ try (char '/' >> (singleLine <|> multiLine))
    where
      singleLine = (try $ char '/') >> manyTill anyChar (ignore newline <|> eof)
      multiLine  = char '*' >> manyTill anyChar (try $ string "*/")

parsePipeline :: KleenexParser [Identifier]
parsePipeline = string "start:" *> skipped *> kleenexIdentifier `sepBy1` (try $ skipAround (string ">>"))

kleenex :: KleenexParser (Kleenex)
kleenex = do
    idents <- skipped *> (try parsePipeline <|> return [Identifier "main"])
    assignments <- skipped *> (kleenexAssignment `sepEndBy` skipped)
    return $ Kleenex idents assignments

kleenexTerm :: KleenexParser KleenexTerm
kleenexTerm = skipAround kleenexExpr
    where
      kleenexExpr = buildExpressionParser table $ skipAround (kleenexPrimTerm <|> parens kleenexTerm)
      schar = skipAround . char
      table = [
          [ Prefix (schar '~' >> return Ignore <?> "Ignored"),
            Prefix (try $ do ident <- many lower
                             _ <- char '@'
                             return $ (\term -> Action (PushOut (varToInt ident)) term `Seq` Action PopOut One)) ],
          -- Use the postfix function below to allow multiple stacked postfix
          -- operators without the need for parentheses around all subterms.
          [ postfix $ choice [ (schar '*' >> return Star <?> "Star")
                             , (schar '?' >> return Question <?> "Question")
                             , (schar '+' >> return Plus <?> "Plus")
                             , range <?> "Range"
                             ]
          ],
          [ Infix   (skipped >> notFollowedBy (char '|') >> return Seq) AssocRight
          ],
          [ Infix   (schar '|' >> return Sum) AssocRight
          ]
        ]

      braces = between (schar '{') (schar '}')
      number = fmap read (many1 digit) <?> "an integer literal"
      range = braces $ try (do n <- optionMaybe number
                               _ <- schar ','
                               m <- optionMaybe number
                               return $ Range n m)
                   <|> do n <- number
                          return $ Range (Just n) (Just n)

-- | Combine postfix operators and allow sequences (e.g., /a/*+?)
postfix :: KleenexParser (a -> a) -> Operator Char () a
postfix p = Postfix . chainl1 p $ return (flip (.))

kleenexPrimTerm :: KleenexParser KleenexTerm
kleenexPrimTerm = skipAround elms
    where
      elms = choice [one, re, identifier, constant, action, output]
      one        = const One <$> (char '1')
                   <?> "One"
      constant   = Constant . encodeString <$> kleenexConstant
                   <?> "Constant"
      re         = RE  <$> between (char '/') (char '/') regexP
                   <?> "RE"
      identifier = Var <$> try (kleenexIdentifier <* notFollowedBy kleenexBecomesToken)
                   <?> "Var"
      action     = Action <$> between (char '[') (char ']') actionP <*> (return One)
                   <?> "Action"
      output     = do ident <- skipAround (char '!' *> kleenexIdentifier)
                      let buf = fromIdent ident
                      return $ Action (RegUpdate 0 [VarA 0, VarA (varToInt buf)]) One
                   <?> "OutputTerm"

encodeString :: String -> ByteString
encodeString = encodeUtf8 . T.pack

regexP :: KleenexParser Regex
regexP = snd <$> (anchoredRegexP $ fancyRegexParser { rep_illegal_chars = "/", rep_freespacing = False })

actionP :: KleenexParser (KleenexAction)
actionP = do skipped
             ident <- kleenexIdentifier
             let reg = varToInt $ fromIdent ident
             try (overwrite reg) <|> concatAction reg
    where
        regs = VarA . varToInt . fromIdent <$> kleenexIdentifier
        constant = ConstA . unpack . encodeString <$> kleenexConstant
        overwrite reg = do
            _ <- skipAround $ string "<-"
            actions <- choice [regs, constant] `sepEndBy1` skipped
            return $ RegUpdate reg actions
        concatAction reg = do
            _ <- skipAround $ string "+="
            actions <- choice [regs, constant] `sepEndBy1` skipped
            return $ RegUpdate reg (VarA reg : actions)

parseKleenex :: String -- ^ Input string
             -> Either String Kleenex
parseKleenex str =
    case runParser (kleenex <* eof) () "" str of
      Left err -> Left (show err)
      Right h -> Right h

parseKleenexFile :: FilePath -> IO (Either String Kleenex)
parseKleenexFile fp = readFile fp >>= return . parseKleenex

-----------------------------------------------------------------
-----------------------------------------------------------------

stateParseTest :: (Stream s Identity t, Show a)
               => u -> Parsec s u a -> s -> IO ()
stateParseTest st p input
    = case runParser p st "" input of
        Left err -> do putStr "parse error at "
                       print err
        Right x  -> print x

parseTest :: (Show a) => KleenexParser a -> String -> IO ()
parseTest = stateParseTest ()

parseTest' :: (Stream s Identity t)
               => Parsec s () (Kleenex) -> s -> IO (Kleenex)
parseTest' p input
    = case runParser p () "" input of
        Left err -> do putStr "parse error at "
                       print err
                       fail ""
        Right x  -> return x
