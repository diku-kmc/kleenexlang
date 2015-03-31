{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module KMC.Kleenex.Parser where

import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Control.Monad.Identity (Identity)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Text.Parsec hiding (parseTest)
import Text.Parsec.Prim (runParser)
import Text.ParserCombinators.Parsec.Expr (Assoc(..), buildExpressionParser, Operator(..))

import KMC.Syntax.Config
import KMC.Syntax.External (Regex, unparse)
import KMC.Syntax.Parser (anchoredRegexP)

import Debug.Trace
    
-- | Change the type of a state in a parser.  
changeState :: forall m s u v a . (Functor m, Monad m)
            => (u -> v) -> (v -> u) -> ParsecT s u m a -> ParsecT s v m a
changeState forward backward = mkPT . transform . runParsecT
  where
    --mapState :: forall u v . (u -> v) -> State s u -> State s v
    mapState f st = st { stateUser = f (stateUser st) }
    --mapReply :: forall u v . (u -> v) -> Reply s u a -> Reply s v a
    mapReply f (Ok a st err) = Ok a (mapState f st) err
    mapReply _ (Error e) = Error e
    --
    fmap3 = fmap . fmap . fmap
    --transform :: (State s u -> m (Consumed (m (Reply s u a))))
    --          -> (State s v -> m (Consumed (m (Reply s v a))))
    transform p st = fmap3 (mapReply forward) (p (mapState backward st))

-- | An Identifier is a String that always starts with a lower-case char.
newtype Identifier = Identifier String deriving (Eq, Ord, Show)

mkIdent :: String -> Identifier
mkIdent str =
    case runParser kleenexIdentifier hpInitState "" str of
      Left e  -> error (show e)
      Right i -> i
fromIdent :: Identifier -> String
fromIdent (Identifier s) = s

-- | A Kleenex program is a list of assignments.
data Kleenex            = Kleenex [KleenexAssignment] deriving (Eq, Ord, Show)

-- | Assigns the term to the name.
data KleenexAssignment  = HA (Identifier, KleenexTerm) deriving (Eq, Ord, Show)

-- | The terms describe how regexps are mapped to strings.
data KleenexTerm = Constant ByteString -- ^ A constant output.
               | RE Regex
               | Var Identifier
               | Seq KleenexTerm KleenexTerm
               | Sum KleenexTerm KleenexTerm
               | Ignore KleenexTerm -- ^ Suppress any output from the subterm.
               | One
  deriving (Eq, Ord, Show)


type HPState = ()

hpInitState :: HPState
hpInitState = ()

type KleenexParser a = Parsec String HPState a


withHPState :: Parsec s () a -> Parsec s HPState a
withHPState p = getState >>= \hps -> changeState (const hps) (const ()) p

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
      escaped = char '\\' >> choice (map escapedChar cr)
      escapedChar (code, replacement) = replacement <$ char code
      cr = [('\\', '\\'), ('"', '"'), ('n', '\n'), ('t', '\t')]
               
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

foldr1ifEmpty :: (a -> a -> a) -> a -> [a] -> a
foldr1ifEmpty _ e [] = e
foldr1ifEmpty f _ l  = foldr1 f l

skipped :: KleenexParser ()
skipped = ignore $ many (choice [ws, comment])
    where
      ws = ignore $ many1 space
      comment = ignore $ try (char '/' >> (singleLine <|> multiLine))
      singleLine = char '/' >> manyTill anyChar (ignore newline <|> eof)
      multiLine  = char '*' >> manyTill anyChar (try $ string "*/")
              
kleenex :: KleenexParser Kleenex
kleenex = Kleenex <$> (skipped *> (kleenexAssignment `sepEndBy` skipped))

kleenexTerm :: KleenexParser KleenexTerm
kleenexTerm = buildExpressionParser table kleenexPrimTerm
    where
      table = [ [Infix (char '|' >> return Sum) AssocRight] ]

kleenexPrimTerm :: KleenexParser KleenexTerm
kleenexPrimTerm = skipped
                >> elms `sepEndBy` skipped
                >>= return . foldr1ifEmpty Seq One
    where
      elms = choice [re, identifier, constant, ignored, parens kleenexTerm]
      constant   = Constant . encodeString <$> kleenexConstant
                   <?> "Constant"
      re         = RE  <$> between (char '<') (char '>') regexP
                   <?> "RE"
      identifier = Var <$> try (kleenexIdentifier <* notFollowedBy kleenexBecomesToken)
                   <?> "Var"
      ignored    = Ignore <$> (char '~' *> elms)
                   <?> "Ignore"

encodeString :: String -> ByteString
encodeString = encodeUtf8 . T.pack

regexP :: KleenexParser Regex
regexP = snd <$> (withHPState $
                  anchoredRegexP $ fancyRegexParser { rep_illegal_chars = "!<>" })

firstName :: Kleenex -> Identifier
firstName (Kleenex (HA (i,_):_)) = i
firstName (Kleenex []) = error "firstName: no assignments"

parseKleenex :: String -- ^ Input string
             -> Either String (Identifier, Kleenex) 
parseKleenex str =
    case runParser (kleenex <* eof) hpInitState "" str of
      Left err -> Left (show err)
      Right h -> Right (firstName h, h)

parseKleenexFile :: FilePath -> IO (Either String (Identifier, Kleenex))
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
parseTest = stateParseTest hpInitState

parseTest' :: (Stream s Identity t)
               => Parsec s HPState Kleenex -> s -> IO Kleenex
parseTest' p input
    = case runParser p hpInitState "" input of
        Left err -> do putStr "parse error at "
                       print err
                       fail ""
        Right x  -> return x

pf = parseTest (kleenex <* eof)
pf' = parseTest' (kleenex <* eof)
