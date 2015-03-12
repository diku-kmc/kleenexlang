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


-- name: Kleenex : HAskell Stream Editor

-- Grammar:
-- parser ::= . | (name ":=" expr) parser
-- expr   ::= "C" expr
--          | <E>
--          | ~E // ignore result of E
--          | x
--          | expr expr
--          | expr "|" expr

-- | An Identifier is a String that always starts with a lower-case char.
newtype Identifier = Identifier String deriving (Eq, Ord)

mkIdent :: String -> Identifier
mkIdent str =
    case runParser kleenexIdentifier hpInitState "" str of
      Left e  -> error (show e)
      Right i -> i
fromIdent :: Identifier -> String
fromIdent (Identifier s) = s

-- | A Kleenex program is a list of assignments.
data Kleenex            = Kleenex [KleenexAssignment] deriving (Eq, Ord)

-- | Assigns the term to the name.
data KleenexAssignment  = HA (Identifier, KleenexTerm) deriving (Eq, Ord)

-- | The terms describe how regexps are mapped to strings.
data KleenexTerm = Constant ByteString -- ^ A constant output.
               | RE Regex
               | Var Identifier
               | Seq KleenexTerm KleenexTerm
               | Sum KleenexTerm KleenexTerm
               | Ignore KleenexTerm -- ^ Suppress any output from the subterm.
               | One
  deriving (Eq, Ord)

-- Some more friendly Show instances.
instance Show Kleenex where
    show (Kleenex l) = "Kleenex: {" ++ (concat $ map ("\n  " ++) (map show l)) ++ "\n}"
instance Show KleenexAssignment where
    show (HA (ident, term)) = show ident ++ " ::== " ++ show term
instance Show Identifier where
    show = fromIdent
instance Show KleenexTerm where
    show One = "1"
    show (Sum l r) = "(" ++ show l ++ ") | (" ++ show r ++ ")"
    show (Seq l r) = show l ++ " " ++ show r
    show (Var i)   = show i
    show (RE re) = "<" ++ unparse re ++ ">"
    show (Constant s) = show s
    show (Ignore e) = "drop:[" ++ show e ++ "]"

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

parens :: KleenexParser a -> KleenexParser a
parens = between (char '(') (char ')')

spaceAround :: KleenexParser a -> KleenexParser a
spaceAround = between (many spaceOrTab) (many spaceOrTab)

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
kleenexBecomesToken = spaceAround (string ":=" >> return ())

kleenexAssignment :: KleenexParser KleenexAssignment
kleenexAssignment = do
  skipComments
  ident <- kleenexIdentifier
  kleenexBecomesToken
  term <- kleenexTerm
  return $ HA (ident, term)

foldr1ifEmpty :: (a -> a -> a) -> a -> [a] -> a
foldr1ifEmpty _ e [] = e
foldr1ifEmpty f _ l  = foldr1 f l

skipComments :: KleenexParser ()
skipComments = ignore $ spaces >> skipComment `sepEndBy` spaces

skipComment :: KleenexParser ()
skipComment = ignore $ try (char '/' >> (singleLine <|> multiLine))
    where
      singleLine = char '/' >> manyTill anyChar newline       >> return ()
      multiLine  = char '*' >> manyTill anyChar (string "*/") >> return ()
              

kleenex :: KleenexParser Kleenex
kleenex = Kleenex <$> kleenexAssignment `sepEndBy` spaces

kleenexTerm :: KleenexParser KleenexTerm
kleenexTerm = buildExpressionParser table (spaceAround kleenexPrimTerm)
    where
      table = [ [Infix (char '|' >> return Sum) AssocRight] ]

indentedNewline :: KleenexParser ()
indentedNewline = ignore $ (many1 newline) >> (many1 spaceOrTab)

start :: KleenexParser ()
start = optional (i <|> c) <?> "start"
    where
      i = do
        many1 newline
        choice [many1 spaceOrTab, skipComment `sepEndBy` (many spaceOrTab)]
        return ()
      c = ignore $ skipComment `sepEndBy` (many spaceOrTab)

kleenexPrimTerm :: KleenexParser KleenexTerm
kleenexPrimTerm = start
                >> elms `sepEndBy` sep
                >>= return . foldr1ifEmpty Seq One
    where
      elms = choice [re, identifier, constant, ignored, parens kleenexTerm]
      sep = many $ spaceOrTab 
                   <|> try indentedNewline 
                   <|>  ignore (skipComment >> many spaceOrTab)
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
