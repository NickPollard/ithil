{-# language OverloadedStrings #-}

module Lang.Lexer (
  Lexeme(..),
  lex',
  lexMaybe,
  ppLexemes,
) where

import Control.Applicative ((<|>), many, some)
import Data.Foldable (asum)
import Data.Text (intercalate, pack, singleton, Text)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, string, space1)
import qualified Text.Megaparsec.Char.Lexer as L

-- TODO should LexerError just be defined in this file?
import Lang.Types (LexerError)

-- Lexemes that can be tokenized from the raw stream of characters of a text source file
data Lexeme = Identifier Text
            | Reserved Text
            | Operator Text
            | Literal Int
            deriving (Ord, Eq, Show)

ppLexemes :: [Lexeme] -> Text
ppLexemes = intercalate " " . fmap ppLexeme

ppLexeme :: Lexeme -> Text
ppLexeme (Identifier ident) = ident
ppLexeme (Reserved res) = res
ppLexeme (Operator op) = op
ppLexeme (Literal i) = pack $ show i

-- Declare Our parser type - they work on Strings, and produce LexerErrors
type Parser = MP.Parsec LexerError String

-- A valid stream is just a sequence of lexemes
lexer :: Parser [Lexeme]
lexer = many lexeme

lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme spaces'

-- SpaceConsumer
-- Comments are
--   --single line
-- or
--  {-
--     multi-line
--  -}
spaces' :: Parser ()
spaces' = L.space space' line' block'
  where space' = space1
        line' = L.skipLineComment "--"
        block' = L.skipBlockComment "{-" "-}"

-- valid lexemes are:
lexeme :: Parser Lexeme
lexeme = lexeme' (
  reserved       -- a reserved word
  <|> literal    -- a literal value
  <|> operator   -- a symbolic operator
  <|> identifier -- an identifier
                  )

-- A programer defined identifier
-- Identifiers take the form [A-Za-z][A-Za-z0-9_]*
identifier :: Parser Lexeme
identifier = Identifier . pack <$> ((:) <$> letterChar <*> many identChar)
  where identChar = alphaNumChar <|> char '_'

-- A literal value, sucha s 42, true or "hello world"
literal :: Parser Lexeme
literal = numLiteral
-- TODO: Allow non numerics

-- A numeric literal, such as 42 or 0
numLiteral :: Parser Lexeme
numLiteral = Literal . read <$> some digitChar <* MP.notFollowedBy letterChar

-- A reserved word in the Ithil language
reserved :: Parser Lexeme
reserved = Reserved . pack <$> asum (string <$> reservedWords)

reservedWords :: [String]
reservedWords =
  [
    "module",
    "where",
    "case",
    "of",
    "let",
    "in",
    "import",
    "qualified",
    "as",
    "type",
    "data",
    "class",
    "instance"
  ]

operator :: Parser Lexeme
operator = Operator <$> ((singleton <$> chars) <|> (pack <$> strings))
  where chars :: Parser Char
        chars = asum $ char <$> operatorChars
        strings = asum $ string <$> operatorStrings

operatorChars :: [Char]
operatorChars = "\\*+=()"

operatorStrings :: [String]
operatorStrings = [ "->", "::", "<-" ]

----
-- Methods to run the lexer
----

-- Run a lexical analysis pass on the input string
lex' :: String -> String -> Either (MP.ParseErrorBundle String LexerError) [Lexeme]
lex' src input = MP.parse lexer src input

-- Convenience method to run a lexical analysis pass on the input string and discard errors
lexMaybe :: String -> Maybe [Lexeme]
lexMaybe = MP.parseMaybe lexer
