{-# language OverloadedStrings #-}

module Parser.Tokenizer (
  tokenize
) where

import Control.Applicative ((<|>), many, some)
import Data.Bifunctor (first)
import Data.Foldable (asum)
import Data.Text (Text, pack)
import qualified Text.Megaparsec as P
import Text.Megaparsec (try)
import Text.Megaparsec.Char (char, digitChar, letterChar, space, space1, string)

import Parser.Errors (Err)

data Token = Identifier Text
           | Reserved Text
           | Operator Text

type Parser = P.Parsec Err String

tokenize :: Parser [Token]
tokenize = many token

token :: Parser Token
token = ident <|> reserved <|> operator

ident :: Parser Token
ident = Identifier . pack <$> some letterChar

reserved :: Parser Token
reserved = Reserved . pack <$> asum (string <$> reserved_words)
  where reserved_words = [ "where", "case", "of", "let" ]

operator :: Parser Token
operator = Operator . pack <$> ((return <$> chars) <|> strings)
  where chars :: Parser Char
        chars = asum $ char <$> operatorChars
        strings = asum $ string <$> operatorStrings

operatorChars :: [Char]
operatorChars = "\\*+="

operatorStrings :: [String]
operatorStrings = [ "->" ]
