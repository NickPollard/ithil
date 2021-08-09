{-# language OverloadedStrings #-}

module Parser.Tokenizer (
  Lexon(..),
  tokenize
) where

import Control.Applicative ((<|>), many, some)
--import Data.Bifunctor (first)
import Data.Foldable (asum)
import Data.Text (pack)
import qualified Text.Megaparsec as P
--import Text.Megaparsec (try)
import Text.Megaparsec.Char (char, digitChar, letterChar, string)

import Parser.Errors (Err)
import Parser.Types (Lexon(..), TokenStream(..))

{-
  TODO

  Tokenizer should probably export a single-use tokenizer function
  e.g.
    tokenize :: String -> [Lexon]
    * Can this fail? What does it mean to fail lexing?
      If so, do we continue and try to parse at all?

  We should also define token somewhere under the name `Token`. This could
  clash with 'Token' as used in MegaParsec's `class TokenStream ..` so need
  to define somewhere it can then be imported qualified.

  Perhaps:
    Parser.Tokenizer.Types (Token)
    Parser.Tokenizer (.. define TokenStream impl ..
    * Oh wait - that breaks the Orphan rule!
    Or define w/ Tokenstream impl, but then alias and re-export somewhere?

    Parser.Tokenizer { type Token = Parser.Tokenizer.Types.Token' }
-}

-- Token Parsers produce streams of `Parser.Types.Token`
type Parser = P.Parsec Err String

tokenize :: String -> TokenStream
-- TODO: BAD
tokenize src = TokenStream . maybe [] id $ P.parseMaybe tokenizer src

tokenizer :: Parser [Lexon]
tokenizer = many token

token :: Parser Lexon
token = ident <|> reserved <|> literal <|> operator

ident :: Parser Lexon
ident = Identifier . pack <$> some letterChar

literal :: Parser Lexon
literal = Literal . read <$> some digitChar

reserved :: Parser Lexon
reserved = Reserved . pack <$> asum (string <$> reserved_words)
  where reserved_words = [ "where", "case", "of", "let" ]

operator :: Parser Lexon
operator = Operator . pack <$> ((return <$> chars) <|> strings)
  where chars :: Parser Char
        chars = asum $ char <$> operatorChars
        strings = asum $ string <$> operatorStrings

operatorChars :: [Char]
operatorChars = "\\*+=()"

operatorStrings :: [String]
operatorStrings = [ "->" ]
