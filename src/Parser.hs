{-# LANGUAGE OverloadedStrings #-}

module Parser (
  parseModule,
) where

import Control.Applicative ((<|>), many, some)
import Data.Bifunctor (first)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Text.Megaparsec as P
import Text.Megaparsec (try, label, token, single)
import Text.Megaparsec.Char (char, digitChar, letterChar, space, space1)

import Lang.Types (Binding(..), Expr(..), Ident(..))
import Parser.Errors (Err, fmtParseError)
import Parser.Types (Lexon(..), TokenStream)
import qualified Parser.Tokenizer as Tokenizer

-- | A Module is a list of bindings
-- ```
--   x = 2
--
--   add2 = \x -> x + 2
parseModule :: TokenStream -> Either Text [Binding Expr]
parseModule src = first fmtParseError $ P.parse (many binding) "<no file>" src

-- We parse into our AST from a tokenized stream
type Parser = P.Parsec Err TokenStream

binding :: Parser (Binding Expr)
binding = Binding <$> ident <*> (single (Tokenizer.Operator "=") *> expr)

expr :: Parser Expr
expr = try app
   <|> try lambda
   <|> try var
   <|> try lit

lit :: Parser Expr
--lit = Lit <$> int
  --where int = read <$> (some digitChar)
lit = label "integer literal" $ token (get_int) Set.empty
  where get_int (Tokenizer.Literal i) = Just $ Lit i
        get_int _ = Nothing

var :: Parser Expr
var = label "variable identifier" $ Var <$> ident

--ident :: Parser Ident
--ident = Ident . pack <$> some letterChar

ident :: Parser Ident
ident = label "identifier" $ token (is_ident) Set.empty
  where is_ident (Tokenizer.Identifier name) = Just $ Ident name
        is_ident _ = Nothing

lambda :: Parser Expr
--lambda = Lambda <$> (char '\\' *> ident <* space <* (char '-' <* char '>' <* space)) <*> expr
lambda = Lambda <$>
  (single (Tokenizer.Operator "\\") *> ident <* single (Tokenizer.Operator "->")) <*> expr

app :: Parser Expr
--app = App <$> fn <* space1 <*> expr
  --where fn = var <|> parens expr
app = App <$> fn <*> expr
  where fn = var <|> parens expr

parens :: Parser a -> Parser a
--parens p = open *> p <* close
 --where open = char '(' *> space
       --close = space <* char ')'
parens p = open *> p <* close

open :: Parser Lexon
open = single $ Tokenizer.Operator "("

close :: Parser Lexon
close = single $ Tokenizer.Operator ")"
