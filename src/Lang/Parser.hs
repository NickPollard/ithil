{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}

module Lang.Parser where

import Control.Applicative ((<|>), some)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import qualified Data.Set as Set
import Data.Text (Text, pack, intercalate)
import qualified Data.Text as Text
import qualified Text.Megaparsec as MP
import Text.Megaparsec (try, label, token, single, many)
import Text.Megaparsec.Char (char, digitChar, letterChar, space, space1)

import Lang.Lexer (Lexeme(..), ppLexemes)
import Lang.Types (Ident(..), Expr(..), BinaryOp(..), Sugar(..))

data ParserErr = ParserErr deriving (Ord, Eq, Show)

-- We parse into our AST from a tokenized stream of lexemes (the result of the lexer)
type Parser = MP.Parsec ParserErr Lexemes

-- | A binding is a top-level binding of a language expression to an identifier
-- e.g.
--   x = 2
-- is a binding of (Lit 2) to the Ident "x"
-- TODO do we need this Binding class?
-- TODO should Module be different? Just use a hashmap? have more?
-- TODO move Expr datatype definition into this file
data Binding a = Binding { bindingName :: Ident
                         , bindingValue :: a
                         } deriving (Eq, Show)

{-
-- | An AST Expression
data Expr = Lambda Ident Expr
          | App Expr Expr
          | Var Ident
          | Lit Int
          | BuiltIn BinaryOp
          -}

-- Each file we parse should produce a named module containing a list of bindings
data Module = Module { moduleName :: String
                     , moduleBindings :: [Binding Expr]
                     }
                     deriving (Eq, Show)

-- The input type for our parser is a stream of Lexemes
data Lexemes = Lexemes [Lexeme]

-- Implement the `Stream` typeclass from Megaparsec
-- This is to allow lexing first, and then parsing a stream of lexemes
-- in a second, distinct pass
instance MP.Stream Lexemes where
  type Token Lexemes = Lexeme
  type Tokens Lexemes = [Lexeme]
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  take1_ (Lexemes (x:xs)) = Just (x, Lexemes xs)
  take1_ (Lexemes []) = Nothing
  takeN_ n (Lexemes s)
    | n <= 0    = Just ([], Lexemes s)
    | otherwise = Just ((take n s), Lexemes (drop n s))
  takeWhile_ p (Lexemes s) =
    let taken = takeWhile p s
     in (taken, Lexemes (drop (length taken) s))


parseModuleMaybe :: Lexemes -> Maybe Module
parseModuleMaybe = MP.parseMaybe module'

parseModule :: Lexemes -> Either Text Module
parseModule src = first fmtParseError $ MP.parse module' "<no file>" src

-- | A Module is a list of bindings
-- ```
--   module Foo where
--
--   x :: Int
--   x = 2
--
--   add2 = \x -> x + 2
-- ```
module' :: Parser Module
module' = Module <$> moduleName' <*> many binding

-- A module name: 'module <NAME> where'
moduleName' :: Parser String
moduleName' = return "<anonymous>" -- TODO parse module names

-- A top level binding
--
-- `f = expr`
binding :: Parser (Binding Expr)
binding = Binding <$> ident <* equals' <*> expr
  where equals' = single $ Operator "="

-- TODO fix associativity, esp. wrt. App
expr :: Parser Expr
expr = try infix' <|> subterm

subterm :: Parser Expr
subterm = try app' -- app' is not left-recursive, expects var or parens
  <|> try lambda  -- lambda is not left-recursive, consumes \
  <|> trivial -- trivial

trivial :: Parser Expr
trivial = try var
    <|> try builtin
    <|> try lit

lit :: Parser Expr
lit = label "integer literal" $ token literalInteger Set.empty
  where literalInteger (Literal i) = Just $ Lit i
        literalInteger _           = Nothing

var :: Parser Expr
var = label "variable identifier" $ Var <$> ident

ident :: Parser Ident
ident = label "identifier" $ token identifier Set.empty
  where identifier (Identifier name) = Just $ Ident name
        identifier _                 = Nothing

lambda :: Parser Expr
lambda = Lambda <$>
  -- \ident -> expr
  (slash' *> ident <* arrow') <*> expr
    where slash' = single $ Operator "\\"
          arrow' = single $ Operator "->"

--app :: Parser Expr
--app = App <$> fn <*> expr
  --where fn = var <|> parens expr

infix' :: Parser Expr
infix' = go <$> subterm <*> infixOp <*> expr
  where infixOp = builtin
        go a op b = App (App op a) b

app' :: Parser Expr
app' = go <$> fn <*> some operand
  where go f (o:os) = foldl App (App f o) os
        go f [] = error "Should be at least one operand parsed" -- impossible?
        fn = var <|> parens expr
        operand = var <|> lit <|> parens expr

builtin :: Parser Expr
builtin = BuiltIn <$> (add' <|> mul')
  where add' = Add <$ single (Operator "+")
        mul' = Mul <$ single (Operator "*")
        -- TODO: add (Div, Sub) operators

parens :: Parser a -> Parser a
parens p = open *> p <* close

open :: Parser Lexeme
open = single $ Operator "("

close :: Parser Lexeme
close = single $ Operator ")"

--
-- Desugaring from front-end language to Core
--

desugar :: Sugar -> Expr
-- let `var` = `value` in `expr`
desugar (Let var value expr) = App (Lambda var expr) value
-- where `var` = `expr`
-- -- TODO - how to express `where`?
--  Probably this is just a list of additional scoped-bindings to an expr
desugar (Where var expr) = error "NYI"

--
-- Error Formatting
--

fmtParseError :: MP.ParseErrorBundle Lexemes ParserErr -> Text
fmtParseError (MP.ParseErrorBundle errors (MP.PosState (Lexemes input) _ (MP.SourcePos _ line col) _ _))
  = Text.unlines . (prefix:) . toList $ fmtError (Lexemes input) <$> errors
    where prefix = "Offending input: " <> ppLexemes input

fmtError :: Lexemes -> MP.ParseError Lexemes ParserErr -> Text
fmtError _ (MP.FancyError pos _) = "unexpected fancy error at pos " <> show' pos
fmtError input (MP.TrivialError pos unexpected expected) =
  "Parse Error at pos " <> show' pos
  <> ": found " <> maybe "nothing" show' unexpected <>
  ", expected one of: " <> showLexons' expected <> "\n" <> highlightErr input pos
    where showLexons' ts = intercalate ", " $ ppToken <$> toList ts

ppToken :: MP.ErrorItem (MP.Token Lexemes) -> Text
ppToken (MP.Label l) = pack $ toList l
ppToken (MP.Tokens ls) = ppLexemes $ toList ls
ppToken (MP.EndOfInput) = "End-of-Input"

-- | Print an input string with an error highlighted as Pos
highlightErr :: Lexemes -> Int -> Text
highlightErr (Lexemes input) pos = ppLexemes input <> "\n" <> spaces <> "^"
  where indent = max 0 (pos - 1)
        spaces = pack $ take indent $ repeat ' '

show' :: Show a => a -> Text
show' = pack . show
