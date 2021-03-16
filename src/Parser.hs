module Parser (
  parseModule
) where

import Control.Applicative ((<|>), many, some)
import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Text.Megaparsec (try)
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (char, digitChar, letterChar, space, space1)

import Types (Binding(..), Expr(..), Ident(..))

parseExpr :: String -> Maybe Expr
parseExpr src = P.parseMaybe expr src

-- | A Module is a list of bindings
-- ```
--   x = 2
--
--   add2 = \x -> x + 2
parseModule :: String -> Either Text [Binding Expr]
parseModule src = first (pack . show) $ P.parse (many binding) "<no file>" src

data Err = Err deriving (Show, Eq, Ord)

type Parser = P.Parsec Err String

binding :: Parser (Binding Expr)
binding = Binding <$> ident <* space <*> (char '=' *> space *> expr)

expr :: Parser Expr
expr = try app
   <|> try lambda
   <|> try var
   <|> try lit

lit :: Parser Expr
lit = Lit <$> int
  where int = read <$> (some digitChar)

var :: Parser Expr
var = Var <$> ident

ident :: Parser Ident
ident = Ident . pack <$> some letterChar

lambda :: Parser Expr
lambda = Lambda <$> (char '\\' *> ident <* space <* (char '-' <* char '>' <* space)) <*> expr

app :: Parser Expr
app = App <$> fn <* space1 <*> expr
  where fn = var <|> parens expr

parens :: Parser a -> Parser a
parens p = open *> p <* close
 where open = char '(' *> space
       close = space <* char ')'
