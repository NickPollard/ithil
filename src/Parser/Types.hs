{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}

module Parser.Types (
  showLexons,
  Lexon(..),
  TokenStream(..),
) where

import Data.Text (Text, pack, intercalate)

import Text.Megaparsec (Stream(..))

-- Tokens that can be tokenized from the raw stream of characters of a text source file
-- Parsing is two-stage:
--   the first stage tokenizes from [char] -> Either [token] Error
--   the second parses [token] -> AST
--   TODO: s/lexon/lexeme
data Lexon = Identifier Text
           | Reserved Text
           | Operator Text
           | Literal Int
           deriving (Ord, Eq, Show)

showLexons :: [Lexon] -> Text
showLexons = intercalate " " . fmap showToken

showToken :: Lexon -> Text
showToken (Identifier ident) = ident
showToken (Reserved res) = res
showToken (Operator op) = op
showToken (Literal i) = pack $ show i

data TokenStream = TokenStream [Lexon]

-- TODO implement the `Stream` typeclass from Megaparsec
-- This is to allow tokenizing first, and then parsing a stream of tokens
-- in a second, distinct pass
instance Stream TokenStream where
  type Token TokenStream = Lexon
  type Tokens TokenStream = [Lexon]
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  take1_ (TokenStream (x:xs)) = Just (x, TokenStream xs)
  take1_ (TokenStream []) = Nothing
  takeN_ n (TokenStream s)
    | n <= 0    = Just ([], TokenStream s)
    | otherwise = Just ((take n s), TokenStream (drop n s))
  takeWhile_ p (TokenStream s) =
    let taken = takeWhile p s
     in (taken, TokenStream (drop (length taken) s))
