{-# language OverloadedStrings #-}

module Parser.Errors where

import Data.Foldable (toList)
import Data.Text (Text, pack, intercalate)
import qualified Data.Text as Text
import qualified Text.Megaparsec as P

import Parser.Types (showLexons, TokenStream(..))

data Err = Err deriving (Show, Eq, Ord)

fmtParseError :: P.ParseErrorBundle TokenStream Err -> Text
fmtParseError (P.ParseErrorBundle errors (P.PosState (TokenStream input) _ (P.SourcePos _ line col) _ _))
  = Text.unlines . (prefix:) . toList $ fmtError (TokenStream input) <$> errors
    where prefix = "Offending input: " <> showLexons input

fmtError :: TokenStream -> P.ParseError TokenStream Err -> Text
fmtError _ (P.FancyError pos _) = "unexpected fancy error at pos " <> showT pos
fmtError input (P.TrivialError pos unexp_token exp_tokens) =
  "Parse Error at pos " <> showT pos
  <> ": found " <> maybe "nothing" showT unexp_token <>
  ", expected one of: " <> showLexons' exp_tokens <> "\n" <> highlightErr input pos
    where showLexons' ts = intercalate ", " $ showtoken <$> toList ts

showtoken :: P.ErrorItem (P.Token TokenStream) -> Text
showtoken (P.Label l) = pack $ toList l
showtoken (P.Tokens ts) = showLexons $ toList ts
showtoken (P.EndOfInput) = "EndOfInput"

-- | Print an input string with an error highlighted as Pos
highlightErr :: TokenStream -> Int -> Text
highlightErr (TokenStream input) pos = showLexons input <> "\n" <> spaces <> "^"
  where indent = max 0 (pos - 1)
        spaces = pack $ take indent $ repeat ' '

showT :: Show a => a -> Text
showT = pack . show

