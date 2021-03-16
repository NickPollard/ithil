module Parser.Errors where

import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Text (Text, pack, unlines)
import qualified Data.Text as Text
import qualified Text.Megaparsec as P

data Err = Err deriving (Show, Eq, Ord)

fmtParseError :: P.ParseErrorBundle String Err -> Text
--fmtParseError (P.ParseErrorBundle errors (P.PosState _ _ (P.SourcePos _ line col) _ _))
--  = pack $ "Parse Error on line " <> show line <> ", col " <> show col
fmtParseError (P.ParseErrorBundle errors (P.PosState input _ (P.SourcePos _ line col) _ _))
  = Text.unlines . (prefix:) . toList $ pack . fmtError input <$> errors
    where prefix = pack $ "Offending input: " <> input

fmtError :: String -> P.ParseError String Err -> String
fmtError _ (P.FancyError pos _) = "unexpected fancy error at pos " <> show pos
fmtError input (P.TrivialError pos unexp_token exp_tokens) = "Parse Error at pos " <> show pos <> ": found " <> maybe "nothing" show unexp_token <> ", expected one of: " <> showtokens exp_tokens <> "\n" <> highlightErr input pos
  where showtokens ts = intercalate ", " $ showtoken <$> toList ts

showtoken :: P.ErrorItem (P.Token String) -> String
showtoken (P.Label l) = toList l
showtoken (P.Tokens ts) = toList ts
showtoken (P.EndOfInput) = "EndOfInput"

-- | Print an input string with an error highlighted as Pos
highlightErr :: String -> Int -> String
highlightErr input pos = input <> "\n" <> spaces <> "^"
  where indent = max 0 (pos - 1)
        spaces = take indent $ repeat ' '

