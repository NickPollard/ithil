--
-- Parse command line arguments for the compiler program
--
module Args (
  argParser,
  Args(..),
) where

import Options.Applicative

data Args = Args { filename :: Maybe String
                 , input :: Maybe String
                 , repl :: Bool
                 }
argParser :: ParserInfo Args
argParser = info argParser' fullDesc

argParser' :: Parser Args
argParser' = Args <$> filenameParser <*> inputParser <*> replParser

filenameParser :: Parser (Maybe String)
filenameParser = optional $ strOption ( long "file" <> short 'f' <> metavar "FILENAME" <> help "Path to the file to be parsed")

inputParser :: Parser (Maybe String)
inputParser = optional $ strOption ( long "src" <> short 's' <> metavar "SRC" <> help "literal source code to parse")

replParser :: Parser Bool
replParser = switch ( long "repl" <> help "run in repl mode")
