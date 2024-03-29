{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (throwError, MonadError)
import Data.Either.Extra (maybeToEither)
--import Data.Foldable (for_)
import Data.Text (Text, unpack, pack)
import Options.Applicative (execParser)

import Args (Args(..), argParser)
--import CodeGen (printCode, runCodegen)
--import qualified Compiler
import Lang.Parser (Lexemes(..), parseModule, Module)
import Lang.Lexer (lexMaybe)

main :: IO ()
main = parse

-- TODO: Implement reading from a file
loadSrcFile :: String -> IO Text
loadSrcFile _ = return "file contents"

parse :: IO ()
parse = do
  args <- execParser argParser
  case args of
    (Args _ _ True) -> runRepl
    _ -> parseSource args

runRepl :: IO ()
runRepl = putStrLn "repl!"
-- TODO: use haskeline to impl a repl
-- e.g. runInputT $ loop
-- where loop = do
--   minpt <- getInptLine "prompt>"
--   output <- parse minput
--   outputLine output >> loop

parseSource :: Args -> IO ()
parseSource args = do
  src <- case args of
    (Args (Just file) _ _) -> loadSrcFile file
    (Args Nothing (Just src) _) -> return (pack src)
    (Args Nothing Nothing _) -> error "Must specify either -f or -s"
  case parseIIR src of
    Left err -> putStrLn $ unpack err
    Right mod' -> putStrLn $ show mod'
    --Right program -> do
      --let fns = bindingValue <$> program
      --for_ fns printFn

  {-
printFn :: IIR.Function -> IO ()
printFn (IIR.Function insts protos) = do
  putStrLn "---function"
  printCode . runCodegen $ insts
  putStrLn "---protos"
  for_ protos printFn
  -}

--iir :: Either Text [Binding IIR.Function]
--iir = parseIIR "id = \\x -> x"

--expr :: Either Text [Binding Expr]
--expr = parseExpr "id = \\x -> x"

  {-
parseExpr :: (MonadError Text m) => Text -> m [Binding Expr]
parseExpr source = do
  let tokenStream = tokenize (unpack source)
  -- Parse the source code into an AST
  eitherToError $ parseModule tokenStream
  -}

parse' :: String -> Either Text Module
parse' src =
  let lexemes = Lexemes <$> lexMaybe src in
  parseModule =<< maybeToEither "Failed to Lex" lexemes

parseIIR :: (MonadError Text m) => Text -> m Module
parseIIR source = do
  -- Parse the source code into an AST
  bindings <- eitherToError $ parse' (unpack source)
  -- Transmute the AST into our IIR (Imperative Intermediate Representation)
  --return $ Compiler.compileBindings bindings
  return bindings

{-
-- | Compile the Ithil source code `src` into Lua bytecode
compile :: (MonadError Text m, MonadCompile m) => Text -> m Text
compile src = do
  -- Parse the source code into an AST
  ast <- parse src `orError` "Foo"
  -- Transmute the AST into our IIR (Imperative Intermediate Representation)
  (Fn iir) <- Compiler.compile ast
  -- Generate Code for the IIR
  code <- codeGen iir
  -- Allocate Registers for our generic vars
  codeWithRx <- allocateRx code
  -- Write out the bytecode as Text
  serializeByteCode codeWithRx
  -}

orError :: MonadError e m => Maybe a -> e -> m a
orError val err = maybe (throwError err) return val

eitherToError :: MonadError e m => Either e a -> m a
eitherToError = either throwError return
