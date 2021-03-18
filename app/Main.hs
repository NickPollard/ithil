{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (throwError, MonadError)
import Data.Foldable (for_)
import Data.Text (Text, unpack, pack)
import Options.Applicative (execParser)

import Args (Args(..), argParser)
import CodeGen (printCode, runCodegen)
import qualified IIR
import qualified Compiler
import Parser (parseModule)
import Lang.Types (Binding(..), Expr)

main :: IO ()
main = parse

loadSrcFile :: String -> IO Text
loadSrcFile _ = return "file contents"

parse :: IO ()
parse = do
  args <- execParser argParser
  src <- case args of
    (Args (Just file) _ ) -> loadSrcFile file
    (Args Nothing (Just src)) -> return (pack src)
    (Args Nothing Nothing) -> error "Must specify either -f or -s"
  --let program = either (error . unpack) id $ parseIIR src
  case parseIIR src of
    Left err -> putStrLn $ unpack err
    Right program -> do
      let fns = bindingValue <$> program
      for_ fns printFn --(printCode . runCodegen)

printFn :: IIR.Function -> IO ()
printFn (IIR.Function insts protos) = do
  putStrLn "---function"
  printCode . runCodegen $ insts
  putStrLn "---protos"
  for_ protos printFn

iir :: Either Text [Binding IIR.Function]
iir = parseIIR "id = \\x -> x"

expr :: Either Text [Binding Expr]
expr = parseExpr "id = \\x -> x"

exampleProg :: Text
exampleProg = "\\x -> x + 2"

parseExpr :: (MonadError Text m) => Text -> m [Binding Expr]
parseExpr source = do
  -- Parse the source code into an AST
  eitherToError $ parseModule (unpack source)

parseIIR :: (MonadError Text m) => Text -> m [Binding IIR.Function]
parseIIR source = do
  -- Parse the source code into an AST
  bindings <- eitherToError $ parseModule (unpack source)
  -- Transmute the AST into our IIR (Imperative Intermediate Representation)
  return $ Compiler.compileBindings bindings

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
