{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Error (throwError, MonadError)
import Data.Foldable (for_)
import Data.Text (Text, unpack)

import CodeGen (printCode, runCodegen)
import qualified IIR
import IIR (Arg(..), Variable(..), BinOp(..))
import qualified Compiler
import Parser (parseModule)
import Types (Binding(..), Expr)

  {-
main :: IO ()
main = printCode $ runCodegen program
  where program = [
                    (IIR.Move (Variable 0) (Arg 0)),
                    (IIR.Call (Variable 1) (Variable 2) [Var (Variable 0), Arg 1]),
                    (IIR.Prim Add (Variable 3) (Var (Variable 1)) (Const 127)),
                    (IIR.Return (Var (Variable 3)))
                  ]
                  -}

{-
main = do
  filename <- arg
  contents <- T.readFile filename
  let output = compile contents
  T.putStrLn output
-}

{-
   example prog:

   add2 = \x -> x + 2
-}

main :: IO ()
--main = print $ iir
main = code

code :: IO ()
code = do
  let program = either (error . unpack) id $ parseIIR "id = \\x -> (\\y -> y) x"

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
