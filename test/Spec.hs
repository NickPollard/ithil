{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except (throwError, MonadError)
import Data.Text (Text, unpack)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Compiler
import qualified IIR
import Lang.Types (Binding(..))
import Parser.Tokenizer (tokenize)
import Parser (parseModule)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "UnitTests" [ test ]

test :: TestTree
test = testCase "foo" $ doTest

doTest :: IO ()
doTest = printIIR $ parseIIR "{\\x add x 2) 3"

printIIR :: Either Text [Binding IIR.Function] -> IO ()
printIIR iir =
  case iir of
    Left err -> assertFailure $ unpack err
    Right _ -> return ()

-- TODO move somewhere (this is duplicated in Main.hs)
parseIIR :: (MonadError Text m) => Text -> m [Binding IIR.Function]
parseIIR source = do
  let tokenStream = tokenize (unpack source)
  -- Parse the source code into an AST
  bindings <- eitherToError $ parseModule tokenStream
  -- Transmute the AST into our IIR (Imperative Intermediate Representation)
  return $ Compiler.compileBindings bindings

-- TODO lift to util
eitherToError :: MonadError e m => Either e a -> m a
eitherToError = either throwError return
