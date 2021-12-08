{-# LANGUAGE OverloadedStrings #-}

module Test.Lang.Parser where

import Data.Either.Extra (maybeToEither)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Lang.Lexer (lexMaybe)
import Lang.Parser (parseModule, Binding(..), Module(..), Lexemes(..))
import Lang.Types (Expr(..), BinaryOp(..), Ident(..))

unitTests :: TestTree
unitTests = testGroup "Parser" [ parserTest
                               , trivialBinding
                               , prefixOp
                               , associativity
                               , infixOp
                               ]

parserTest :: TestTree
parserTest = testCase "Default Parser Test" $ assertEqual "Failed Default Parser Test"
  0
  0

trivialBinding :: TestTree
trivialBinding = testCase "A trivial binding" $ assertEqual "Failed to parse trivial binding"
  (Right (Module {
    moduleName = "<anonymous>",
    moduleBindings = [Binding (Ident "f") (App (Var (Ident "g_")) (Var (Ident "x")))]
  }))
  (parse' "f = g_ x")

prefixOp :: TestTree
prefixOp = testCase "A prefix op" $ assertEqual "Failed to parse prefix operator"
  (Right (Module {
    moduleName = "<anonymous>",
    moduleBindings = [Binding (Ident "f") (App (BuiltIn Add) (Var (Ident "x")))]
  }))
  (parse' "f = (+) x")

associativity :: TestTree
associativity = testCase "Associativity" $ assertEqual "Failed to parse trivial binding"
  (Right (Module {
    moduleName = "<anonymous>",
    moduleBindings = [Binding (Ident "f") (App (App (BuiltIn Add) (Var (Ident "x"))) (Var (Ident "y")))]
  }))
  (parse' "f = (+) x y")

infixOp :: TestTree
infixOp = testCase "An infix op" $ assertEqual "Failed to parse infix operator"
  (Right (Module {
    moduleName = "<anonymous>",
    moduleBindings = [Binding (Ident "f") (App (App (BuiltIn Add) (Var (Ident "x"))) (Var (Ident "y")))]
  }))
  (parse' "f = x + y")


-- TODO
-- parens: (parse' "f = ((+) x y)")
-- precedence: (parse' "f = (+) (x y)")
-- TODO
--   match-args
--   "add a b = a + b"
--   "withTwo f = f 2"
--   "triangleArea a b = a * b / 2"
--   if-then, comparison
--   "factorial n = if n < 2 then 1 else n * factorial n"
-- TODO Full example
  {-
infixOp :: TestTree
infixOp = testCase "A prefix op" $ assertEqual "Failed to parse infix operator"
  (Right (Module {
    moduleName = "<anonymous>",
    moduleBindings = [Binding (Ident "f") (App (App (BuiltIn Add) (Var (Ident "x"))) (Var (Ident "y")))]
  }))
  (parse' "f x y =  (x * x) + (2 * y)")
  -}

parse' :: String -> Either Text Module
parse' src =
  let lexemes = Lexemes <$> lexMaybe src in
  parseModule =<< maybeToEither "Failed to Lex" lexemes
