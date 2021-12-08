{-# LANGUAGE OverloadedStrings #-}

module Test.Lang.Lexer where

import Data.List.NonEmpty(NonEmpty(..))
import Data.Set (fromList)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Pos (Pos(..))

import Lang.Lexer (lexMaybe, lex', Lexeme(..))

unitTests :: TestTree
unitTests = testGroup "Lexer" [ lexemeTest
                              , lineTest
                              , moduleTest
                              , invalidLexeme
                              , lambdaTest
                              ]

lexemeTest :: TestTree
lexemeTest = testCase "an individual identifier" $ assertEqual "failed to lex identifier"
  (Just [(Identifier "anIdentifier_2")])
  (lexMaybe "anIdentifier_2")

-- TODO empty input

invalidLexeme :: TestTree
invalidLexeme = testCase "an invalid lexeme" $ assertEqual "Invalid lexeme parsed unexpectedly successfully"
  (Left (MP.ParseErrorBundle {
    MP.bundleErrors = MP.TrivialError 3 (Just (MP.Tokens ('a' :| ""))) (fromList [MP.Label ('d' :| "igit")]) :| [],
    MP.bundlePosState = MP.PosState {
      MP.pstateInput = "123abc",
      MP.pstateOffset = 0,
      MP.pstateSourcePos = MP.SourcePos {
        MP.sourceName = "<no src>",
        MP.sourceLine = MP.mkPos 1,
        MP.sourceColumn = MP.mkPos 1
      },
      MP.pstateTabWidth = MP.mkPos 8,
      MP.pstateLinePrefix = ""
    }
  }))
  (lex' "<no src>" "123abc")

lineTest :: TestTree
lineTest = testCase "a trivial function def" $ assertEqual "Failed to lex function def correctly."
  (Just [
  (Identifier "f"),
  (Identifier "x"),
  (Identifier "y"),
  (Operator "="),
  (Identifier "x"),
  (Operator "+"),
  (Identifier "y")
  ])
  (lexMaybe "f x y=x+y")

lambdaTest :: TestTree
lambdaTest = testCase "a lambda def" $ assertEqual "Failed to lex lambda def correctly"
  (Just [
  (Identifier "f"),
  (Operator "="),
  (Operator "\\"),
  (Identifier "a"),
  (Operator "->"),
  (Operator "\\"),
  (Identifier "b"),
  (Operator "->"),
  (Identifier "a"),
  (Operator "+"),
  (Identifier "b")
        ])
  (lexMaybe "f = \\a -> \\b -> a + b")

moduleTest :: TestTree
moduleTest = testCase "module" $ assertEqual "Failed to lex module"
  (Just [
    (Reserved "module"),
    (Identifier "Foo"),
    (Reserved "where"),
    (Identifier "bar"),
    (Identifier "a"),
    (Identifier "b"),
    (Operator "="),
    (Identifier "a"),
    (Identifier "b")
  ])
  (lexMaybe $ unlines [
  "module Foo where       ",
  "                       ",
  "-- function bar comment",
  "bar a b = a b          "])

-- TODO enable me (add to test list)
complexModuleTest :: TestTree
complexModuleTest = testCase "module" $ assertEqual "Failed to lex module"
  (Just [
    (Reserved "module"),
    (Identifier "Foo"),
    (Reserved "where"),
    (Identifier "bar"),
    (Identifier "a"),
    (Identifier "b"),
    (Operator "="),
    (Identifier "a"),
    (Identifier "b")
  ])
  (lexMaybe $ unlines [
  "module Foo where       ",
  "                       ",
  "import Parser (parse)  ",
  "import qualified Parser as P",
  "                       ",
  "-- function bar comment",
  "bar :: (Int -> String) -> Int -> String",
  "bar a b = let x = a b in x"])
