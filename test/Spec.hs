{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty

import qualified Test.Lang.Lexer as Lexer
import qualified Test.Lang.Parser as Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ Lexer.unitTests
                          , Parser.unitTests
                          ]
