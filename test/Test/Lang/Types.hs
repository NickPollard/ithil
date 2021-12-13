{-# LANGUAGE OverloadedStrings #-}

module Test.Lang.Types where

import Test.Tasty
import Test.Tasty.HUnit

import Lang.Types (Expr(..), Ident(..), pretty)

unitTests :: TestTree
unitTests = testGroup "Types" [ prettyTest
                              ]

prettyTest :: TestTree
prettyTest = testCase "pretty-print expr" $ assertEqual "failed to pretty-print"
  (pretty (Lambda (Ident "x") (App (Var (Ident "f")) (Var (Ident "x")))))
  "\\x -> f x"
