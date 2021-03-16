{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text (Text, pack, unpack)

-- | A source code identifier
-- Identifiers must begin with an alphabet or underscore character, and may contain alphanumeric
-- characters, underscores and primes (')
newtype Ident = Ident Text
  deriving (Show, Ord, Eq)

-- | An AST Expression
data Expr = Lambda Ident Expr
          | App Expr Expr
          | Var Ident
          | Lit Int
          | BuiltIn BinaryOp

-- | A top-level binding of a language expression to an identifier
-- e.g.
--   x = 2
-- is a binding of (Lit 2) to the Ident "x"
data Binding a = Binding { bindingName :: Ident
                         , bindingValue :: a
                         } deriving (Functor, Applicative, Monad, Foldable, Traversable, Show)

data BinaryOp = Add
              | Mul

  {-
     infix operators should probably just parse into standard app, e.g.
        x + y
     is just syntax sugar for
        (+) x y
     -}

pretty :: Expr -> Text
pretty (Lit i)              = pack $ show i
pretty (Var (Ident i))      = i
pretty (App a b)            = "(" <> pretty a <> ") (" <> pretty b <> ")"
pretty (Lambda (Ident v) b) = "\\" <> v <> "-> (" <> pretty b <> ")"
pretty (BuiltIn Add) = "+"
pretty (BuiltIn Mul) = "*"

instance Show Expr where
  show = unpack . pretty
