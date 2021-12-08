{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.Types where

import Data.Text (Text, pack, unpack)

-- An error produced by the lexer
data LexerError = LexerInvalidToken deriving (Eq, Ord, Show)

-- | An AST Expression
data Expr = Lambda Ident Expr
          | App Expr Expr
          | Var Ident
          | Lit Int
          | BuiltIn BinaryOp
          | IfThenElse Expr Expr Expr
          deriving (Eq)

-- | Syntax sugar available in the front-end language but not in Core
data Sugar = Let Ident Expr Expr
           | Where Ident Expr

-- | A source code identifier
-- Identifiers must begin with an alphabet or underscore character, and may contain alphanumeric
-- characters, underscores and primes (')
newtype Ident = Ident Text
  deriving (Show, Ord, Eq)

-- | A builtin binary operation such as + or *
data BinaryOp = Add
              | Mul
              deriving (Eq, Show)

-- TODO better parens
-- TODO test for this output!
pretty :: Expr -> Text
pretty = pretty' False

pretty' :: Bool -> Expr -> Text
pretty' _ (Lit i)            = pack $ show i
pretty' _ (Var (Ident i))    = i
pretty' True  (App a b)      = "(" <> pretty' True a <> " " <> pretty' True b <> ")"
pretty' False (App a b)      = pretty' True a <> " " <> pretty' True b
pretty' True  (Lambda (Ident v) b) = "(\\" <> v <> " -> " <> pretty' False b <> ")"
pretty' False (Lambda (Ident v) b) = "\\" <> v <> " -> " <> pretty' False b
pretty' _ (BuiltIn Add)        = "+"
pretty' _ (BuiltIn Mul)        = "*"
pretty' _ (IfThenElse if' then' else') = "if " <> pretty' False if' <> " then " <> pretty' False then' <> " else " <> pretty' False else'


instance Show Expr where
  show = unpack . pretty

-- | A top-level binding of a language expression to an identifier
-- e.g.
--   x = 2
-- is a binding of (Lit 2) to the Ident "x"
data Binding a = Binding { bindingName :: Ident
                         , bindingValue :: a
                         } deriving (Functor, Foldable, Traversable, Show)


-- An encoding of a simple type system
data Type = PrimType Prim
          | FnType Type Type

data Prim = IntPrim | BoolPrim

  {-
infer :: Expr -> Type
infer (Lit _) = PrimType IntPrim
infer (Var _) = ???
infer (BuiltIn _) = PrimType IntPrim
infer (App f x) = case (infer f, infer x) of
                    ((FnType a b), c) if a == c -> a
                    otherwise -> error "TypeError"
infer (Lambda ident x) = do
  a <- newVar ident
  FnType a (infer x)

check :: Expr -> Type -> Bool
check expr typ = (infer expr == typ)
-}
