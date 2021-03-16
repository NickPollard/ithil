{-# LANGUAGE GADTs #-}

module Lib
    ( eval
    , prettyPrint
    ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Text.Megaparsec as P
import Text.Megaparsec.Char (char, digitChar, letterChar, space, space1)

type VIdent = Char

-- \x. (\y. x) 4 5
-- > 4
--
-- TODO:
--   built-ins
--   let bindings
--   if-then-else

data Expr = Lambda VIdent Expr
          | App Expr Expr
          | Var VIdent
          | Lit Int

type Env = Map VIdent Expr

eval :: Env -> Expr -> Maybe Expr
eval _    (Lambda v e) = Just $ Lambda v e -- TODO - Need to handle upvalues here
eval vars (Var v)      = Map.lookup v vars
eval _    (Lit i)      = Just $ Lit i
eval vars (App fn arg) =
  do
   fn' <- eval vars fn
   a <- eval vars arg
   case fn' of
     (Lambda v body) -> eval (Map.insert v a vars) body
     Lit _   -> Nothing
     Var _   -> Nothing
     App _ _ -> Nothing

prettyPrint :: Expr -> String
prettyPrint (Lit i)      = show i
prettyPrint (Var v)      = [v]
prettyPrint (App a b)    = "(" <> prettyPrint a <> ") (" <> prettyPrint b <> ")"
prettyPrint (Lambda v b) = "\\" <> [v] <> ". (" <> prettyPrint b <> ")"

instance Show Expr where
  show = prettyPrint
