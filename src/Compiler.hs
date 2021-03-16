{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
--{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Compiler (
  MonadCompile,
  compileBindings,
  execCompile
) where

import Control.Monad.State (MonadState, get, State, modify, execState, state)
import Control.Lens (over)
import Data.List.Extra (snoc)
import qualified Data.Map as Map
import Data.Tuple.Extra ((&&&))

import Compiler.Types
import qualified IIR as IIR
--import IIR (IIR)
import Lang.Types (Binding, Expr(..))

compileBinding :: Binding Expr -> Binding IIR.Function
compileBinding = fmap (execCompile . compileFn)

compileBindings :: [Binding Expr] -> [Binding IIR.Function]
compileBindings = fmap compileBinding

compileFn :: MonadCompile m => Expr -> m ()
compileFn body = do
  v <- compile body
  emit $ IIR.Return (IIR.Var v)

-- `Compile` is our main implementatio of MonadCompile, which uses a simple
-- state monad to track variable bindings and emitted instructions.
newtype Compile a = Compile { runCompile :: State Env a } deriving (Functor, Applicative, Monad)

deriving instance MonadState Env Compile

-- Evaluate a compilation inside `Compile`
execCompile' :: Env -> Compile a -> IIR.Function
execCompile' env job = extractFn $ execState (runCompile job) env
  where extractFn (Env _ _ is ps) = IIR.Function is ps

execCompile :: Compile a -> IIR.Function
execCompile = execCompile' emptyEnv

instance MonadCompile Compile where
  -- Emit an IIR instruction which will present in the compiled code
  emit inst = modify $ over _insts (flip snoc inst)

  -- Resolve the binding for variable `Ident`
  resolve ident = do
    env <- Compile $ get
    case Map.lookup ident (vars env) of
      Just v -> return v
      Nothing -> error $ "Reference to undefined variable " <> show ident

  -- Allocate an unbound variable
  allocate = state $ (IIR.Variable . top) &&& (over _top (+1))

  proto argname fn = do
    envWithArg <- over _vars (Map.insert argname (IIR.Arg 0)) <$> get
    p <- length . protos <$> get
    modify $ over _protos (flip snoc $ execCompile' envWithArg fn)
    return p

{-
-- Insert a new variable binding for `Ident` into the enclosing environment
insertVar :: Ident -> Compile IIR.Arg
insertVar ident = do
  env <- Compile $ get
  let vars' = vars env
  let v = top env
  Compile . put $ env { vars = Map.insert ident (IIR.Var $ IIR.Variable v) vars', top = v + 1 }
  return . IIR.Var $ IIR.Variable v
  -}

-- Compile `Expr` into a series of instructions that cause the evaluated
-- result to be located in `Arg`
compile :: MonadCompile m => Expr -> m IIR.Variable
compile (Var identifier) = do
  v <- resolve identifier
  case v of
    IIR.Var v' -> return v'
    a -> do
      var <- allocate
      emit $ IIR.Move var a
      return var
compile (Lit k) = do
  var <- allocate
  emit $ IIR.Move var (IIR.Const k)
  return var
compile (Lambda ident body) = do
  p <- proto ident $ compileFn body
  var <- allocate
  emit $ IIR.Proto var p
  return var
compile (App fn arg) = do
  fn' <- compile fn
  arg' <- compile arg
  var <- allocate
  emit $ IIR.Call var fn' [IIR.Var arg']
  return var
-- TODO - compile builtin (if I don't remove it)
compile (BuiltIn _) = error "Builtins are currently unimplemented"





{-
    a worked example:
      lambda:
        \x,y -> x + y

      IIR:
        BinOp Add var0 arg0 arg1
        Return var0

      lambda:
        \x -> (\y -> y + y) x

      IIR:
        Call var0 fn0 arg0
        return var0
        -- fn0

      lambda:
        \x,y -> (\t,u -> u)(y, x)

      IIR:
        # begin fn f1
        push_closure f2
        push arg1
        push arg2
        call
        ..
        return

        # begin fn f2
        return arg2
-}
