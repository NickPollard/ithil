{-# LANGUAGE OverloadedStrings #-}

module IIR.Compiler where

import Control.Monad.Except (throwError)
import Control.Monad.State (StateT(..), runStateT, modify', gets, state)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import qualified Lang.Types as Lang
import Lang.Types (Expr(..), Ident(..))
import IIR.Types (SVal(..), IIR(..), Rx(..), PrimOp(..), Constant(..), Arg(..), FnTemplate)

-- Compiling takes a lambda-calculus `Expr` (essentially our AST) and produces code in IIR.
compile :: Expr -> Either Text Function
compile expr = _fn . snd <$> runCompile (compile' expr)

runCompile :: CompileM a -> Either Text (a, CompileState)
runCompile comp = runStateT comp initState

initState :: CompileState
initState = CompileState Map.empty (Function [] []) (Rx 0)




------ Types ------

-- An Environment is just a mapping of variable identifiers to simple values
type Env = Map Ident SVal

data Function = Function {
  -- name :: Text -- do I need/want this?
  _body :: [IIR],
  templates :: [Function]
}

-- Compile state used in our monadic computations - containing a cursor into our
-- tree of output functions, and the current binding environment
data CompileState = CompileState {
  _env :: Env,
  _fn :: Function,
  _rx :: Rx
}

-- TODO(nickpollard) - define lenses for CompileState

addFunction :: Function -> CompileState -> CompileState
addFunction fn (CompileState env fn' rx) = (CompileState env fn' { templates = (fn : templates fn') } rx)

addVar :: Ident -> SVal -> CompileState -> CompileState
addVar id' val (CompileState env fn' rx) = (CompileState (Map.insert id' val env) fn' rx)

-- Compile monad that supports writing out a list of instructions, and reading
-- in an environment
type CompileM a = StateT CompileState (Either Text) a

-- Write an instrution to the current function (Insts are in reverse order)
emit :: IIR -> CompileM ()
emit inst = modify' $
  \(CompileState env (Function body templates') rx) -> (CompileState env (Function (inst:body) templates') rx)

-- Allocate a new unique register index
-- Return the current next and increment the counter
newRx :: CompileM Rx
newRx = state $ \(CompileState env fn (Rx n)) -> ((Rx n), (CompileState env fn (Rx (n+1))))

-- Allocate a new unique SVal-wrapped register index
newReg :: CompileM SVal
newReg = Reg <$> newRx

-- Add a new variable identifier binding
bind :: Ident -> SVal -> CompileM ()
bind id' val = modify' $ addVar id' val

writeFn :: Function -> CompileM ()
writeFn fn = modify' $ addFunction fn

-- TODO - implement this - turn a variable identifier reference into an SVal
lookup' :: Ident -> CompileM SVal
lookup' ident = do
  sval <- gets $ Map.lookup ident . _env
  maybe (throwError "Missing variable") return sval

------ Compilation Impl ------

compile' :: Expr -> CompileM SVal
compile' (Lambda arg body) =
  newLambda arg body

-- FULLY SATURATED PRIM - look for saturated apply, emit prim
compile' (App (App (BuiltIn op) a) b) = do
  a' <- compile' a
  b' <- compile' b
  rx <- newRx
  emit $ PRIM (primOp op) a' b' rx
  return $ Reg rx

-- PARTIAL SATURATED PRIM - if partial or unsaturated, eta expand into closure
-- e.g.
--   ADD y
--     becomes
--   \x -> ADD y x
compile' (App (BuiltIn op) a) = do
  -- compile `a` so it's ready to be closed over
  upvalue_a' <- compile' a
  -- Build a new function template and then create a closure over the upvalue
  -- TODO replace "$_1" with a guaranteed unique local var
  -- Need an upvalue type in SVal?
  upvalueRef <- undefined
  newLambda (Ident "$_1") (App (App (BuiltIn op) upvalueRef) (Var $ Ident "$_1"))

-- Base APP case
compile' (App f a) = do
  f' <- compile' f
  a' <- compile' a
  rx <- newRx
  -- TODO check f' is a callable
  emit $ CALL f' [a'] rx
  return $ Reg rx
compile' (Var varIdent) = do
  lookup' varIdent
compile' (Lit int') = return . Const $ KInt int'
compile' (BuiltIn op) = return . Prim $ primOp op
compile' (IfThenElse _ _ _) = error "NYI" -- TODO handle if-then-else

-- TODO declare as an iso?
primOp :: Lang.BinaryOp -> PrimOp
primOp Lang.Add = ADD
primOp Lang.Mul = MUL
primOp Lang.Sub = error "sub NYI"
primOp Lang.Div = error "div NYI"

-- TODO - impl this, how does it deal with upvalues/closure creation?
newLambda :: Ident -> Expr -> CompileM SVal
newLambda arg body = do
  -- Write the template to the sub-protos section of this function
  template <- writeFnTemplate arg body
  -- Reify a closure including upvalues
  rx <- newRx
  let upvalues = undefined
  emit $ CLOSURE template upvalues rx
  return $ Reg rx

-- Build a function template and return the index of it
writeFnTemplate :: Ident -> Expr -> CompileM FnTemplate
writeFnTemplate identifier body = do
  -- allocate a new subfunction
  compiled <- return . runCompile $ do
    -- existing vars in the environment get marked as Upvalues - record their use when hit
    -- set `Ident` to `Arg0` in the environment of that function
    bind identifier (Argument $ Arg 0)
  -- `compile body` within that subfunction
    compile' body
  case compiled of
    Left errMsg -> throwError errMsg
    Right (_, CompileState _ fn _) -> do
      writeFn fn
      -- TODO
      return undefined
