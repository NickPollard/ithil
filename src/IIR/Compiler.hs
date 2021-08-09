module IIR.Compiler where

import Control.Monad.State (StateT)
import Data.Map (Map)
import qualified Data.Map as Map

-- Compiling takes a lambda-calculus `Expr` (essentially our AST) and produces code in IIR.
--compile :: Expr -> Either Text IIR
--compile expr = withFreshEnv $ compile' expr

runCompile :: CompileM a -> Either Text (CompileState, a)
runCompile comp = runStateT comp initState

initState :: CompileState
initState = CompileState Map.empty (Function [] [])




------ Types ------

data Env = Map Ident SVal

data Function = Function {
  -- name :: Text -- do I need/want this?
  body :: [Inst],
  templates :: [Function],
}

-- Compile state used in our monadic computations - containing a cursor into our
-- tree of output functions, and the current binding environment
data CompileState = CompileState Env Function

addFunction :: Function -> CompileState -> CompileState
addFunction fn (CompileState env fn') = (CompileState env fn' { templates :: (fn : templates fn')})

-- Compile monad that supports writing out a list of instructions, and reading
-- in an environment
data CompileM a = StateT CompileState (Either Text) a

emit :: Inst -> CompileM ()
emit inst = \_ -> ([inst], ())

-- Allocate a new unique register index
-- TODO - env will have to store allocated Rx info
newRx :: CompileM Rx
newRx = undefined

-- Allocate a new unique SVal-wrapped register index
newReg :: CompileM SVal
newReg = Reg <$> newRx

-- Add a new variable identifier binding
bind :: Ident -> SVal -> CompileM
bind id val = modify' $ Map.insert id val

writeFn :: Function -> CompileM ()
writeFn fn = modify $ addFunction fn


------ Compilation Impl ------

compile' :: Expr -> CompileM SVal
compile' (Lambda arg body) =
  newLambda arg body

-- FULLY SATURATED PRIM - look for saturated apply, emit prim
compile' (App (App (BuiltIn op) a) b) = do
  a' <- compile' a
  b' <- compile' b
  rx <- newRx
  emit <- $ PRIM op a' b' rx
  return rx

-- PARTIAL SATURATED PRIM - if partial or unsaturated, eta expand into closure
-- e.g.
--   ADD y
--     becomes
--   \x -> ADD y x
compile' (App (BuiltIn op) a) = do
  -- compile a so it's ready to be closed over
  upvalue_a' <- compile' a
  -- Build a new function template and then create a closure over the upvalue
  -- TODO replace "$_1" with a guaranteed unique local var
  newLambda "$_1" (App (App (BuiltIn op) upvalue_a' "$_1"))

-- Base APP case
compile' (App f a) = do
  f' <- compile' f
  a' <- compile' a
  rx <- newRx
  emit $ CALL f' [a'] rx
  return rx
compile' (Var varIdent) = do
  getValForVar' varIdent
compile' (Lit int') = return . Const $ KInt int''
compile' (BuiltIn op) = primOp op

getValFor' :: Ident -> CompileM SVal
getValFor' ident = undefined

primOp :: Lang.Prim -> CompileM SVal
primOp Lang.Add = Prim ADD
primOp Lang.Mul = Prim MUL


{-
-- Given a var identifier, return an SVal that references the value
getValForVar :: Map Ident SVal -> Ident -> Either Text SVal
getValForVar env (Ident varName) =
  maybe (writeError $ "Could not find var '" <> varName <> "'") return . lookup env varName
-}

-- TODO - impl this, how does it deal with upvalues/closure creation?
newLambda :: Ident -> Expr -> CompileM SVal
newLambda arg body = do
  -- Write the template to the sub-protos section of this function
  template <- writeFnTemplate arg body
  -- Reify a closure including upvalues
  rx <- newRx
  emit $ CLOSURE template upvalues rx
  return rx

-- Build a function template and return the index of it
writeFnTemplate :: Ident -> Expr -> CompileM FnTemplate
writeFnTemplate identifier body = do
  -- allocate a new subfunction
  let compiled = runCompile $ do
    -- existing vars in the environment get marked as Upvalues - record their use when hit
    -- set `Ident` to `Arg0` in the environment of that function
    bind identifier (Argument $ Arg 0)
  -- `compile body` within that subfunction
    compile' body
  case compiled of
    Left errMsg -> throwError errMsg
    Right (CompileState _ fn, _) -> writeFn fn
