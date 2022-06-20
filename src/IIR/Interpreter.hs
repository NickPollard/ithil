{-# language ImportQualifiedPost #-}
{-# language FlexibleContexts #-}

module IIR.Interpreter where

import Control.Monad.State (MonadState(..), State, StateT(..))
import Data.Map (Map)
import Data.Map qualified as Map
import IIR.Types (IIR(..), SVal(..), PrimOp(..))

data Env = Env { globals :: Map SVal IValue
               }

data Locals = Locals {}

-- A runtime interpreter value (Dynamically typed)
data IValue = VInt Int
            | VBool Bool
            | VClosure IClosure

data IClosure = IClosure { body :: [IIR]
                         -- For now upvalues can be inline here as they're immutable
                         , upvalues :: [IValue]
                          }

data IError = NoMainDefined
            | NoReturnValue

-- Evaluate a block of code, such as a program or individual function
eval :: [IIR] -> Either IError IValue
eval [i] = exec i
eval (i:is) = do
  exec i
  eval is

exec :: Env -> IIR -> ()
exec env (CLOSURE template upvalues' returnReg) =
  do
    let closure = IClosure { body = (body template), upvalues = upvalues' }
    store' returnReg (VClosure closure)

exec env (CALL fn args returnReg) =
  do
    -- Make a new stack `frame` including args
    newFrame args
    -- Evaluate the code in the context of `frame`
    eval frame (getFnInsts fn)

-- TODO - add do_return as a continuation passed in?
-- perhaps make the whole thing a continuation oriented?
exec env (RETURN v) = do_return v
exec env (PRIM op a b r) =
  case op of
    ADD -> store' r (a + b)
    MUL -> store' r (a * b)
    DIV -> store' r (a / b)
    SUB -> store' r (a - b)

store' :: MonadState Env m => SVal -> SVal -> m ()
store' key value = modify $ Map.insert key value

newFrame :: MonadState Env m => [SVal] -> m ()
newFrame args =
    for_ (indices `zip` args) uncurry store'
      where indices = ([1..] <&> (Argument . Arg))


-- Notes from what I scribbled on the bus
-- Sensible (hopefully) types for evaluation
-- Process is the monad for execution, just StateT with the program state
-- `read` and `write` to interact with the state
data Frame = Frame { closure :: Closure
                   , args :: [Arg]
                   }

data Closure = Closure { fn :: Proto
                       , upvalues :: [Upvalue]
                       }

data Proto = Proto { insts :: [Inst] }

-- TODO Add ExceptT to handle errors of type IError in the computation
data Process a = State ProcessState a

data ProcessState { prog :: Prog
                  , frame :: Frame
                  }

runProg :: Prog -> Process a -> a
runProg prog process = execState process prog

data FnHandle

data Prog = Prog { fns :: Map FnHandle Proto }

-- Read Arg in the ProcessState as a Value
read :: Arg -> Process Value

-- Store value in Arg in the ProcessState
write :: Arg -> Value -> Process ()

-- Call a closure value
call :: Closure -> [Arg] -> Process ()
call closure args = do
  frame <- return $ Frame closure args
  execFrame frame

execFrame :: Frame -> Process ()
execFrame (Frame closure args) = go frame args (insts $ fn closure)
  where
    go frame args (inst:insts) = do
      exec inst
      go frame args insts -- todo proper tail recursion
    go _ _ [] = return () -- TODO how do we actually handle 'RETURN' statements?

mkClosure :: FnHandle -> [Upvalue] -> Arg -> Process Closure
mkClosure fn ups r = do
  fn' <- findProto fn
  closure = Closure fn' ups
  write r closure

-- Find a proto in the running program
findProto :: FnHandle -> Process Proto
findProto = read' Map.lookup

runFn :: Prog -> Fn -> Either IError IValue
runFn = 

run :: Prog -> Either IError IValue
run prog = runProg prog $ do
  main <- findProto (FnHandle "main") `orError` NoMainDefined
  mainC <- mkClosure main [] -- no upvalues
  call mainC [] -- no args
