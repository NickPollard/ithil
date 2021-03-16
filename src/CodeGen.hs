{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module CodeGen (
  runCodegen,
  printCode
) where

import Control.Monad (join, forM, void)
import Control.Monad.State (MonadState, get, put, runState)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map

import IIR (IIR(..), Arg(..), BinOp(..), Variable(..))
import qualified IIR
import qualified Lua

-- TODO I should:
--  * Add pretty printing for `Instruction UniqRx`
--  * add a tests module for this function
--  * test that if I pass in certain pre-build IIR lists, they output correctly.

-- An index into the list of Function Constants
newtype Constant = Constant Int deriving (Show, Eq, Ord)

-- | An argument that is either a constant or a register index of type `r`
data Rk r = K Constant | R r
  deriving Show
  -- TODO - terser custom show impl

-- | An index to a particular proto in the enclosing function's proto array
data Pr = Pr Int deriving (Show, Eq, Ord)

-- For Register Allocation
-- `r` is the type of register allocations
data Instruction r = OP_MOVE r (Rk r)
                   -- ^ Load reg or constant. Equivalent to OP_MOVE or OP_LOADK in Lua, depending on const-ness
                   -- ^ move: a = b
                   | OP_RETURN r
                   -- ^ return the given variable
                   | OP_ADD r (Rk r) (Rk r)
                   -- ^ add
                   -- ^ a = b + c
                   | OP_CALL r r r
                   -- ^ a = b(c..)
                   | OP_PROTO r Pr
                   -- ^ a = proto(b)
                   deriving Show

printCode :: [Instruction UniqRx] -> IO ()
printCode insts = void $ forM insts (putStrLn . show)

-- A variable allocation, e.g. unique per var. Assumes we have infinite registers identified by the
-- natural numbers
newtype UniqRx = UniqRx Int
  deriving Show
  -- TODO - terser custom show impl
-- A register allocation; Multiple (non-overlapping) variables may be assigned to a given register;
-- Also a single variable may be assigned to multiple registers over its lifetime.
newtype Rx = Rx Int
  deriving Show

-- Generate LUA VM bytecode instructions from our IIR. This includes register
-- allocation.
--
-- Currently no signficant optimizations are performed
codegen :: MonadState Env m => [IIR] -> m [Instruction UniqRx]
codegen ir = join <$> traverse genInst ir

runCodegen :: [IIR] -> [Instruction UniqRx]
runCodegen ir = fst $ runState (codegen ir) mkEnv
  where mkEnv = Env { vars = Map.empty
                    , top = 0
                    , constants = Map.empty
                    , topConstant = 0
                    }

allocateRx :: [Instruction UniqRx] -> [Instruction Rx]
allocateRx = undefined

-- CodeGen environment
data Env = Env { vars :: Map IIR.Variable UniqRx
                 -- ^ Register assignments for variables
               , top :: Int
               , constants :: Map Constant Int
                 -- ^ Index assignments for constants
                 -- TODO do I even need the above?
               , topConstant :: Int
               }

-- Generate a new Register index
genRx :: MonadState Env m => m UniqRx
genRx = do
  env <- get
  let top' = top env
  put $ env { top = top' }
  return $ UniqRx top'

-- Lookup the register index for a var, or allocate one
varRx :: MonadState Env m => IIR.Variable -> m UniqRx
varRx x = do
  env <- vars <$> get
  maybe (newVarRx x) return $ Map.lookup x env

newVarRx :: MonadState Env m => IIR.Variable -> m UniqRx
newVarRx x = do
  rx <- genRx
  env <- get
  put $ env { vars = Map.insert x rx (vars env) }
  return rx

writeOp :: MonadState Env m => Instruction UniqRx -> m ()
writeOp = undefined

genInst :: MonadState Env m => IIR -> m [Instruction UniqRx]
genInst i = case i of
              Call res fn args -> genCall res fn args
              Return arg -> genReturn arg
              Move v arg -> return <$> moveVar v arg
              Prim op v a b -> genPrim op v a b
              Proto v p -> (\v' -> [OP_PROTO v' (Pr p)]) <$> varRx v
              --_ -> error "unhandled"

genCall :: MonadState Env m => Variable -> Variable -> [Arg] -> m [Instruction UniqRx]
genCall res fn args = do
  setupArgs <- traverse pushArg args
  arg_top <- top <$> get
  rx <- varRx res
  fn' <- varRx fn
  return $ setupArgs <> [OP_CALL rx fn' (UniqRx arg_top)]

genReturn :: MonadState Env m => Arg -> m [Instruction UniqRx]
genReturn arg = do
  (rx, insts) <- mkArgRx arg
  return $ insts <> [OP_RETURN rx]

-- Get the register for a given arg, and arguments (move or load) required to put the argument in a
-- register if necessary
mkArgRx :: MonadState Env m => Arg -> m (UniqRx, [Instruction UniqRx])
mkArgRx (Arg a) = return (UniqRx a, [])
mkArgRx (Var v) = varRx v <&> (,[])
mkArgRx (Const k) = do
  rx <- genRx
  k' <- defineConstant k
  return (rx, [OP_MOVE rx (K k')])

genPrim :: MonadState Env m => BinOp -> Variable -> Arg -> Arg -> m [Instruction UniqRx]
genPrim Add v a b = do
  rx <- varRx v
  rkA <- rk a
  rkB <- rk b
  return [OP_ADD rx rkA rkB]
genPrim Mul v a b = error "Mul is not yet implemented"
genPrim Sub v a b = error "Mul is not yet implemented"

rk :: MonadState Env m => Arg -> m (Rk UniqRx)
rk (Var v) = R <$> varRx v
rk (Arg a) = R <$> argRx a
rk (Const k) = K <$> defineConstant k

argRx :: MonadState Env m => Int -> m UniqRx
argRx a = return $ UniqRx a

-- | Push an arg onto the stack in prep for a function call
pushArg :: MonadState Env m => Arg -> m (Instruction UniqRx)
pushArg arg = do
  -- TODO: This could be a problem if this has the side effect of allocationg another reg?
  top <- incTop
  move top arg

moveVar :: MonadState Env m => Variable -> Arg -> m (Instruction UniqRx)
moveVar v a = do
  rx <- varRx v
  move rx a

-- | Move an Arg into a Register
move :: MonadState Env m => UniqRx -> Arg -> m (Instruction UniqRx)
-- If it's a function Arg, it's already in R(arg_index)
move rx arg = do
  rkArg <- rk arg
  return $ OP_MOVE rx rkArg

-- | return the current stack top, and increment it in the state
incTop :: MonadState Env m => m UniqRx
incTop = genRx

{-
rk :: Arg -> m Int
-- | Function arguments are already register indices
rk (Arg a) = return a
-- | If it's a var, we need to know which register
rk (Var v) = varRx v
-- | Constant values are encoded with the top bit set
-- TODO: If constant is too big:
--   emit a constant for the function table
--   emit a LOAD_K and then define an arg
rk (Const k) = return $ k .|. constantBit
  where constantBit = 256
  -}

-- | Return the index in the function
-- TODO optimize to look up if constant already exists, and then alias that
defineConstant :: MonadState Env m => Int -> m Constant
defineConstant kvalue = do
  c <- genConst
  state <- get
  let cs = Map.insert c kvalue (constants state)
  put $ state { constants = cs }
  return c

genConst :: MonadState Env m => m Constant
genConst = do
  state <- get
  let t = topConstant $ state
  put $ state { topConstant = t + 1 }
  return $ Constant t
