module IIR where

-- A simple Imperative Intermediate Representation (IIR), using a Single Static
-- Assignemnt (SSA) form. This forms the target for compilation from the high
-- level language, and itself is compiled to LUA bytecode.

-- | An argument to a function or operation
data Arg
    -- | An index to the Function arguments
    = Arg Int -- TODO make this a newtype
    -- | A reference to a var in the enclosing environment
    | Var Variable
    -- | A constant value
    | Const Int
    deriving (Show)
    -- TODO - should a Proto be a type of Arg? Then we can use just 'Move'
    -- instead of needing the extra 'Proto' command in IIR

-- | A variable name
newtype Variable = Variable Int deriving (Show, Ord, Eq)

-- | A function prototype, containing nested prototypes for contained lambdas
data Function = Function { instructions :: [IIR]
                         , protos :: [Function]
                         }
  deriving Show

type Fn = Variable

data IIR
    -- | Call function `fn` with args `args`, store the result in `variable`
    -- | variable = fn args...
    = Call Variable Fn [Arg]
    -- | Return value `arg` from the enclosing function
    | Return Arg
    -- | Execute primitive binary op with two args, and store in Dest
    -- | variable = arg `binOp` arg
    | Prim BinOp Variable Arg Arg
    -- | Store in `variable` the value from `Arg`
    | Move Variable Arg
    -- | Load nested Function Prototype `Proto` into `Variable`
    | Proto Variable Int
    deriving Show

data BinOp = Add | Mul | Sub -- TODO: add Div, Mod, Exp
  deriving Show


-- Examples:
--
--   function f(x, y)
--     z = x + y
--     return z
--   end
--
--   function g(x)
--     y = f(x, x)
--     return y
--   end
--
-- - becomes -
--
--  f:
--   Prim Add Var0 Arg0 Arg1
--   Return Var0
--
--  g:
--   Call 'f' Var0 [Arg0, Arg0]
--   Return Var0

