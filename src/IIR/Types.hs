module IIR.Types where

import Data.Text (Text)

-- 'Simple' Values, e.g. those that have been evaluated and can be used as arguments
-- to instructions
data SVal = Reg Rx
          | Argument Arg
          | Const Constant
          | Prim PrimOp
          -- | Upvalue ???

-- Function Argument at a given index
data Arg = Arg Int

-- A given register address
data Rx = Rx Int

-- Types of constant values allowed
data Constant = KInt Int

-- A Function template that a Closure can be created from
data FnTemplate = FnTemplate Text

data PrimOp
  = ADD
  | MUL

data IIR =
  -- Create a closure from a given function template and closing over given upvalues, store result in Rx
  CLOSURE FnTemplate [SVal] Rx
  -- Call a given Closure with given arguments, store result in Rx
  -- TODO first arg should not be SVal - Or should it?
  | CALL SVal [SVal] Rx
  -- Return from this function the given `SVal`
  | RETURN SVal
  -- Execute a primitive binary op, store results in `Rx`
  | PRIM PrimOp SVal SVal Rx
