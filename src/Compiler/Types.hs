module Compiler.Types where

import Control.Lens (Lens', lens)
import Data.Map (Map)
import qualified Data.Map as Map

import qualified IIR as IIR
import IIR (IIR)
import Lang.Types (Ident)

-- Compiler environment
data Env = Env { vars :: Map Ident IIR.Arg
               , top :: Int
               , insts :: [IIR]
               , protos :: [IIR.Function]
               }

_vars :: Lens' Env (Map Ident IIR.Arg)
_vars = lens vars $ \env vs -> env { vars = vs }

_top :: Lens' Env Int
_top = lens top $ \env t -> env { top = t }

_insts :: Lens' Env [IIR]
_insts = lens insts $ \env is -> env { insts = is }

_protos :: Lens' Env [IIR.Function]
_protos = lens protos $ \env ps -> env { protos = ps }

emptyEnv :: Env
emptyEnv = Env { vars = Map.empty
               , top = 0
               , insts = []
               , protos = []
               }

-- Monad in which Compilation effects can take place
class Monad m => MonadCompile m where
  -- Emit an IIR instruction which will present in the compiled code
  emit :: IIR -> m ()

  -- Resolve the binding for variable `Ident`
  resolve :: Ident -> m IIR.Arg

  -- Allocate an unbound variable
  allocate :: m IIR.Variable

  -- Nest a function prototype under this one
  -- Returns the index of the proto in the function
  proto :: Ident -> m () -> m Int
