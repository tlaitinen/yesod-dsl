module Env (Env, emptyEnv, bindEnvVar, lookupEnvVar) where

import AST

import qualified Data.Map as Map
type VarMap = Map.Map VariableName (Entity, Location)
newtype Env = Env VarMap

emptyEnv :: Env
emptyEnv = Env Map.empty

bindEnvVar :: VariableName -> Entity -> Location -> Env -> Env
bindEnvVar v e l (Env m) = Env $ Map.insert v (e,l) m

lookupEnvVar :: VariableName -> Env -> Maybe (Entity, Location)
lookupEnvVar v (Env m) = Map.lookup v m



