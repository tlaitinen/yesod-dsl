module Validation (validate) where

import AST
import Data.List
import Validation.Names
import Validation.Refs

validate :: Module -> String
validate m = nameErrors ns ++ refErrors ns (refs m)
    where ns = names m
