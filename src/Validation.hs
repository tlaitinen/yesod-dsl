module Validation (validate) where

import AST
import Data.List
import Validation.Names
import Validation.Refs
import Validation.Fields
import Validation.Handlers
validate :: Module -> String
validate m = nameErrors ns ++ refErrors ns (refs m) ++ fieldErrors m ns 
           ++ handlerErrors m
           
    where ns = names m
