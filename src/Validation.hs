module Validation (validate) where

import AST
import Data.List
import Validation.Names
import Validation.Refs
import Validation.Fields
import Validation.Handlers
import Data.Maybe

validate :: Module -> String
validate m = nameErrors ns ++ refErrors ns (refs m) ++ fieldErrors m ns 
           ++ handlerErrors m
           ++ missingModuleName 
    where ns = names m
          missingModuleName 
            | isNothing $ modName m = "Top-level module missing name"
            | otherwise = ""
