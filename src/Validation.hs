module Validation (validate) where

import AST
import Data.List
import Validation.Fields
import Validation.Handlers
import qualified Validation.State as V
import Data.Maybe

validate :: Module -> String
validate m = fieldErrors m  
           ++ handlerErrors m
           ++ missingModuleName 
           ++ (intercalate "\n" $ V.validate m)
    where 
          missingModuleName 
            | isNothing $ modName m = "Top-level module missing name"
            | otherwise = ""
