module YesodDsl.Validation (validate) where

import YesodDsl.AST
import Data.List
import YesodDsl.Validation.Fields
import YesodDsl.Validation.Handlers
import qualified YesodDsl.Validation.State as V
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
