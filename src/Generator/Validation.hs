{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Generator.Validation where
import AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import Data.String.Utils (rstrip)
import Generator.Models
import Generator.Common

validationFieldCheck :: Entity -> Field -> FunctionName -> String
validationFieldCheck e f func = rstrip $ T.unpack $(codegenFile "codegen/validation-field.cg")

validationEntityCheck :: Entity -> FunctionName -> String
validationEntityCheck e func = rstrip $ T.unpack $(codegenFile "codegen/validation-entity.cg")
    where fieldRef f = "(" ++ (lowerFirst . entityName) e ++ upperFirst f ++ " v)"

validationEntity :: Entity -> String
validationEntity e = T.unpack $(codegenFile "codegen/validation-entity-header.cg")
                   ++ (intercalate ",\n " $ [ validationFieldCheck e f func
                                          | f <- entityFields e,
                                            func <- fieldChecks f])
                   ++ (intercalate ",\n " $ [ validationEntityCheck e func |
                                              func <- entityChecks e ])
                   ++ (T.unpack $(codegenFile "codegen/validation-entity-footer.cg"))

type TypeName = String

validationFieldFunction :: (Field, FunctionName) -> String
validationFieldFunction (f,func) = T.unpack $(codegenFile "codegen/validation-function-field.cg")

validationEntityFunction :: (Entity, FunctionName) -> String
validationEntityFunction (e, func) = T.unpack $(codegenFile "codegen/validation-function-entity.cg")
    

lookupFieldType :: Module -> EntityName -> FieldName -> String
lookupFieldType m en fn = hsFieldType (fromJust $ lookupField m en fn)
validation :: Module -> String
validation m = T.unpack $(codegenFile "codegen/validation-header.cg")
             ++ (concatMap validationFieldFunction $ 
                    nubBy (\(_,f1) (_,f2) -> f1 == f2)
                    [ (f,func) | e <- modEntities m,
                             f <- entityFields e,
                             func <- fieldChecks f ])
             ++ (concatMap validationEntityFunction $ 
                   [ (e, func) | e <- modEntities m,   func <- entityChecks e ])
             ++ (concatMap validationEntity (modEntities m))


