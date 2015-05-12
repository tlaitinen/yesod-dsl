{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator.Validation where
import YesodDsl.AST
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import Data.String.Utils (rstrip)
import YesodDsl.Generator.Common

validationFieldCheck :: Entity -> Field -> FunctionName -> String
validationFieldCheck e f func = rstrip $ T.unpack $(codegenFile "codegen/validation-field.cg")

validationEntityCheck :: Entity -> FunctionName -> String
validationEntityCheck e func = rstrip $ T.unpack $(codegenFile "codegen/validation-entity.cg")

validationEntity :: Entity -> String
validationEntity e = T.unpack $(codegenFile "codegen/validation-entity-header.cg")
                   ++ (intercalate ",\n " $ [ validationFieldCheck e f func
                                          | f <- entityFields e,
                                            func <- fieldChecks f])
                   ++ (intercalate ",\n " $ [ validationEntityCheck e func |
                                              func <- entityChecks e ])
                   ++ (T.unpack $(codegenFile "codegen/validation-entity-footer.cg"))


 
