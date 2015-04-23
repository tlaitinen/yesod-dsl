{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator.EntityFactories where
import YesodDsl.AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import YesodDsl.Generator.Common
import YesodDsl.Generator.Models (baseFieldType)

entityFactory :: Entity -> String
entityFactory e = T.unpack $(codegenFile "codegen/entity-factory.cg")
    where requiredFields = [ f | f <- entityFields e, 
                                 fieldOptional f == False, 
                                 isNothing $ fieldDefault f ]
          defaultFields = (entityFields e) \\ requiredFields
          fieldParamName f = fieldName f ++ "_"
          fieldSetter f = entityFieldName e f ++ " = " ++ value f
          value f 
            | f `elem` requiredFields = fieldParamName f
            | fieldOptional f = "Nothing"
            | otherwise = case fieldDefault f of
                    Just fv -> fieldValueToHs fv
                    Nothing -> error "missing default value in entityFactory"

entityFactories :: Module -> String
entityFactories m = concatMap entityFactory (modEntities m)


