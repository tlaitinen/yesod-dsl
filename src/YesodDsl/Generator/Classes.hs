{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module YesodDsl.Generator.Classes where
import YesodDsl.AST
import YesodDsl.AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import YesodDsl.Generator.Models
import YesodDsl.Generator.Common
import Data.String.Utils (rstrip)
classFieldName :: Class -> Field -> String
classFieldName i f = (lowerFirst . className) i ++ (upperFirst . fieldName) f

classDefField :: Class -> Field -> String
classDefField c f = T.unpack $(codegenFile "codegen/class-field.cg")

classFieldTypeName :: Class -> Field -> String
classFieldTypeName c f= rstrip $ T.unpack $(codegenFile "codegen/class-field-type-field-name.cg")

classFieldType :: Class -> [Field] -> String
classFieldType c fs = if null fs
    then ""
    else T.unpack $(codegenFile "codegen/class-field-type.cg")

classInstanceField :: Class -> Entity -> Field -> String
classInstanceField c e f = T.unpack $(codegenFile "codegen/class-instance-field.cg")

classInstance :: Class -> Entity -> String
classInstance c e = T.unpack $(codegenFile "codegen/class-instance-header.cg")
                  ++ (concatMap (classInstanceField c e) (classFields c))

classEntityInstanceField :: Class -> [Entity] -> Field -> String
classEntityInstanceField c es f = T.unpack $(codegenFile "codegen/class-entity-instance-field.cg")
    where caseEntity e = T.unpack $(codegenFile "codegen/class-entity-instance-field-entity.cg")


classEntityInstances :: Class -> [Entity] -> String
classEntityInstances c es = T.unpack $(codegenFile "codegen/class-entity-instances.cg")
    ++ (concatMap (classEntityInstanceField c es) (classFields c))
    where entityInstance e = T.unpack $(codegenFile "codegen/class-entity-instance.cg")

classSelectFilterDataType :: Class -> String
classSelectFilterDataType c = T.unpack $(codegenFile "codegen/class-select-filter-data-type.cg")
    where 
        fieldFilterDataType f = rstrip $ T.unpack $(codegenFile "codegen/class-select-filter-data-type-field.cg")

classSelect :: Class -> [Entity] -> String
classSelect c es = maybeFilterDataType 
    ++ T.unpack $(codegenFile "codegen/class-select.cg")
    where 
        selectEntity e = T.unpack $(codegenFile "codegen/class-select-entity.cg")
        wrapResult e = T.unpack $(codegenFile "codegen/class-select-result.cg")
        hasClassFields = not . null $ classFields c
        maybeFilter e = if hasClassFields
            then T.unpack $(codegenFile "codegen/class-select-entity-filter.cg")
            else ""
        filterField e f = T.unpack $(codegenFile "codegen/class-select-entity-filter-field.cg")
        maybeFilterDataType = if hasClassFields 
            then classSelectFilterDataType c
            else ""
        maybeFilterParam = if hasClassFields then "filters" :: String else ""
        maybeFilterType = if hasClassFields then rstrip $ T.unpack $(codegenFile "codegen/class-select-filter-type.cg") else ""
        


classInstances :: Module -> Class -> String
classInstances m c = T.unpack $(codegenFile "codegen/class-header.cg")
                   ++ (concatMap (classDefField c) (classFields c))
                   ++ (classFieldType c $ classFields c)
                   ++ (concatMap (classInstance c) (instancesOf c))
                   ++ (classEntityInstances c (instancesOf c))
                   ++ (classSelect c $ instancesOf c)
    where 
        instancesOf c = [ e | e <- modEntities m, 
                         (className c) `elem` (entityInstances e)]

                                           
                                                         

classes :: Module -> String
classes m = 
    (concatMap (classInstances m) (modClasses m))
    

