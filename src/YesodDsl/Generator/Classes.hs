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
classDefField c cf = T.unpack $(codegenFile "codegen/class-field.cg")
    where
        f = case cf of
            Field _ _ _ _ (EntityField "ClassInstance") -> cf {
                fieldContent = EntityField (className c ++ "Instance")
            }
            _ -> cf
                  
        

classFieldTypeName :: Class -> Field -> String
classFieldTypeName c f= rstrip $ T.unpack $(codegenFile "codegen/class-field-type-field-name.cg")

classFieldType :: Class -> [Field] -> String
classFieldType c fs = if null fs
    then ""
    else T.unpack $(codegenFile "codegen/class-field-type.cg")

classInstanceField :: Class -> Entity -> Field -> String
classInstanceField c e f = T.unpack $(codegenFile "codegen/class-instance-field.cg")
    where
        mapper = case f of
            Field _ opt _ _ (EntityField "ClassInstance") -> 
                let base = className c ++ "Instance" ++ entityName e ++ "Id"
                    in if opt
                        then "(fmap " ++ base ++ ") . "
                        else base ++ " . "
            _ -> ""     

classInstance :: Class -> Entity -> String
classInstance c e = T.unpack $(codegenFile "codegen/class-instance-header.cg")
                  ++ (concatMap (classInstanceField c e) (classFields c))

classEntityInstanceField :: Class -> [Entity] -> Field -> String
classEntityInstanceField c es f = T.unpack $(codegenFile "codegen/class-entity-instance-field.cg")
    where   
        caseEntity e = T.unpack $(codegenFile "codegen/class-entity-instance-field-entity.cg")
        mapper e = case f of
            Field _ opt _ _ (EntityField "ClassInstance") -> 
                let base = className c ++ "Instance" ++ entityName e ++ "Id"
                    in if opt
                        then "(fmap " ++ base ++ ") $ "
                        else base ++ " $ "
            _ -> ""     
 


classEntityInstances :: Class -> [Entity] -> String
classEntityInstances c es = T.unpack $(codegenFile "codegen/class-entity-instances.cg")
    ++ (concatMap (classEntityInstanceField c es) (classFields c))
    where 
        entityInstance e = T.unpack $(codegenFile "codegen/class-entity-instance.cg")
        entityInstanceId e = T.unpack $(codegenFile "codegen/class-entity-instance-id.cg")

classSelectFilterDataType :: Class -> String
classSelectFilterDataType c = T.unpack $(codegenFile "codegen/class-select-filter-data-type.cg")
    where 
        fieldFilterDataType (Field _ _ _ _ (EntityField "ClassInstance")) = Nothing
        fieldFilterDataType f = Just $ rstrip $ T.unpack $(codegenFile "codegen/class-select-filter-data-type-field.cg")

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
        maybeFilterDataType = if hasClassFields 
            then classSelectFilterDataType c
            else ""
        maybeFilterParam = if hasClassFields then "filters" :: String else ""
        filterField e (Field _ _ _ _ (EntityField "ClassInstance")) = ""
        filterField e f = T.unpack $(codegenFile "codegen/class-select-entity-filter-field.cg")
        maybeFilterType = if hasClassFields then rstrip $ T.unpack $(codegenFile "codegen/class-select-filter-type.cg") else ""


classUpdate :: Class -> [Entity] -> String
classUpdate c es 
    | hasClassFields = T.unpack $(codegenFile "codegen/class-update-data-type.cg")
        ++ (T.unpack $(codegenFile "codegen/class-update.cg"))
    | otherwise = ""
    where
        hasClassFields = not . null $ classFields c
        updateEntity e = T.unpack $(codegenFile "codegen/class-update-entity.cg")
        fieldUpdateDataType (Field _ _ _ _ (EntityField "ClassInstance")) = Nothing
        fieldUpdateDataType f = Just $ rstrip $ T.unpack $(codegenFile "codegen/class-update-data-type-field.cg")
        updateEntityField _ (Field _ _ _ _ (EntityField "ClassInstance")) = ""
        updateEntityField e f = T.unpack $(codegenFile "codegen/class-update-entity-field.cg")
        maybeFilter e = if hasClassFields
            then T.unpack $(codegenFile "codegen/class-select-entity-filter.cg")
            else ""
        filterField e (Field _ _ _ _ (EntityField "ClassInstance")) = ""
        filterField e f = T.unpack $(codegenFile "codegen/class-select-entity-filter-field.cg")
        
instancesOf :: Module -> Class -> [Entity]
instancesOf m c = [ e | e <- modEntities m,  (className c) `elem` (entityInstances e)]


classInstances :: Module -> Class -> String
classInstances m c = T.unpack $(codegenFile "codegen/class-header.cg")
                   ++ (concatMap (classDefField c) (classFields c))
                   ++ (classFieldType c $ classFields c)
                   ++ (concatMap (classInstance c) (instancesOf m c))
                   ++ (classEntityInstances c (instancesOf m c))
                   ++ (classSelect c $ instancesOf m c)
                   ++ (classUpdate c $ instancesOf m c)

                                           
entityClassFieldWrappers :: Module -> Entity -> String
entityClassFieldWrappers m e = concatMap fieldWrapper $ entityClassFields e
    where
        fieldWrapper f@(Field _ _ _ _ (EntityField cName)) = case find (\c -> className c == cName) $ modClasses m of
            Just c ->  T.unpack $(codegenFile "codegen/entity-class-field-wrapper.cg")
            Nothing -> ""
        fieldWrapper _ = ""
        wrapInstance c f e2 = T.unpack $(codegenFile "codegen/entity-class-field-wrapper-wrap-instance.cg")
classes :: Module -> String
classes m = 
    (concatMap (classInstances m) (modClasses m))
    ++ (concatMap (entityClassFieldWrappers m) (modEntities m))
    

