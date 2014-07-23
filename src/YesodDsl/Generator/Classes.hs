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

classFieldName :: Class -> Field -> String
classFieldName i f = (lowerFirst . className) i ++ (upperFirst . fieldName) f

classDefField :: Class -> Field -> String
classDefField c f = T.unpack $(codegenFile "codegen/class-field.cg")

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


classInstances :: Module -> Class -> String
classInstances m c = T.unpack $(codegenFile "codegen/class-header.cg")
                   ++ (concatMap (classDefField c) (classFields c))
                   ++ (concatMap (classInstance c) (instancesOf c))
                   ++ (classEntityInstances c (instancesOf c))
    where 
        instancesOf c = [ e | e <- modEntities m, 
                         (className c) `elem` (entityInstances e)]

                                           
                                                         

classes :: Module -> String
classes m = concatMap (classInstances m) (modClasses m)


