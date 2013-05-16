{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Classes where
import AST
import AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import Generator.Models

classFieldName :: Class -> Field -> String
classFieldName i f = (lowerFirst . className) i ++ (upperFirst . fieldName) f

classDefField :: Class -> Field -> String
classDefField c f = T.unpack $(codegenFile "codegen/class-field.cg")

classInstanceField :: Class -> Entity -> Field -> String
classInstanceField c e f = T.unpack $(codegenFile "codegen/class-instance-field.cg")

classInstance :: Class -> Entity -> String
classInstance c e = T.unpack $(codegenFile "codegen/class-instance-header.cg")
                  ++ (concatMap (classInstanceField c e) (classFields c))

classInstances :: Module -> Class -> String
classInstances m c = T.unpack $(codegenFile "codegen/class-header.cg")
                   ++ (concatMap (classDefField c) (classFields c))
                   ++ (concatMap (classInstance c) 
                                 [ e | e <- modEntities m, 
                                  (className c) `elem` (entityInstances e)])
                                                         

classes :: Module -> String
classes m = concatMap (classInstances m) (modClasses m)


