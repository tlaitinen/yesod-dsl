{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Generator (generate) where

import System.IO (FilePath)
import AST
import Text.Shakespeare.Text hiding (toText)
import qualified Data.Text as T
import Data.List
import Data.Maybe

import Data.Char
recName :: String -> String -> String
recName dt f = lowerFirst dt ++ upperFirst f

lowerFirst :: String -> String
lowerFirst (a:b) = (toLower a):b
lowerFirst a = a

upperFirst :: String -> String
upperFirst (a:b) = (toUpper a):b
upperFirst a = a

baseFieldType :: Field -> String
baseFieldType f = case fieldContent f of
    (NormalField ft _) -> show ft
    (EntityField en) -> en ++ "Id"

boolToMaybe :: Bool -> String
boolToMaybe True = "Maybe "
boolToMaybe False = ""

hsFieldType :: Field -> String
hsFieldType f = (boolToMaybe . fieldOptional) f
              ++ baseFieldType f

persistFieldType :: Field -> String
persistFieldType f = baseFieldType f 
                   ++ " " ++ (boolToMaybe . fieldOptional) f
                   ++ (maybeDefault . fieldDefault) f
    where maybeDefault (Just d) = " default='" ++ show d ++ "'"
          maybeDefault _ = " "
classFieldName :: Class -> Field -> String
classFieldName i f = (lowerFirst . className) i ++ (upperFirst . fieldName) f

entityFieldName :: Entity -> Field -> String
entityFieldName e f = (lowerFirst . entityName) e ++ (upperFirst . fieldName) f

enum :: EnumType -> String
enum e = T.unpack $(codegenFile "codegen/enum.cg")

modelField :: Field -> String
modelField f = T.unpack $(codegenFile "codegen/model-field.cg")

modelUnique :: Unique -> String
modelUnique (Unique name fields) = T.unpack $(codegenFile "codegen/model-unique.cg")

modelDeriving :: String -> String
modelDeriving d = T.unpack $(codegenFile "codegen/model-deriving.cg")

model :: Entity -> String
model e = T.unpack $(codegenFile "codegen/model-header.cg")
        ++ (concatMap modelField (entityFields e))
        ++ (concatMap modelUnique (entityUniques e)) 
        ++ (concatMap modelDeriving (entityDeriving e))

models :: Module -> String
models m = T.unpack $(codegenFile "codegen/models-header.cg")
         ++ (concatMap model (modEntities m))
         ++ (T.unpack $(codegenFile "codegen/models-footer.cg"))

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

hsRouteName :: [PathPiece] -> String
hsRouteName = f . routeName 
    where f ('/':x:xs) = toUpper x : f xs
          f ('#':xs) = f xs
          f (x:xs) = x : f xs
          f [] = "R"

hsRouteType :: [PathPiece] -> String
hsRouteType = (intercalate " ") . (mapMaybe toType)
    where toType (PathText _) = Nothing
          toType (PathId en) = Just $ en ++ "Id -> "

routeResource :: Resource -> String
routeResource r = T.unpack $(codegenFile "codegen/route.cg")
    where handlers = intercalate " " (map (show . handlerType) (resHandlers r))

routes :: Module -> String
routes m = T.unpack $(codegenFile "codegen/routes-header.cg")
         ++ (concatMap routeResource (modResources m))
         ++ (T.unpack $(codegenFile "codegen/routes-footer.cg"))

validationFieldCheck :: Entity -> Field -> FunctionName -> String
validationFieldCheck e f func = T.unpack $(codegenFile "codegen/validation-field.cg")

validationEntityCheck :: Entity -> FunctionName -> String
validationEntityCheck e func = T.unpack $(codegenFile "codegen/validation-entity.cg")

validationEntity :: Entity -> String
validationEntity e = T.unpack $(codegenFile "codegen/validation-entity-header.cg")
                   ++ (intercalate ", " $ [ validationFieldCheck e f func 
                                          | f <- entityFields e, 
                                            func <- fieldChecks f ]
                                          ++ [ validationEntityCheck e func
                                              | func <- entityChecks e ])
                   ++ (T.unpack $(codegenFile "codegen/validation-entity-footer.cg"))


validationFunction :: FunctionName -> String
validationFunction func = T.unpack $(codegenFile "codegen/validation-function.cg")

validation :: Module -> String
validation m = T.unpack $(codegenFile "codegen/validation-header.cg")
             ++ (concatMap validationFunction $ nub $ 
                    [ func | e <- modEntities m, f <- entityFields e,
                             func <- fieldChecks f]
                  ++ [ func | e <- modEntities m, func <- entityChecks e ])
             ++ (concatMap validationEntity (modEntities m))

hsRouteParams :: [PathPiece] -> String
hsRouteParams ps = intercalate " " [("p" ++ show x) | 
                                    x <- [1..length (filter hasType ps)]]
    where hasType (PathId _) = True
          hasType _ = False

hsHandlerMethod :: HandlerType -> String          
hsHandlerMethod GetHandler    = "get"
hsHandlerMethod PutHandler    = "put"
hsHandlerMethod PostHandler   = "post"
hsHandlerMethod DeleteHandler = "delete"

handler :: Module -> Resource -> Handler -> String
handler m r (Handler ht ps) = T.unpack $(codegenFile "codegen/handler-header.cg")

generate :: Module -> String
generate m = T.unpack $(codegenFile "codegen/header.cg")
         ++ (concatMap enum $ modEnums m)
         ++ models m
         ++ classes m
         ++ routes m
         ++ validation m
         ++ (T.unpack $(codegenFile "codegen/json-wrapper.cg"))
         ++ (concat [ handler m r h | r <- modResources m, h <- resHandlers r ])
                            



