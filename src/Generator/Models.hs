{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Generator.Models where
import AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
recName :: String -> String -> String
recName dt f = lowerFirst dt ++ upperFirst f

persistFieldType :: Field -> String
persistFieldType f = baseFieldType f 
                   ++ " " ++ (boolToMaybe . fieldOptional) f
                   ++ (maybeDefault . fieldDefault) f
                   ++ (maybeDefaultNull f)
    where maybeDefault (Just d) = " default='" ++ stripQuotes (show d) ++ "'"
          maybeDefault _ = " "
          stripQuotes ('"':xs)  = take ((length xs) -1) xs
          stripQuotes xs = xs
          maybeDefaultNull (Field True _ (EntityField _)) = " default='NULL'"
          maybeDefaultNull _ = ""



entityFieldName :: Entity -> Field -> String
entityFieldName e f = (lowerFirst . entityName) e ++ (upperFirst . fieldName) f
entityFieldTypeName :: Entity -> Field -> String
entityFieldTypeName e f = upperFirst $ entityFieldName e f
enum :: EnumType -> String
enum e = T.unpack $(codegenFile "codegen/enum.cg")
    where fromPathPieces = concatMap fromPathPiece (enumValues e) 
          toPathPieces = concatMap toPathPiece (enumValues e)
          parseJSONs = concatMap parseJSON (enumValues e)
          toJSONs = concatMap toJSON (enumValues e)
          fromPathPiece v = T.unpack $(codegenFile "codegen/enum-frompathpiece.cg")
          toPathPiece v = T.unpack $(codegenFile "codegen/enum-topathpiece.cg")
          parseJSON v = T.unpack $(codegenFile "codegen/enum-parsejson.cg")
          toJSON v = T.unpack $(codegenFile "codegen/enum-tojson.cg")
          readsPrecs = concatMap readsPrec (enumValues e)
          showsPrecs = concatMap showsPrec (enumValues e)
          readsPrec v = T.unpack $(codegenFile "codegen/enum-readsprec.cg")
          showsPrec v = T.unpack $(codegenFile "codegen/enum-showsprec.cg")
          toCharList s = "'" ++ (intercalate "':'" (map (:[]) s)) ++ "'"
          prefixedValues e = intercalate " | " $ map ((enumName e) ++) $ enumValues e

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


