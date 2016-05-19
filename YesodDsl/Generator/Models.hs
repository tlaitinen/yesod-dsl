{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator.Models where
import YesodDsl.AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import YesodDsl.Generator.Common

boolToMaybe :: Bool -> String
boolToMaybe True = "Maybe "
boolToMaybe False = ""


hsFieldType :: Field -> String
hsFieldType f = (boolToMaybe . fieldOptional) f
              ++ baseFieldType f
fieldTypeToHsType :: FieldType -> String
fieldTypeToHsType ft = case ft of
    FTWord32 -> "Word32"
    FTWord64 -> "Word64"
    FTInt32 -> "Int32"
    FTInt -> "Int"
    FTInt64 -> "Int64"
    FTText -> "Text"
    FTBool -> "Bool"
    FTDouble -> "Double"
    FTRational -> "Rational"
    FTTimeOfDay -> "TimeOfDay"
    FTDay -> "Day"
    FTUTCTime -> "UTCTime"
    FTCheckmark -> "Checkmark"


baseFieldType :: Field -> String
baseFieldType f = case fieldContent f of
    (NormalField ft) -> fieldTypeToHsType ft
    (EntityField en) -> en ++ "Id"
    (EnumField en) -> en


persistFieldType :: Field -> String
persistFieldType f = baseFieldType f 
                   ++ " " ++ (boolToMaybe . fieldOptional) f
                   ++ (maybeDefault . fieldDefault) f
                   ++ (maybeDefaultNull f)
                   ++ (maybeCheckmarkNullable $ fieldContent f)
                   ++ (maybeColumnName . fieldColumnName) f 
                   ++ (maybeColumnType . fieldColumnType) f
    where 
          maybeDefault (Just d) = " \"default=" ++ (fieldValueToSql d)  ++ "\""
          maybeDefault _ = " "
          maybeDefaultNull (Field _ True _ (EntityField _) _ _) = " default=NULL"
          maybeDefaultNull _ = ""
          maybeCheckmarkNullable (NormalField FTCheckmark) = " nullable"
          maybeCheckmarkNullable _ = ""
          maybeColumnName (Just cn) =" \"sql=" ++ cn ++ "\""
          maybeColumnName Nothing = ""
          maybeColumnType (Just ct) =" \"sqltype=" ++ ct ++ "\""
          maybeColumnType Nothing = ""




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
          readsPrecs = concatMap readsPrec' (enumValues e)
          showsPrecs = concatMap showsPrec' (enumValues e)
          readsPrec' v = T.unpack $(codegenFile "codegen/enum-readsprec.cg")
          showsPrec' v = T.unpack $(codegenFile "codegen/enum-showsprec.cg")
          toCharList s = "'" ++ (intercalate "':'" (map (:[]) s)) ++ "'"
          prefixedValues  = intercalate " | " $ map ((enumName e) ++) $ enumValues e

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
    where
        maybeEntityTableName
            | isJust $ entityTable e = " \"sql=" ++ (fromJust $ entityTable e) ++ "\""
            | otherwise = ""
models :: Module -> String
models m = T.unpack $(codegenFile "codegen/models-header.cg")
         ++ (concatMap model (modEntities m))
         ++ (T.unpack $(codegenFile "codegen/models-footer.cg"))


