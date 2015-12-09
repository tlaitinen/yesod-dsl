{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator.HsClient (moduleToHsClient) where
import YesodDsl.AST
import Data.List
import Data.Maybe
import Data.Char (toLower)
import Data.String.Utils (rstrip)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Shakespeare.Text hiding (toText)
import Data.Generics.Uniplate.Data
import YesodDsl.Generator.Common
import YesodDsl.Generator.Input
import YesodDsl.Generator.Models
import YesodDsl.Generator.Client
import System.FilePath (joinPath)
  
mkField :: FieldName -> (Bool,FieldContent) -> Field
mkField n (o,c) = Field (Loc "" 0 0) o n c [] Nothing

moduleToHsClient :: Module -> [(FilePath, String)]
moduleToHsClient m = [ 
        (baseName ++".hs", T.unpack $(codegenFile "codegen/hs-client.cg")),
        (joinPath [baseName, "Result.hs"], T.unpack $(codegenFile "codegen/hs-client-result.cg")),
        (joinPath [baseName, "Types.hs"], T.unpack $(codegenFile "codegen/hs-client-types.cg")),
        (joinPath [baseName, "Json.hs"], T.unpack $(codegenFile "codegen/hs-client-json.cg")),
        (joinPath [baseName, "Enums.hs"], T.unpack $(codegenFile "codegen/hs-client-enums.cg"))

        
    ] ++ [ (handlerFileName r h,  handler r h) 
            | r <- modRoutes m, h <- routeHandlers r ]
      ++ [ (enumFileName e, enum e) | e <- modEnums m ]
    where
        importEnum e = T.unpack $(codegenFile "codegen/hs-client-import-enum.cg")
        exportEnum e = T.unpack $(codegenFile "codegen/hs-client-export-enum.cg")
        entityIdType e = T.unpack $(codegenFile "codegen/hs-client-idtypes-entity.cg")
        baseName = moduleName m ++ "Client"
        handlerFileName r h = joinPath [ baseName, (upperFirst . (map toLower) . show . handlerType) h ++ concatMap pathName (routePath r)   ++ ".hs"]
        enumFileName e = joinPath [ baseName, enumName e  ++ ".hs" ]
        enum e = T.unpack $(codegenFile "codegen/hs-client-enum.cg")
        handler r h 
            | handlerType h == GetHandler = T.unpack $(codegenFile "codegen/hs-client-handler-get.cg")
            | null $ handlerInputFields h =T.unpack $(codegenFile "codegen/hs-client-handler-update-empty-body.cg")
            | otherwise = T.unpack $(codegenFile "codegen/hs-client-handler-update.cg")
            where
                ifBodyAllowed content 
                    | handlerType h `elem` [PutHandler, PostHandler] = content :: String
                    | otherwise = ""
                fieldLabelModifier (src, dst) = T.unpack $(codegenFile "codegen/hs-client-field-label-modifier.cg")
                fieldLabelModifiers = [ (safeHsName $ fieldJsonName f, fieldJsonName f) | f <- handlerOutputFields m h, safeHsName (fieldJsonName f) /= fieldJsonName f ] 
                maybeFieldLabelModifier
                    | null fieldLabelModifiers = ""
                    | otherwise = T.unpack $(codegenFile "codegen/hs-client-field-label-modifiers.cg")
                methodName = map toLower $ show $ handlerType h
                defineResultType
                    | null $ handlerOutputFields m h = "type Result = A.Value" :: String
                    | otherwise = T.unpack $(codegenFile "codegen/hs-client-handler-update-result-type.cg")
                field f  = rstrip $ T.unpack $(codegenFile "codegen/hs-client-field.cg")

                handlerTypeName = upperFirst $ map toLower (show $ handlerType h) 
                handlerEntityName = handlerTypeName ++ concatMap pathName (routePath r) 
        pathName pp = case pp of
            PathText t -> upperFirst t
            PathId _ en -> "_"
         
        routePathParams r = mapMaybe (\(n,pp) -> case pp of
            PathText _ -> Nothing
            PathId _ en -> Just ("(Key p" ++ show (n::Int) ++ ")",en ++ "Id")) $ zip [1..] (routePath r)
        routePathUrl r = concatMap (\(n,pp) -> case pp of
            PathText t -> " ++ \"/" ++ t ++ "\""
            PathId _ _ -> " ++ \"/\" ++ show p" ++ show (n::Int)) $ zip [1..] (routePath r)
        
        inputField (fn,Just f) = rstrip $ T.unpack $(codegenFile "codegen/hs-client-inputfield.cg")
        inputField (fn,Nothing) = rstrip $ T.unpack $(codegenFile "codegen/hs-client-inputfield-unknown.cg")
        entity e = T.unpack $(codegenFile "codegen/hs-client-entity.cg")
