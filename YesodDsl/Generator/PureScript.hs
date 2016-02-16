{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator.PureScript (moduleToPureScript) where
import YesodDsl.AST
import Data.List
import Data.Maybe
import Data.Char (toLower)
import Data.String.Utils (rstrip, replace)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Shakespeare.Text hiding (toText)
import Data.Generics.Uniplate.Data
import YesodDsl.Generator.Common
import YesodDsl.Generator.Input
import YesodDsl.Generator.Client

fieldValueToPureScript :: FieldValue -> String
fieldValueToPureScript fv = case fv of
    StringValue s -> "\"" ++ s ++ "\""
    IntValue i -> show i
    FloatValue d -> show d
    BoolValue b -> if b then "true" else "false"
    NothingValue -> "Nothing"
    CheckmarkValue Active -> "Active"
    CheckmarkValue Inactive -> "Inactive"
    EnumFieldValue en ev ->  en ++ ev
    EmptyList -> "[]"


requiredParamName :: String -> String
requiredParamName fn = "x.\"" ++ fn ++ "\""

pureScriptDefaultValue :: Field -> String
pureScriptDefaultValue f = case fieldDefault f of
    Just v -> (if fieldOptional f then "Just $ " else "") 
        ++ fieldValueToPureScript v
    Nothing -> if fieldOptional f then "Nothing" else requiredParamName (fieldJsonName f)
            
pureScriptFieldType :: Field -> String
pureScriptFieldType f = (if fieldOptional f then "Maybe " else "") 
    ++ case fieldContent f of
        NormalField ft -> case ft of
            FTWord32 -> "Int"
            FTWord64 -> "BigIntP"
            FTInt32 -> "Int"
            FTInt -> "BigIntP"
            FTInt64 -> "BigIntP"
            FTText -> "String"
            FTBool -> "Boolean"
            FTDouble -> "Number"
            FTRational -> "Number" -- TODO: Data.Rational
            FTTimeOfDay -> "TimeOfDay"
            FTDay -> "Day"           
            FTUTCTime -> "UTCTime"
            FTCheckmark -> "Boolean"
        EntityField en -> en ++ "Id"
        EnumField en -> en


moduleToPureScript :: Module -> String
moduleToPureScript m = T.unpack $(codegenFile "codegen/purescript.cg")
    where
        exportRouteModule r = rstrip $ T.unpack $(codegenFile "codegen/purescript-export-route.cg")
        importRouteModule r = T.unpack $(codegenFile "codegen/purescript-import-route.cg")
        enum e = T.unpack $(codegenFile "codegen/purescript-enum.cg")
            where
                value v = enumName e ++ v
                showValue v = T.unpack $(codegenFile "codegen/purescript-enum-show.cg")
                decodeValue v = rstrip $ T.unpack $(codegenFile "codegen/purescript-enum-decodevalue.cg")
                encodeValue v = T.unpack $(codegenFile "codegen/purescript-enum-encodevalue.cg")

        entity e = T.unpack $(codegenFile "codegen/purescript-entity.cg")

routeModuleNameP :: Route -> String
routeModuleNameP = routePathNameP

routePathNameP :: Route -> String
routePathNameP r = (concatMap pathName) $ routePath r

pathName :: PathPiece -> String
pathName pp = case pp of
    PathText t -> upperFirst t
    PathId _ en -> "I"

route :: Module -> Route -> String
route m r = T.unpack $(codegenFile "codegen/purescript-route.cg")        
    where
        routeModuleName = routeModuleNameP r
        routePathName = routePathNameP r
        handlerBaseName h = handlerTypeName h ++ routeModuleName
        handlerQueryString h = case handlerType h of
            GetHandler -> rstrip $ T.unpack $(codegenFile "codegen/purescript-get-handler-query-string.cg")
            _ -> ""
        handlerContent h = case handlerType h of
            GetHandler -> "Nothing"
            _ -> rstrip $ T.unpack $(codegenFile "codegen/purescript-update-handler-content.cg")
        exportHandler h = rstrip $ T.unpack $(codegenFile "codegen/purescript-export-handler.cg")
        importHandler h = T.unpack $(codegenFile "codegen/purescript-import-handler.cg")
        handlerRequest h = T.unpack $(codegenFile "codegen/purescript-handler-request.cg")
        handlerParseResponse h 
          | null $ handlerOutputFields m h = T.unpack $(codegenFile "codegen/purescript-handler-empty-response.cg")
          | otherwise = T.unpack $(codegenFile "codegen/purescript-handler-parse-response.cg")
        handlerRequestDataType h = T.unpack $(codegenFile "codegen/purescript-handler-request-data-type.cg")
            where
                resultType = case handlerType h of
                    GetHandler -> "(Result " ++ handlerEntityName h ++ ") -> a"
                    _ -> updateHandlerResultType h

        updateHandlerResultType h
            | null $ handlerOutputFields m h = "a"
            | otherwise = handlerEntityName h ++ "Result -> a"
        handlerModuleName = handlerTypeName

        handlerEntityName h = (if handlerType h /= GetHandler then handlerTypeName h else "") ++ routePathName
        handlerTypeName h = upperFirst $ map toLower (show $ handlerType h) 
        choose :: Bool -> String -> String -> String 
        choose cond t e 
            | cond = t
            | otherwise = e
        handler h 
            | handlerType h == GetHandler = T.unpack $(codegenFile "codegen/purescript-handler-get.cg")
            | otherwise = T.unpack $(codegenFile "codegen/purescript-handler-update.cg")
            where

                defineResultType
                    | null $ handlerOutputFields m h = ""
                    | otherwise = T.unpack $(codegenFile "codegen/purescript-handler-update-result-type.cg")

                defineHandlerParams h = T.unpack $(codegenFile "codegen/purescript-handler-params.cg")
                processResult 
                    | null $ handlerOutputFields m h = T.unpack $(codegenFile "codegen/purescript-handler-update-boolean-result.cg")
                    | otherwise = T.unpack $(codegenFile "codegen/purescript-handler-update-process-result.cg")
                field f  = rstrip $ T.unpack $(codegenFile "codegen/purescript-field.cg")
                encodeJson (fn,Right f) = rstrip $ T.unpack $(codegenFile "codegen/purescript-encodejson-field.cg")
                encodeJson (fn,Left _) = rstrip $ T.unpack $(codegenFile "codegen/purescript-encodejson-unknown.cg")

                decodeJsonExtract f = T.unpack $(codegenFile "codegen/purescript-decodejson-extract.cg")
                decodeJsonAssign f = rstrip $ T.unpack $(codegenFile "codegen/purescript-decodejson-assign.cg")


        defaultInputField (fn, Right f) = rstrip $ T.unpack $(codegenFile "codegen/purescript-inputfield-default.cg")
        defaultInputField (fn, Left optional) = rstrip $ T.unpack $(codegenFile "codegen/purescript-inputfield-unknown-default.cg")

        requiredInputFields h = filter (\x -> case x of (_, Right f) -> not (fieldOptional f) && isNothing (fieldDefault f) ; (_, Left optional) -> not optional) $ handlerInputFields h
        maybeRequiredParamsType h 
            | null $ requiredInputFields h = ""
            | otherwise = rstrip (T.unpack $(codegenFile "codegen/purescript-handler-required-params-type.cg")) ++ " "
        inputField (fn,Right f) = rstrip $ T.unpack $(codegenFile "codegen/purescript-inputfield.cg")
        inputField (fn,Left optional) = rstrip $ T.unpack $(codegenFile "codegen/purescript-inputfield-unknown.cg")
        toURIQuery (fn, Right f) = T.unpack $(codegenFile "codegen/purescript-toqueryparam.cg")
        toURIQuery (fn, Left optional) = T.unpack $(codegenFile "codegen/purescript-toqueryparam-unknown.cg")
        maybeMaybe x = if x then "Maybe " else "" :: Text
        routePathParams = mapMaybe (\(n,pp) -> case pp of
            PathText _ -> Nothing
            PathId _ en -> Just ("p" ++ show (n::Int),en ++ "Id")) $ zip [1..] (routePath r)
        routePathUrl = intercalate " ++ " $ map (\(n,pp) -> case pp of
            PathText t -> "\"/" ++ t ++ "\""
            PathId _ _ -> "\"/\" ++ show p" ++ show (n::Int)) $ zip [1..] (routePath r)
        
