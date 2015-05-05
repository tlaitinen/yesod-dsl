{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator.Json (moduleToJson) where
import YesodDsl.AST
import Text.Shakespeare.Text hiding (toText)
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Data.Char
import YesodDsl.Generator.Common
import Data.Aeson.Encode.Pretty
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Text.Lazy.Encoding as LTE
import YesodDsl.Generator.Input

import qualified Data.Text.Lazy as LT
moduleToJson :: Module -> String
moduleToJson m = LT.unpack $ LTE.decodeUtf8 $ encodePretty $ object [
        "name" .= moduleName m,
        "classes" .= [
            object [
                "name" .= className c,
                "fields" .= [ fieldJson f | f <- classFields c, fieldInternal f == False ]
            ] | c <- modClasses m
        ],
        "entities" .= [
            object [
                "name" .= entityName e,
                "fields" .= [ fieldJson f | f <- entityFields e, fieldInternal f == False ]
            ] | e <- modEntities m
        ],
        "enums" .= [
            object [
                "name" .= enumName e,
                "values" .= enumValues e 
            ] | e <- modEnums m
        ],
        "routes" .= [
            object [
                "path" .= [
                    case pp of
                        PathText s -> object [
                                            "type" .= ("string" :: String),
                                            "value" .= s
                                        ]  
                        PathId _ en -> object [
                                            "type" .= ("entity-id" :: String),
                                            "name" .= en
                                        ]                 
                    | pp <- routePath r
                ],
                "handlers" .= [
                    object [
                        "public" .= (Public `elem` (handlerParams h)),
                        "type" .= (show $ handlerType h),
                        "inputs" .= [ 
                                object [
                                    "name" .= fn,
                                    "type" .= Null
                                ] 
                            | fn <- concatMap getJsonAttrs $ handlerParams h 
                        ],
                        "outputs" .= (concatMap outputs $ handlerParams h)
                    ] |  h <- routeHandlers r
                ]
            ] | r <- modRoutes m
        ]
        
    ]
    where
        outputs hp = case hp of
            Select sq -> [
                    object [
                        "name" .= selectFieldName sf
                    ] | sf <- sqFields sq
                ]
            Return ofs -> [
                    object [
                        "name" .= pn,
                        "type" .= Null
                    ] | (pn,ofr) <- ofs 
                ]   
            _ -> []
        selectFieldName sf = case sf of
            SelectField _ fn mvn -> fromMaybe fn mvn
            SelectIdField _ mvn -> fromMaybe "id" mvn
            SelectParamField _ pn mvn -> fromMaybe pn mvn
            SelectValExpr _ vn -> vn
            _ -> ""        

        fieldJson f = object [
                "name" .= fieldName f,
                "optional" .= fieldOptional f,
                "default" .= (fieldDefault f >>= fieldValueJson),
                "references" .= (case fieldContent f of
                    EntityField en -> toJSON en
                    EnumField en _ -> toJSON en
                    _ -> Null),
                "type" .= (case fieldContent f of
                    NormalField ft _ -> case ft of
                        FTWord32 -> "integer"
                        FTWord64 -> "integer"
                        FTInt32 -> "integer"
                        FTInt -> "integer"
                        FTInt64 -> "integer"
                        FTText -> "string"
                        FTBool -> "boolean"
                        FTDouble -> "number"
                        FTTimeOfDay -> "timeofday"
                        FTDay -> "day"
                        FTUTCTime -> "utctime"
                        FTZonedTime -> "zonedtime"
                    EntityField en -> "integer"
                    EnumField en _ -> ("string" :: String)
                    CheckmarkField _ -> "boolean")
            ]
        fieldValueJson fv = Just $ case fv of
            StringValue s -> toJSON s
            IntValue i -> toJSON i
            FloatValue f -> toJSON f
            BoolValue b -> toJSON b
            NothingValue -> Null
            CheckmarkFieldValue cv -> toJSON $ show cv
            EnumFieldValue _ ev -> toJSON ev
            EmptyList -> Array V.empty


