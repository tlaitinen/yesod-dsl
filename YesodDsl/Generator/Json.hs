{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator.Json (moduleToJson) where
import YesodDsl.AST
import Data.Aeson.Encode.Pretty
import qualified Data.Text as T
import Data.Aeson
import Data.Maybe
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
                "fields" .= [ fieldJson f | f <- classFields c, fieldInternal f == False, fieldReadOnly f == False ],
                "instances" .= [ entityName e | e <- modEntities m, 
                                 className c `elem` entityInstances e ]
            ] | c <- modClasses m
        ],
        "entities" .= [
            object [
                "name" .= entityName e,
                "fields" .= [ fieldJson f | f <- entityFields e, fieldInternal f == False, fieldReadOnly f == False ]
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
                                            "references" .= ("null" :: String),
                                            "value" .= s
                                        ]  
                        PathId _ en -> object [
                                            "type" .= ("integer" :: String),
                                            "references" .= en
                                        ]                 
                    | pp <- routePath r
                ],
                "handlers" .= [
                    object [
                        "public" .= (Public `elem` (handlerStmts h)),
                        "type" .= (show $ handlerType h),
                        "inputs" .= [ 
                                object [
                                    "name" .= fn,
                                    "type" .= ((eitherToMaybe mfc) >>= Just . toJSON . jsonFieldType . fieldContent),
                                    "references" .= ((eitherToMaybe mfc) >>= Just . toJSON . jsonFieldReferences . fieldContent)
                                ] 
                            | (fn, mfc) <- nubAttrs $ concatMap requestAttrs $ handlerStmts h 
                        ],
                        "outputs" .= (concatMap outputs $ handlerStmts h)
                    ] |  h <- routeHandlers r
                ]
            ] | r <- modRoutes m
        ]
        
    ]
    where
        eitherToMaybe = either (const Nothing) Just
        outputs hp = case hp of
            Select sq -> map selectField $ sqFields sq
            Return ofs -> [
                    object [
                        "name" .= pn,
                        "type" .= Null
                    ] | (pn,_,_) <- ofs 
                ]   
            _ -> []

        selectField sf = object [
                "name" .= name, 
                "type" .= type_,
                "references" .= references
            ] 
            where
                name = case sf of
                    SelectField _ fn mvn -> fromMaybe fn mvn
                    SelectIdField _ mvn -> fromMaybe "id" mvn
                    SelectExpr _ vn -> vn
                    _ -> ""
                type_ = case sf of
                    SelectField (Var _ (Right e) _) fn _ -> fromMaybe Null $ lookupField e fn >>= Just . toJSON . jsonFieldType . fieldContent
                    SelectIdField _ _ -> String "integer"
                    SelectExpr ve _ -> case ve of
                        ConcatManyExpr _ -> String "string"
                        BinOpExpr _ Concat _ -> String "string"
                        BinOpExpr _ _ _ -> String "number"
                        UnOpExpr Floor _ -> String "number"
                        UnOpExpr Ceiling _ -> String "number"
                        UnOpExpr Not _ -> String "boolean"
                        UnOpExpr (Extract _) _ -> String "string"
                        _ -> Null
                    _ -> Null    
                references = case sf of
                    SelectIdField (Var _ (Right e) _) _ -> String $ T.pack $ entityName e
                    SelectField (Var _ (Right e) _) fn _ -> fromMaybe Null $ lookupField e fn >>= Just . jsonFieldReferences . fieldContent
                    _ -> Null
                  
        fieldJson f = object [
                "name" .= fieldJsonName f,
                "optional" .= fieldOptional f,
                "default" .= (fieldDefault f >>= fieldValueJson),
                "references" .= (jsonFieldReferences $ fieldContent f),
                "type" .= (jsonFieldType $ fieldContent f)
              ]
        jsonFieldReferences fc = case fc of
            EntityField en -> toJSON en
            EnumField en -> toJSON en
            _ -> Null
        jsonFieldType fc = case fc of
            NormalField ft -> case ft of
                FTWord32 -> "integer"
                FTWord64 -> "integer"
                FTInt32 -> "integer"
                FTInt -> "integer"
                FTInt64 -> "integer"
                FTText -> "string"
                FTBool -> "boolean"
                FTDouble -> "number"
                FTRational -> "number"
                FTTimeOfDay -> "timeofday"
                FTDay -> "day"
                FTUTCTime -> "utctime"
                FTCheckmark -> "boolean"
            EntityField _ -> "integer"
            EnumField _ -> ("string" :: String)
 
        fieldValueJson fv = Just $ case fv of
            StringValue s -> toJSON s
            IntValue i -> toJSON i
            FloatValue f -> toJSON f
            BoolValue b -> toJSON b
            NothingValue -> Null
            CheckmarkValue cv -> toJSON $ show cv
            EnumFieldValue _ ev -> toJSON ev
            EmptyList -> Array V.empty


