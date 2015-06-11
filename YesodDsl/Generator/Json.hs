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
import Data.Generics.Uniplate.Data
import qualified Data.List as L
import qualified Data.Text.Lazy as LT
import Data.Function
fieldRefMappingToAttrs :: Entity -> [FieldRefMapping] -> [(FieldName, Maybe FieldContent)]
fieldRefMappingToAttrs e fs = [ (fieldName f, Just $ fieldContent f) | f <- entityFields e, isNothing $ fieldDefault f, fieldOptional f == False, fieldInternal f == False, fieldName f `notElem` mapped ] ++ [ (pn, Just $ fieldContent f) | f <- entityFields e, (fn,fr,_) <- fs, (RequestField pn) <- universeBi fr, fieldName f == fn ]
    where
        mapped = [ fn | (fn, _, _) <- fs ]

requestAttrs :: Stmt -> [(FieldName, Maybe FieldContent)]
requestAttrs (Update (Right e) _ Nothing) = [ (fieldName f, Just $ fieldContent f) | f <- entityFields e, fieldInternal f == False ]
requestAttrs (Update (Right e) _ (Just fs)) = fieldRefMappingToAttrs e fs
requestAttrs (Insert (Right e) Nothing _) = [ (fieldName f, Just $ fieldContent f) | f <- entityFields e, fieldInternal f == False ]
requestAttrs (Insert (Right e) (Just (_, fs)) _) = fieldRefMappingToAttrs e fs
requestAttrs hp = [ (fn, Nothing) | RequestField fn <- universeBi hp ] ++ (concat $ [ requestAttrs i | i@(Insert _ _ _) <- universeBi hp ] ++ [ requestAttrs u | u@(Update _ _ _) <- universeBi hp ])
requestAttrs hp = [ (fn, Nothing) | RequestField fn <- universeBi hp ]
                ++ (concat [ [ (fieldName f, Just $ fieldContent f) | f <- entityFields e, isNothing $ fieldDefault f, fieldOptional f == False ]
                    | Insert (Right e) Nothing _ <- universeBi hp ])

nubAttrs :: [(FieldName, Maybe FieldContent)] -> [(FieldName, Maybe FieldContent)]
nubAttrs  = L.nubBy ((==) `on` fst) . (L.sortBy cmp)
    where
        cmp (_, Just _) (_, Nothing) = LT
        cmp (_, Nothing) (_, Just _) = GT
        cmp _ _ = EQ
 
moduleToJson :: Module -> String
moduleToJson m = LT.unpack $ LTE.decodeUtf8 $ encodePretty $ object [
        "name" .= moduleName m,
        "classes" .= [
            object [
                "name" .= className c,
                "fields" .= [ fieldJson f | f <- classFields c, fieldInternal f == False ],
                "instances" .= [ entityName e | e <- modEntities m, 
                                 className c `elem` entityInstances e ]
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
                                    "type" .= (mfc >>= Just . toJSON . jsonFieldType),
                                    "references" .= (mfc >>= Just . toJSON . jsonFieldReferences)
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
                    SelectValExpr _ vn -> vn
                    _ -> ""
                type_ = case sf of
                    SelectField (Var _ (Right e) _) fn _ -> fromMaybe Null $ lookupField e fn >>= Just . toJSON . jsonFieldType . fieldContent
                    SelectIdField _ _ -> String "integer"
                    SelectValExpr ve _ -> case ve of
                        ConcatManyExpr _ -> String "string"
                        ValBinOpExpr _ Concat _ -> String "string"
                        ValBinOpExpr _ _ _ -> String "number"
                        RandomExpr -> String "number"
                        FloorExpr _ -> String "number"
                        CeilingExpr _ -> String "number"
                        ExtractExpr _ _ -> String "string"
                        _ -> Null
                    _ -> Null    
                references = case sf of
                    SelectIdField (Var _ (Right e) _) _ -> String $ T.pack $ entityName e
                    SelectField (Var _ (Right e) _) fn _ -> fromMaybe Null $ lookupField e fn >>= Just . jsonFieldReferences . fieldContent
                    _ -> Null
                  
        fieldJson f = object [
                "name" .= fieldName f,
                "optional" .= fieldOptional f,
                "default" .= (fieldDefault f >>= fieldValueJson),
                "references" .= (jsonFieldReferences $ fieldContent f),
                "type" .= (jsonFieldType $ fieldContent f)
              ]
        jsonFieldReferences fc = case fc of
            EntityField en -> toJSON en
            EnumField en _ -> toJSON en
            _ -> Null
        jsonFieldType fc = case fc of
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
                FTCheckmark -> "boolean"
            EntityField _ -> "integer"
            EnumField _ _ -> ("string" :: String)
 
        fieldValueJson fv = Just $ case fv of
            StringValue s -> toJSON s
            IntValue i -> toJSON i
            FloatValue f -> toJSON f
            BoolValue b -> toJSON b
            NothingValue -> Null
            CheckmarkValue cv -> toJSON $ show cv
            EnumFieldValue _ ev -> toJSON ev
            EmptyList -> Array V.empty


