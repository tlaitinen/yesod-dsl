{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator.PureScript (moduleToPureScript, moduleToPureScriptJs) where
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
  
pureScriptFieldType :: Field -> String
pureScriptFieldType f = (if fieldOptional f then "Maybe " else "") 
    ++ case fieldContent f of
        NormalField ft -> case ft of
            FTWord32 -> "Int"
            FTWord64 -> "Number"
            FTInt32 -> "Int"
            FTInt -> "Number"
            FTInt64 -> "Number"
            FTText -> "String"
            FTBool -> "Boolean"
            FTDouble -> "Number"
            FTRational -> "Number"
            FTTimeOfDay -> "TimeOfDay"
            FTDay -> "Day"           
            FTUTCTime -> "UTCTime"
            FTCheckmark -> "Boolean"
        EntityField en -> en ++ "Id"
        EnumField en -> en

mkField :: FieldName -> (Bool,FieldContent) -> Field
mkField n (o,c) = Field (Loc "" 0 0) o n c [] Nothing

moduleToPureScriptJs :: Module -> String
moduleToPureScriptJs m = T.unpack $(codegenFile "codegen/purescript-js.cg")
moduleToPureScript :: Module -> String
moduleToPureScript m = T.unpack $(codegenFile "codegen/purescript.cg")
        ++ concat [ handler r h | r <- modRoutes m, h <- routeHandlers r ]
    where
        enum e = T.unpack $(codegenFile "codegen/purescript-enum.cg")
            where
                value v = enumName e ++ v
                showValue v = T.unpack $(codegenFile "codegen/purescript-enum-show.cg")
                decodeValue v = rstrip $ T.unpack $(codegenFile "codegen/purescript-enum-decodevalue.cg")
                encodeValue v = T.unpack $(codegenFile "codegen/purescript-enum-encodevalue.cg")
        handler r h = handlerOutput 
            where
                
                handlerOutput
                    | null $ outputFields h = ""
                    | otherwise = T.unpack $(codegenFile "codegen/purescript-handler-output.cg")
                field en f  = rstrip $ T.unpack $(codegenFile "codegen/purescript-field.cg")
                decodeJsonExtract en f = T.unpack $(codegenFile "codegen/purescript-decodejson-extract.cg")
                decodeJsonAssign en f = rstrip $ T.unpack $(codegenFile "codegen/purescript-decodejson-assign.cg")

                handlerTypeName = upperFirst $ map toLower (show $ handlerType h) 
                outputEntityName = handlerTypeName ++ concatMap pathName (routePath r) 
        pathName pp = case pp of
            PathText t -> upperFirst t
            PathId _ en -> en ++ "Id"
         
        routePathParams r = mapMaybe (\(n,pp) -> case pp of
            PathText _ -> Nothing
            PathId _ en -> Just ("(Key p" ++ show (n::Int) ++ ")",en ++ "Id")) $ zip [1..] (routePath r)
        routePathUrl r = concatMap (\(n,pp) -> case pp of
            PathText t -> " ++ \"/" ++ t ++ "\""
            PathId _ _ -> " ++ \"/\" ++ show p" ++ show (n::Int)) $ zip [1..] (routePath r)
        
        outputFields h = concatMap stmtOutputs $ handlerStmts h
        stmtOutputs s = case s of
            Select sq -> mapMaybe selectFieldToField $ sqFields sq
            Return ofs -> mapMaybe (\(pn,fr,_) -> fieldRefToContent fr >>= Just . (mkField pn)) ofs
            _ -> []
        selectFieldToField sf = case sf of
            SelectField (Var _ (Right e) mf) fn mvn -> do
                f <- lookupField e fn
                let f' = f { fieldOptional = fieldOptional f || mf }
                Just $ case mvn of
                    Just vn -> f' { fieldName = vn }
                    Nothing -> f'
            SelectIdField (Var _ (Right e) mf) mvn -> Just $ 
                mkField (fromMaybe "id" mvn) (mf, EntityField $ entityName e)
            SelectValExpr ve vn -> do
                fc <- case ve of
                    FieldExpr fr -> fieldRefToContent fr
                    ValBinOpExpr _ op _ -> Just $ if op `elem` [Add,Sub,Div,Mul]
                        then (False, NormalField FTDouble)
                        else (False, NormalField FTText)
                    RandomExpr -> Just (False, NormalField FTDouble)
                    FloorExpr _ -> Just (False, NormalField FTDouble)
                    CeilingExpr _ -> Just (False, NormalField FTDouble)
                    ExtractExpr _ _ -> Just (False, NormalField FTDouble)
                    _ -> Nothing
                Just $ mkField vn fc
            _ -> Nothing    
        fieldRefToContent fr = case fr of
            SqlId (Var _ (Right e) mf)     -> Just (mf, EntityField $ entityName e)
            SqlField (Var _ (Right e) mf) fn -> do
                f <- lookupField e fn
                Just $ (fieldOptional f || mf, fieldContent f)
            AuthId -> Just (False, EntityField "User")
            AuthField fn -> listToMaybe [ (fieldOptional f, fieldContent f)
                                          | e <- modEntities m,   
                                            f <- entityFields e,  
                                            entityName e == "User",
                                            fieldName f == fn ]
            LocalParamField (Var _ (Right e) mf) fn -> do
                f <- lookupField e fn
                Just $ (fieldOptional f || mf, fieldContent f)
            _ -> Nothing
        entity e = T.unpack $(codegenFile "codegen/purescript-entity.cg")
