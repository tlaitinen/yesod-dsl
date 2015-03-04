{-# LANGUAGE TemplateHaskell #-}
module YesodDsl.Generator.Input where
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import Data.String.Utils (rstrip)
import Control.Monad.State
import YesodDsl.AST
import YesodDsl.Generator.Esqueleto
import YesodDsl.Generator.Common
import YesodDsl.Generator.Models
import qualified Data.Map as Map
formatType :: Type -> String
formatType (TypeEntityId en) = en ++ "Id"
formatType (TypeEnum en) = en 
formatType (TypeList t) = "[" ++ formatType t ++ "]"
formatType (TypeField ft) = fieldTypeToHsType ft
formatType (TypeMaybe f) = "Maybe (" ++ formatType f ++ ")"


inputFieldRefType :: InputFieldRef -> State Context String
inputFieldRefType InputFieldAuthId = return $ "UserId"
inputFieldRefType (InputFieldAuth fn) = do
    m <- gets ctxModule
    let mf = listToMaybe [ hsFieldType f | e <- modEntities m,
                                           f <- entityFields e,
                                           fieldName f == fn ]
    case mf of
        Just ft -> return ft
        Nothing -> return "Unknown"
inputFieldRefType (InputFieldLocalParam vn) = do
    ps <- gets ctxHandlerParams
    let en = fromMaybe "Unknown" $ listToMaybe $ concatMap f ps
    return $ en ++ "Id"

    where
        f (Insert en _ (Just vn')) = if vn' == vn then [en] else []
        f _ = []
     

inputFieldRefType (InputFieldLocalParamField vn fn) = do
    ps <- gets ctxHandlerParams
    let en = fromMaybe "Unknown" $ listToMaybe $ concatMap f ps
    m <- gets ctxModule
    let mf = lookupField m en fn
    return $ case mf of
        Just f -> hsFieldType f
        Nothing -> "Unknown"
    where 
          f (GetById en _ vn') = if vn' == vn then [en] else []
          f _ = []
            
inputFieldRefType (InputFieldPathParam i) = do
    mr <- gets ctxRoute
    case mr of
        Just r -> do
            let p = (routePathParams r) !! (i-1)
            case p of
                PathId _ en -> return $ en ++ "Id"
                _ -> return ""
        Nothing -> return ""
inputFieldRefType ifr = do
    types <- gets ctxTypes
    return $ Map.findWithDefault "Unknown" ifr types

inputFieldRef :: InputFieldRef -> State Context String
inputFieldRef InputFieldAuthId = return $ rstrip $ T.unpack $(codegenFile "codegen/input-field-authid.cg")
inputFieldRef (InputFieldAuth fn) = return $ rstrip $ T.unpack $(codegenFile "codegen/input-field-auth.cg")
inputFieldRef (InputFieldLocalParam vn) = return $ rstrip $ T.unpack $(codegenFile "codegen/map-input-field-localparam.cg")
 
inputFieldRef (InputFieldLocalParamField vn fn) = do
    ps <- gets ctxHandlerParams
    let en = fromJust $ listToMaybe $ concatMap f ps
    return $ rstrip $ T.unpack $(codegenFile "codegen/input-field-local-param-field.cg")
    where 
          f (GetById en _ vn') = if vn' == vn then [en] else []
          f _ = []
            
inputFieldRef (InputFieldPathParam i) = return $ T.unpack $(codegenFile "codegen/input-field-path-param.cg")
inputFieldRef (InputFieldNormal pn) = return $ rstrip $ T.unpack $(codegenFile "codegen/input-field-normal.cg")
inputFieldRef ifr = return $ show ifr

fieldRefToJsonAttrs :: FieldRef -> [FieldName]
fieldRefToJsonAttrs (FieldRefRequest fn) = [fn]
fieldRefToJsonAttrs _ = []
                         
inputFieldRefToJsonAttr :: InputFieldRef -> Maybe FieldName
inputFieldRefToJsonAttr (InputFieldNormal fn) = Just fn
inputFieldRefToJsonAttr _ = Nothing

inputFieldToJsonAttr :: InputField -> Maybe FieldName
inputFieldToJsonAttr (_,fr) = inputFieldRefToJsonAttr fr
inputFieldToJsonAttr _ = Nothing

valExprToJsonAttr :: ValExpr -> [FieldName]
valExprToJsonAttr (FieldExpr fr) = fieldRefToJsonAttrs fr
valExprToJsonAttr (ConcatManyExpr ves) = concatMap valExprToJsonAttr ves
valExprToJsonAttr (ValBinOpExpr ve1 _ ve2) = concatMap valExprToJsonAttr [ve1,ve2]
valExprToJsonAttr (FloorExpr ve) = valExprToJsonAttr ve
valExprToJsonAttr (CeilingExpr ve) = valExprToJsonAttr ve
valExprToJsonAttr (SubQueryExpr sq) = fromMaybe [] $ do
    expr <- sqWhere sq
    return $ exprToJsonAttrs expr
valExprToJsonAttr _ = []

exprToJsonAttrs :: BoolExpr -> [FieldName]
exprToJsonAttrs (AndExpr e1 e2) = concatMap exprToJsonAttrs [e1,e2]
exprToJsonAttrs (OrExpr e1 e2) = concatMap exprToJsonAttrs [e1,e2]
exprToJsonAttrs (NotExpr e) = exprToJsonAttrs e
exprToJsonAttrs (BinOpExpr ve1 _ ve2) = concatMap valExprToJsonAttr [ve1,ve2]
exprToJsonAttrs (ExistsExpr sq) = fromMaybe [] $ do
    expr <- sqWhere sq
    return $ exprToJsonAttrs expr

getJsonAttrs :: Module -> HandlerParam -> [FieldName]
getJsonAttrs _ (Update _ fr (Just fields)) = maybeToList (inputFieldRefToJsonAttr fr) ++ (mapMaybe inputFieldToJsonAttr fields)
getJsonAttrs m (Update en fr Nothing) = maybeToList (inputFieldRefToJsonAttr fr) ++ case lookupEntity m en of
    Just e -> [ fieldName f | f <- entityFields e, isNothing $ fieldDefault f, fieldOptional f == False ]
    _ -> []
getJsonAttrs _ (Insert _ (Just (_,fields)) _) = mapMaybe inputFieldToJsonAttr fields
getJsonAttrs m (Insert en Nothing _) =  case lookupEntity m en of
    Just e -> [ fieldName f | f <- entityFields e, isNothing $ fieldDefault f, fieldOptional f == False ]
    _ -> []
getJsonAttrs _ (DeleteFrom _ _ (Just e)) = exprToJsonAttrs e
getJsonAttrs _ (Require sq) = let
    exprs = catMaybes $ [sqWhere sq] ++ [joinExpr j| j <- sqJoins sq]
    in concatMap exprToJsonAttrs exprs
getJsonAttrs m (For vn fr ps) = maybeToList (inputFieldRefToJsonAttr fr ) ++ concatMap (getJsonAttrs m) ps
getJsonAttrs _ (Call _ ifrs) = mapMaybe (inputFieldRefToJsonAttr . fst) ifrs
getJsonAttrs _ _ = []


