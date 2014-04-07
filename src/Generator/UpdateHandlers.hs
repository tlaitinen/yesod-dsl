{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.UpdateHandlers where
import AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import Data.String.Utils (rstrip)
import Generator.Esqueleto
import Generator.Common
import Generator.Models
import Generator.Require

inputFieldRef :: Context -> InputFieldRef -> String
-- inputFieldRef ps (InputFieldNormal fn) = T.unpack $(codegenFile "codegen/input-field-normal.cg") TODO
inputFieldRef ctx InputFieldAuthId = T.unpack $(codegenFile "codegen/input-field-authid.cg")
inputFieldRef ctx (InputFieldAuth fn) = T.unpack $(codegenFile "codegen/input-field-auth.cg")
inputFieldRef ctx (InputFieldLocalParamField vn fn) = rstrip $ T.unpack $(codegenFile "codegen/input-field-local-param-field.cg")
    where en = fromJust $ listToMaybe $ concatMap f (ctxHandlerParams ctx)
          f (GetById en _ vn') = if vn' == vn then [en] else []
          f _ = []
            
inputFieldRef ctx (InputFieldPathParam i) = T.unpack $(codegenFile "codegen/input-field-path-param.cg")
inputFieldRef ctx (InputFieldNormal pn) = rstrip $ T.unpack $(codegenFile "codegen/input-field-normal.cg")
inputFieldRef _ ifr  = error $ "Cannot use " ++ show ifr 



updateHandlerRunDB :: Module -> Route -> [HandlerParam] -> (Int,HandlerParam) -> String
updateHandlerRunDB m r ps (pId,p) = indent 4 (updateHandlerDecode m r ps (pId,p)) ++ case p of
    GetById en fr vn     -> T.unpack $(codegenFile "codegen/get-by-id.cg")
    Update en fr io          -> T.unpack $(codegenFile "codegen/replace.cg")
    Insert en io mbv          -> T.unpack $(codegenFile "codegen/insert.cg")
    DeleteFrom en vn Nothing -> let 
            maybeExpr = rstrip $ T.unpack $(codegenFile "codegen/delete-all.cg") 
            ctx = Context { ctxNames = [(en,vn, False)], ctxModule = m, ctxHandlerParams = ps  }
        in T.unpack $(codegenFile "codegen/delete.cg")
    DeleteFrom en vn (Just e) -> 
        let maybeExpr = hsExpr ctx e 
            ctx = Context { ctxNames=  [(en,vn, False)], ctxModule = m, ctxHandlerParams = ps }
        in T.unpack $(codegenFile "codegen/delete.cg")
    For vn ifr hps -> let 
            content = concatMap (updateHandlerRunDB m r hps) $ zip [1..] hps
        in T.unpack $(codegenFile "codegen/for.cg")    
    _ -> ""
    where 
        ctx = Context { ctxNames = [], ctxModule = m, ctxHandlerParams = ps }
        maybeBindResult (Just vn) = "result_" ++ vn ++ " <- "
        maybeBindResult _ = ""

defaultFieldValue :: Field -> String
defaultFieldValue f = case fieldDefault f of
    Just fv -> fieldValueToHs fv
    Nothing -> if fieldOptional f
        then "Nothing"
        else let fn = fieldName f in T.unpack $(codegenFile "codegen/map-input-field-normal.cg")

mapJsonInputField :: [InputField] -> Bool -> (Entity,Field) -> String
mapJsonInputField ifields isNew (e,f) = T.unpack $(codegenFile "codegen/map-input-field.cg")
    where 
        maybeInput = matchInputField ifields (fieldName f)
        notNothing = case maybeInput of
            Just (InputFieldConst NothingValue) -> False
            _ -> True 
        notInputField = case maybeInput of
            Just (InputFieldNormal _) -> False
            _ -> True
        promoteJust = fieldOptional f && isJust maybeInput && notNothing && notInputField
        content = case maybeInput of
            Just (InputFieldNormal fn) -> T.unpack $(codegenFile "codegen/map-input-field-normal.cg")
            Just InputFieldAuthId -> T.unpack $(codegenFile "codegen/map-input-field-authid.cg")
            Just (InputFieldAuth fn) -> T.unpack $(codegenFile "codegen/map-input-field-auth.cg")
            Just (InputFieldPathParam i) -> T.unpack $(codegenFile "codegen/map-input-field-pathparam.cg")
            Just (InputFieldConst v) -> T.unpack $(codegenFile "codegen/map-input-field-const.cg")
            Just (InputFieldNow) -> T.unpack $(codegenFile "codegen/map-input-field-now.cg")
            Just (InputFieldLocalParam vn) -> T.unpack $(codegenFile "codegen/map-input-field-localparam.cg")
            Nothing -> if isNew then defaultFieldValue f
                                else T.unpack $(codegenFile "codegen/map-input-field-no-match.cg")
matchInputField :: [InputField] -> FieldName -> Maybe InputFieldRef
matchInputField ifields fn =  listToMaybe [ inp | (pn,inp) <- ifields, pn == fn ]
prepareJsonInputField :: FieldName -> String
prepareJsonInputField fn = T.unpack $(codegenFile "codegen/prepare-input-field-normal.cg")

 
updateHandlerDecode :: Module -> Route -> [HandlerParam] -> (Int,HandlerParam) -> String
updateHandlerDecode m r ps (pId,p) = case p of
    Update en fr io -> readInputObject (fromJust $ lookupEntity m en) io (Just fr)
    Insert en io _ -> readInputObject (fromJust $ lookupEntity m en) io Nothing
    _ -> ""
    where readInputObject e (Just fields) fr =  T.unpack $(codegenFile "codegen/read-input-object-fields.cg")           
          readInputObject e Nothing _ = T.unpack $(codegenFile "codegen/read-input-object-whole.cg")

          maybeSelectExisting e fields (Just fr)
              | Nothing `elem` [ matchInputField fields (fieldName f) 
                                 | f <- entityFields e ] = let ctx = Context { ctxNames = [], ctxModule = m, ctxHandlerParams = ps }  in T.unpack $(codegenFile "codegen/select-existing.cg")
              | otherwise = ""
          maybeSelectExisting e _ _ = ""
          mapFields e fields isNew = intercalate ",\n" $ map (mapJsonInputField fields isNew) 
                                      [ (e,f) | f <- entityFields e ]
fieldRefToJsonAttrs :: FieldRef -> [FieldName]
fieldRefToJsonAttrs (FieldRefRequest fn) = [fn]
fieldRefToJsonAttrs (FieldRefSubQuery sq) = fromMaybe [] $ do
    expr <- sqWhere sq
    return $ exprToJsonAttrs expr
fieldRefToJsonAttrs _ = []
                         
inputFieldRefToJsonAttr :: InputFieldRef -> Maybe FieldName
inputFieldRefToJsonAttr (InputFieldNormal fn) = Just fn
inputFieldRefToJsonAttr _ = Nothing

inputFieldToJsonAttr :: InputField -> Maybe FieldName
inputFieldToJsonAttr (_,fr) = inputFieldRefToJsonAttr fr
inputFieldToJsonAttr _ = Nothing

valExprToJsonAttr :: ValExpr -> [FieldName]
valExprToJsonAttr (FieldExpr fr) = fieldRefToJsonAttrs fr
valExprToJsonAttr (ConcatExpr ve1 ve2) = concatMap valExprToJsonAttr [ve1,ve2]
valExprToJsonAttr _ = []

exprToJsonAttrs :: Expr -> [FieldName]
exprToJsonAttrs (AndExpr e1 e2) = concatMap exprToJsonAttrs [e1,e2]
exprToJsonAttrs (OrExpr e1 e2) = concatMap exprToJsonAttrs [e1,e2]
exprToJsonAttrs (NotExpr e) = exprToJsonAttrs e
exprToJsonAttrs (ListOpExpr fr1 _ fr2) = concatMap fieldRefToJsonAttrs [fr1,fr2]
exprToJsonAttrs (BinOpExpr ve1 _ ve2) = concatMap valExprToJsonAttr [ve1,ve2]

getJsonAttrs :: Module -> HandlerParam -> [FieldName]
getJsonAttrs _ (Update _ fr (Just fields)) = maybeToList (inputFieldRefToJsonAttr fr) ++ (mapMaybe inputFieldToJsonAttr fields)
getJsonAttrs m (Update en fr Nothing) = maybeToList (inputFieldRefToJsonAttr fr) ++ case lookupEntity m en of
    Just e -> [ fieldName f | f <- entityFields e, isNothing $ fieldDefault f, fieldOptional f == False ]
    _ -> []
getJsonAttrs _ (Insert _ (Just fields) _) = mapMaybe inputFieldToJsonAttr fields
getJsonAttrs m (Insert en Nothing _) =  case lookupEntity m en of
    Just e -> [ fieldName f | f <- entityFields e, isNothing $ fieldDefault f, fieldOptional f == False ]
    _ -> []
getJsonAttrs _ (DeleteFrom _ _ (Just e)) = exprToJsonAttrs e
getJsonAttrs _ (Require sq) = let
    exprs = catMaybes $ [sqWhere sq] ++ [joinExpr j| j <- sqJoins sq]
    in concatMap exprToJsonAttrs exprs
getJsonAttrs m (For vn fr ps) = maybeToList (inputFieldRefToJsonAttr fr ) ++ concatMap (getJsonAttrs m) ps
getJsonAttrs _ _ = []

updateHandlerReadJsonFields :: Module -> Route -> [HandlerParam] -> String
updateHandlerReadJsonFields m r ps = concatMap prepareJsonInputField jsonAttrs
    where
        jsonAttrs = nub $ concatMap (getJsonAttrs m) ps

handlerParamToInputFieldRefs :: HandlerParam -> [InputFieldRef]
handlerParamToInputFieldRefs (Update _ fr io) = [fr] ++ [ fr' | (_,fr') <- fromMaybe [] io]
handlerParamToInputFieldRefs (Insert _ io _) = [ fr | (_,fr) <- fromMaybe [] io ]
handlerParamToInputFieldRefs (GetById _ ifr _) = [ifr]
handlerParamToInputFieldRefs _ = []

updateHandlerMaybeCurrentTime :: [HandlerParam] -> String
updateHandlerMaybeCurrentTime ps = if  InputFieldNow  `elem` inputFields 
    then (T.unpack $(codegenFile "codegen/prepare-now.cg"))
    else ""
    where inputFields = concatMap handlerParamToInputFieldRefs ps

updateHandlerMaybeAuth :: [HandlerParam] -> String
updateHandlerMaybeAuth ps 
    | (not . null) (filter isAuthField inputFields) = T.unpack $(codegenFile "codegen/load-auth.cg")
    | otherwise = ""
    where inputFields = concatMap handlerParamToInputFieldRefs ps
          isAuthField (InputFieldAuth _) = True
          isAuthField _ = False

updateHandlerReturnRunDB :: [HandlerParam] -> String
updateHandlerReturnRunDB ps = case listToMaybe $ filter isReturn ps of
    Just (Return ofrs) -> T.unpack $(codegenFile "codegen/rundb-return-fields.cg")
    _ -> T.unpack $(codegenFile "codegen/rundb-return-none.cg")
    where
        isReturn (Return _) = True
        isReturn _ = False
        trOutputField (pn,OutputFieldLocalParam vn) = rstrip $ T.unpack $(codegenFile "codegen/output-field-local-param.cg")

updateHandler :: Module -> Route -> [HandlerParam] -> String
updateHandler m r ps = (T.unpack $(codegenFile "codegen/json-body.cg"))
            ++ (updateHandlerReadJsonFields m r ps)
            ++ updateHandlerMaybeCurrentTime ps
            ++ updateHandlerMaybeAuth ps
            ++ (requireStmts m ps)
            ++ (T.unpack $(codegenFile "codegen/rundb.cg"))
            ++ (concatMap (updateHandlerRunDB m r ps) $ zip [1..] ps)
            ++ updateHandlerReturnRunDB ps
            ++ (T.unpack $(codegenFile "codegen/update-handler-footer.cg"))


