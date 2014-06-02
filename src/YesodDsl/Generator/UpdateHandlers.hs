{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module YesodDsl.Generator.UpdateHandlers where
import YesodDsl.AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import Data.String.Utils (rstrip)
import YesodDsl.Generator.Esqueleto
import YesodDsl.Generator.Common
import YesodDsl.Generator.Models
import YesodDsl.Generator.Require
import Control.Monad.State
import qualified Data.Map as Map

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
                PathId en -> return $ en ++ "Id"
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
updateHandlerRunDB :: (Int,HandlerParam) -> State Context String
updateHandlerRunDB (pId,p) = liftM concat $ sequence ([
        updateHandlerDecode (pId,p) >>= return . (indent 4),
        case p of
            GetById en fr vn -> do
                ifr <- inputFieldRef fr
                return $ T.unpack $(codegenFile "codegen/get-by-id.cg")
            Update en fr io -> do
                ifr <- inputFieldRef fr
                return $ T.unpack $(codegenFile "codegen/replace.cg")
            Insert en io mbv -> 
                return $ T.unpack $(codegenFile "codegen/insert.cg")
                    where 
                    maybeBindResult (Just vn) = "result_" ++ vn ++ " <- "
                    maybeBindResult _ = ""
            DeleteFrom en vn Nothing -> return $
                let maybeExpr = rstrip $ T.unpack $(codegenFile "codegen/delete-all.cg") 
                in T.unpack $(codegenFile "codegen/delete.cg")
            DeleteFrom en vn (Just e) -> do
                withScope [(en,vn,False)] $ do
                    maybeExpr <- hsBoolExpr e
                    return $ T.unpack $(codegenFile "codegen/delete.cg")
            For vn fr hps -> do
                content <- liftM concat $ mapM updateHandlerRunDB $ 
                    zip [1..] hps
                ifr <- inputFieldRef fr
                return $ T.unpack $(codegenFile "codegen/for.cg")    
            Call fn frs -> do
                ifrs <- mapM inputFieldRef frs
                types <- mapM inputFieldRefType frs
                ctx <- get
                put $ ctx { ctxCalls = (fn,types):ctxCalls ctx}
                return $ T.unpack $(codegenFile "codegen/call.cg") 
            _ -> return ""
    ] :: [State Context String])
defaultFieldValue :: Field -> String
defaultFieldValue f = case fieldDefault f of
    Just fv -> fieldValueToHs fv
    Nothing -> if fieldOptional f
        then "Nothing"
        else let fn = fieldName f in T.unpack $(codegenFile "codegen/map-input-field-normal.cg")
addCtxType :: InputFieldRef -> TypeName -> State Context ()
addCtxType ifr typeName = do
    ctx <- get
    put $ ctx { ctxTypes = Map.insert ifr typeName $ ctxTypes ctx }

mapJsonInputField :: [InputField] -> Bool -> (Entity,Field) -> State Context String
mapJsonInputField ifields isNew (e,f) = do
    content' <- mkContent
    let content = rstrip content'
    return $ T.unpack $(codegenFile "codegen/map-input-field.cg")
    where 
        maybeJust :: Bool -> String -> String
        maybeJust True v = "(Just " ++ v ++ ")"
        maybeJust False v = v
        maybeInput = matchInputField ifields (fieldName f)
        notNothing = case maybeInput of
            Just (InputFieldConst NothingValue) -> False
            _ -> True 
        notInputField = case maybeInput of
            Just (InputFieldNormal _) -> False
            _ -> True
        promoteJust = fieldOptional f && isJust maybeInput && notNothing && notInputField
        mkContent = case maybeInput of
            Just (ifr@(InputFieldNormal fn)) -> do
                addCtxType ifr (hsFieldType f)
                return $ T.unpack $(codegenFile "codegen/map-input-field-normal.cg")
            Just InputFieldAuthId -> return $ T.unpack $(codegenFile "codegen/map-input-field-authid.cg")
            Just (InputFieldAuth fn) -> return $ T.unpack $(codegenFile "codegen/map-input-field-auth.cg")
            Just (InputFieldPathParam i) -> return $ T.unpack $(codegenFile "codegen/map-input-field-pathparam.cg")
            Just (InputFieldConst v) -> return $ T.unpack $(codegenFile "codegen/map-input-field-const.cg")
            Just (InputFieldNow) -> return $ T.unpack $(codegenFile "codegen/map-input-field-now.cg")
            Just (InputFieldLocalParam vn) -> return $ T.unpack $(codegenFile "codegen/map-input-field-localparam.cg")
            Just (InputFieldLocalParamField vn fn) -> do
                ps <- gets ctxHandlerParams
                let en = fromJust $ listToMaybe $ concatMap f ps
                return $ T.unpack $(codegenFile "codegen/input-field-local-param-field.cg")
                where
                      f (GetById en _ vn') = if vn' == vn then [en] else []
                      f _ = []
            Nothing -> return $ if isNew then defaultFieldValue f
                                else T.unpack $(codegenFile "codegen/map-input-field-no-match.cg")
matchInputField :: [InputField] -> FieldName -> Maybe InputFieldRef
matchInputField ifields fn =  listToMaybe [ inp | (pn,inp) <- ifields, pn == fn ]
prepareJsonInputField :: FieldName -> String
prepareJsonInputField fn = T.unpack $(codegenFile "codegen/prepare-input-field-normal.cg")

 
updateHandlerDecode :: (Int,HandlerParam) -> State Context String
updateHandlerDecode (pId,p) = case p of
    Update en fr io -> do
        m <- gets ctxModule
        readInputObject (fromJust $ lookupEntity m en) io (Just fr)
    Insert en io _ -> do
        m <- gets ctxModule
        readInputObject (fromJust $ lookupEntity m en) io Nothing
    _ -> return ""
    where 
        readInputObject e (Just fields) fr = do
            maybeExisting <- maybeSelectExisting e fields fr
            fieldMappers <- mapFields e fields (isNothing fr)
            return $ T.unpack $(codegenFile "codegen/read-input-object-fields.cg")           
        readInputObject e Nothing _ = do
            fieldMappers <- mapFields e [] True
            return $ T.unpack $(codegenFile "codegen/read-input-object-whole.cg")

        maybeSelectExisting e fields (Just fr)
            | Nothing `elem` [ matchInputField fields (fieldName f) 
                                 | f <- entityFields e ] = do
                 ifr <- inputFieldRef fr
                 return $ T.unpack $(codegenFile "codegen/select-existing.cg")
            | otherwise = return ""
        maybeSelectExisting e _ _ = return ""
        mapFields e fields isNew = liftM (intercalate ",\n") $ mapM (mapJsonInputField fields isNew) 
                                      [ (e,f) | f <- entityFields e ]
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

updateHandlerReadJsonFields :: State Context String
updateHandlerReadJsonFields = do
    m <- gets ctxModule
    ps <- gets ctxHandlerParams
    let attrs = jsonAttrs m ps
    if null attrs
        then return ""
        else return $
            (T.unpack $(codegenFile "codegen/json-body.cg")) 
            ++ concatMap prepareJsonInputField attrs
    where
        jsonAttrs m ps = nub $ concatMap (getJsonAttrs m) ps

handlerParamToInputFieldRefs :: HandlerParam -> [InputFieldRef]
handlerParamToInputFieldRefs (Update _ fr io) = [fr] ++ [ fr' | (_,fr') <- fromMaybe [] io]
handlerParamToInputFieldRefs (Insert _ io _) = [ fr | (_,fr) <- fromMaybe [] io ]
handlerParamToInputFieldRefs (GetById _ ifr _) = [ifr]
handlerParamToInputFieldRefs (Call _ ifrs) = ifrs
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

updateHandler :: State Context String
updateHandler = do
    ps <- gets ctxHandlerParams

    liftM concat $ sequence ([
            updateHandlerReadJsonFields,
            return $ updateHandlerMaybeCurrentTime ps,
            return $ updateHandlerMaybeAuth ps,
            requireStmts,
            return $ (T.unpack $(codegenFile "codegen/rundb.cg")),
            liftM concat $ mapM updateHandlerRunDB $ zip [1..] ps,
            return $ updateHandlerReturnRunDB ps,
            return $ T.unpack $(codegenFile "codegen/update-handler-footer.cg")
        ] :: [State Context String])


