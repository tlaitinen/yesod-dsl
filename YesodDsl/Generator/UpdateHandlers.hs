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
import YesodDsl.Generator.Input
import Data.Generics.Uniplate.Data
import Control.Monad.State
import qualified Data.Map as Map


updateHandlerRunDB :: (Int,Stmt) -> State Context String
updateHandlerRunDB (pId,p) = liftM concat $ sequence ([
        updateHandlerDecode (pId,p) >>= return . (indent 4),
        case p of
            GetById (Right e) fr vn -> do
                let en = entityName e
                ifr <- inputFieldRef fr
                return $ T.unpack $(codegenFile "codegen/get-by-id.cg")
            Update (Right e) fr io -> do
                let en = entityName e
                ifr <- inputFieldRef fr
                return $ T.unpack $(codegenFile "codegen/replace.cg")
            Insert (Right e) io mbv -> do
                let en = entityName e
                return $ T.unpack $(codegenFile "codegen/insert.cg")
                    where 
                    maybeBindResult (Just vn) = "result_" ++ vn ++ " <- "
                    maybeBindResult _ = ""
            DeleteFrom en vn Nothing -> return $
                let maybeExpr = rstrip $ T.unpack $(codegenFile "codegen/delete-all.cg") 
                in T.unpack $(codegenFile "codegen/delete.cg")
            DeleteFrom (Right en) vn (Just e) -> do
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
                return $ T.unpack $(codegenFile "codegen/call.cg") 
            _ -> return ""
    ] :: [State Context String])
defaultFieldValue :: Field -> String
defaultFieldValue f = case fieldDefault f of
    Just fv -> fieldValueToHs fv
    Nothing -> if fieldOptional f
        then "Nothing"
        else let fn = fieldName f in T.unpack $(codegenFile "codegen/map-input-field-normal.cg")

mapJsonInputField :: [FieldRefMapping] -> Bool -> (Entity,Field) -> State Context (Maybe String)
mapJsonInputField ifields isNew (e,f) = do
    mcontent <- mkContent
    case mcontent of
        Just content' -> do
            let content = rstrip content'
            return $ Just $ T.unpack $(codegenFile "codegen/map-input-field.cg")
        Nothing -> return Nothing
    where 
        maybeJust :: Bool -> String -> String
        maybeJust True v = "(Just " ++ v ++ ")"
        maybeJust False v = v
        maybeInput = matchInputField ifields (fieldName f)
        notNothing = case maybeInput of
            Just (Const NothingValue, _) -> False
            _ -> True 
        notInputField = case maybeInput of
            Just (RequestField _, _) -> False
            _ -> True
        promoteJust = fieldOptional f && isJust maybeInput && notNothing && notInputField

        mapper mmapper = maybe "" ((" $ " ++) . (++ " $ ")) mmapper
        mkContent = case maybeInput of
            Just (ifr@(RequestField fn), mm) -> do
                return $ Just $ mapper mm ++ T.unpack $(codegenFile "codegen/map-input-field-normal.cg")
            Just (AuthId, mm) -> return $ Just $ mapper mm ++ T.unpack $(codegenFile "codegen/map-input-field-authid.cg")
            Just (AuthField fn, mm) -> return $ Just $ mapper mm ++ T.unpack $(codegenFile "codegen/map-input-field-auth.cg")
            Just (PathParam i, mm) -> return $ Just $ mapper mm ++ T.unpack $(codegenFile "codegen/map-input-field-pathparam.cg")
            Just (Const v, mm) -> return $ Just $ mapper mm ++ T.unpack $(codegenFile "codegen/map-input-field-const.cg")
            Just (Now, mm) -> return $ Just $ mapper mm ++ T.unpack $(codegenFile "codegen/map-input-field-now.cg")
            Just (NamedLocalParam vn, mm) -> return $ Just $ mapper mm ++ T.unpack $(codegenFile "codegen/map-input-field-localparam.cg")
            Just (LocalParamField (Var vn (Right e) _) fn, mm) -> do
                let en = entityName e 
                return $ Just $ mapper mm ++ T.unpack $(codegenFile "codegen/input-field-local-param-field.cg")
            Nothing -> return $ if isNew then Just $ defaultFieldValue f
                                else Nothing
matchInputField :: [FieldRefMapping] -> FieldName -> Maybe (FieldRef, Maybe FunctionName)
matchInputField ifields fn =  listToMaybe [ (inp,mm) | (pn,inp,mm) <- ifields, pn == fn ]
prepareJsonInputField :: (FieldName,Maybe FieldValue) -> String
prepareJsonInputField (fn,Nothing) = T.unpack $(codegenFile "codegen/prepare-input-field-normal.cg")
prepareJsonInputField (fn, Just d) = T.unpack $(codegenFile "codegen/prepare-input-field-normal-default.cg")


 
updateHandlerDecode :: (Int,Stmt) -> State Context String
updateHandlerDecode (pId,p) = case p of
    Update (Right e) fr io -> do
        m <- gets ctxModule
        readInputObject e (io >>= \io' -> Just (Nothing, io')) (Just fr)
    Insert (Right e) io _ -> do
        m <- gets ctxModule
        readInputObject e io Nothing
    _ -> return ""
    where 
        readInputObject :: Entity -> Maybe (Maybe VariableName, [FieldRefMapping]) -> Maybe FieldRef -> State Context String
        readInputObject e (Just (mv, fields)) fr = do
            maybeExisting <- maybeSelectExisting e (mv,fields) fr
            fieldMappers <- mapFields e fields isNew
            return $ T.unpack $(codegenFile "codegen/read-input-object-fields.cg")           
            where 
                isNew = isNothing fr && isNothing mv
                entityToUpdate
                    | isNew = entityName e
                    | otherwise = "e"

        readInputObject e Nothing _  = do
            fieldMappers <- mapFields e [] True
            return $ T.unpack $(codegenFile "codegen/read-input-object-whole.cg")

        maybeSelectExisting e (Nothing, fields) (Just fr)
            | Nothing `elem` [ matchInputField fields (fieldName f) 
                                 | f <- entityFields e ] = do
                 ifr <- inputFieldRef fr
                 return $ T.unpack $(codegenFile "codegen/select-existing.cg")
            | otherwise = return ""
        maybeSelectExisting _ (Just vn, _) _ = return $ T.unpack $(codegenFile "codegen/select-bound-result.cg")
        maybeSelectExisting e _ _ = return ""
        mapFields e fields isNew = liftM ((intercalate ",\n") . catMaybes) $ mapM (mapJsonInputField fields isNew) 
                                      [ (e,f) | f <- entityFields e ]
updateHandlerReadJsonFields :: State Context String
updateHandlerReadJsonFields = do
    m <- gets ctxModule
    ps <- gets ctxStmts
    let attrs = jsonAttrs m ps
    let defaults = getParamDefaults ps
    if null attrs
        then return ""
        else return $
            (T.unpack $(codegenFile "codegen/json-body.cg")) 
            ++ concatMap (\attr -> prepareJsonInputField (attr, Map.lookup attr defaults)) attrs
    where
        jsonAttrs m ps = nub $ concatMap getJsonAttrs ps



updateHandlerMaybeCurrentTime :: [Stmt] -> String
updateHandlerMaybeCurrentTime ps = if  Now  `elem` inputFields 
    then (T.unpack $(codegenFile "codegen/prepare-now.cg"))
    else ""
    where inputFields = concatMap universeBi ps

updateHandlerMaybeAuth :: [Stmt] -> String
updateHandlerMaybeAuth ps 
    | (not . null) (filter isAuthField inputFields) = T.unpack $(codegenFile "codegen/load-auth.cg")
    | otherwise = ""
    where inputFields = concatMap universeBi ps
          isAuthField (AuthField _) = True
          isAuthField _ = False

updateHandlerReturnRunDB :: [Stmt] -> String
updateHandlerReturnRunDB ps = case listToMaybe $ filter isReturn ps of
    Just (Return ofrs) -> T.unpack $(codegenFile "codegen/rundb-return-fields.cg")
    _ -> T.unpack $(codegenFile "codegen/rundb-return-none.cg")
    where
        isReturn (Return _) = True
        isReturn _ = False
        trOutputField (pn,NamedLocalParam vn,mm) = rstrip $ T.unpack $(codegenFile "codegen/output-field-local-param.cg")
        trOutputField (pn,fr,_) = error "not implemented yet, sorry"

updateHandler :: State Context String
updateHandler = do
    ps <- gets ctxStmts

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


