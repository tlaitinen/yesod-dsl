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
import YesodDsl.Generator.Require
import YesodDsl.Generator.Input
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map


updateHandlerRunDB :: (Int,Stmt) -> String
updateHandlerRunDB (pId,p) = concat $ [
        indent 4 $ updateHandlerDecode (pId,p),
        case p of
            GetById (Right e) fr vn -> 
                let en = entityName e
                    ifr = inputFieldRef fr
                in T.unpack $(codegenFile "codegen/get-by-id.cg")
            Update (Right e) fr _ -> 
                let en = entityName e
                    ifr = inputFieldRef fr 
                in T.unpack $(codegenFile "codegen/replace.cg")
            Insert (Right e) _ mbv -> let en = entityName e in
                T.unpack $(codegenFile "codegen/insert.cg")
                    where 
                    maybeBindResult (Just vn) = "result_" ++ vn ++ " <- "
                    maybeBindResult _ = ""
            DeleteFrom en vn Nothing -> 
                let maybeExpr = rstrip $ T.unpack $(codegenFile "codegen/delete-all.cg") 
                in T.unpack $(codegenFile "codegen/delete.cg")
            DeleteFrom (Right en) vn (Just e) -> 
                    let maybeExpr = scopedBoolExpr (Map.fromList [(vn,(en,False))]) e
                    in T.unpack $(codegenFile "codegen/delete.cg")
            For vn fr hps -> 
                let content = concatMap updateHandlerRunDB $ zip [1..] hps
                    ifr = inputFieldRef fr
                in T.unpack $(codegenFile "codegen/for.cg")    
            Call fn frs -> 
                let ifrs = map inputFieldRef frs
                in T.unpack $(codegenFile "codegen/call.cg") 
            _ -> ""
    ] 
defaultFieldValue :: Field -> String
defaultFieldValue f = case fieldDefault f of
    Just fv -> fieldValueToHs fv
    Nothing -> if fieldOptional f
        then "Nothing"
        else let fn = fieldName f in T.unpack $(codegenFile "codegen/map-input-field-normal.cg")

mapJsonInputField :: [FieldRefMapping] -> Bool -> (Entity,Field) -> Maybe String
mapJsonInputField ifields isNew (e,f) = do
    case mcontent of
        Just content' -> let content = rstrip content' 
                         in Just $ T.unpack $(codegenFile "codegen/map-input-field.cg")
        Nothing -> Nothing
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

        mcontent 
            | null ifields && fieldInternal f == False  = Just $ let fn = fieldName f in T.unpack $(codegenFile "codegen/map-input-field-normal.cg")
            | otherwise = case maybeInput of
                Just (RequestField fn, mm) -> Just $ resultMapper mm ++ T.unpack $(codegenFile "codegen/map-input-field-normal.cg")
                Just (AuthId, mm) -> Just $ resultMapper mm ++ T.unpack $(codegenFile "codegen/map-input-field-authid.cg")
                Just (AuthField fn, mm) -> Just $ resultMapper mm ++ T.unpack $(codegenFile "codegen/map-input-field-auth.cg")
                Just (PathParam i, mm) -> Just $ resultMapper mm ++ T.unpack $(codegenFile "codegen/map-input-field-pathparam.cg")
                Just (Const v, mm) -> Just $ resultMapper mm ++ T.unpack $(codegenFile "codegen/map-input-field-const.cg")
                Just (Now, mm) -> Just $ resultMapper mm ++ T.unpack $(codegenFile "codegen/map-input-field-now.cg")
                Just (NamedLocalParam vn, mm) -> Just $ resultMapper mm ++ T.unpack $(codegenFile "codegen/map-input-field-localparam.cg")
                Just (LocalParamField (Var vn (Right e') _) fn, mm) -> do
                    let en = entityName e'
                    return $ resultMapper mm ++ T.unpack $(codegenFile "codegen/input-field-local-param-field.cg")
                Just (fr,_) -> error $ "Sorry, not implemented yet: " ++ show fr    
                Nothing -> if isNew then Just $ defaultFieldValue f
                                    else Nothing
matchInputField :: [FieldRefMapping] -> FieldName -> Maybe (FieldRef, Maybe FunctionName)
matchInputField ifields fn =  listToMaybe [ (inp,mm) | (pn,inp,mm) <- ifields, pn == fn ]
prepareJsonInputField :: (FieldName,Maybe FieldValue) -> String
prepareJsonInputField (fn,Nothing) = T.unpack $(codegenFile "codegen/prepare-input-field-normal.cg")
prepareJsonInputField (fn, Just d) = T.unpack $(codegenFile "codegen/prepare-input-field-normal-default.cg")


 
updateHandlerDecode :: (Int,Stmt) -> String
updateHandlerDecode (pId,p) = case p of
    Update (Right e) fr io -> readInputObject e (io >>= \io' -> Just (Nothing, io')) (Just fr)
    Insert (Right e) io _ ->  readInputObject e io Nothing
    _ -> ""
    where 
        readInputObject :: Entity -> Maybe (Maybe VariableName, [FieldRefMapping]) -> Maybe FieldRef -> String
        readInputObject e (Just (mv, fields)) fr = 
            let
                maybeExisting = maybeSelectExisting e (mv,fields) fr
                fieldMappers = mapFields e fields isNew
                isNew = isNothing fr && isNothing mv
                entityToUpdate
                    | isNew = entityName e
                    | otherwise = "e"
            in T.unpack $(codegenFile "codegen/read-input-object-fields.cg")           

        readInputObject e Nothing _  = 
            let
                fieldMappers =  mapFields e [] True
            in  T.unpack $(codegenFile "codegen/read-input-object-whole.cg")

        maybeSelectExisting e (Nothing, fields) (Just fr)
            | Nothing `elem` [ matchInputField fields (fieldName f) 
                                 | f <- entityFields e ] = 
                 let ifr = inputFieldRef fr
                 in T.unpack $(codegenFile "codegen/select-existing.cg")
            | otherwise =  ""
        maybeSelectExisting _ (Just vn, _) _ = T.unpack $(codegenFile "codegen/select-bound-result.cg")
        maybeSelectExisting _ _ _ = ""
        mapFields e fields isNew = intercalate ",\n" $ catMaybes $ map (mapJsonInputField fields isNew) 
                                      [ (e,f) | f <- entityFields e ]
updateHandlerReadJsonFields :: [Stmt] -> String
updateHandlerReadJsonFields ps = do
    let attrs = nub $ concatMap getJsonAttrs ps
    let defaults = getParamDefaults ps
    if null attrs
        then ""
        else  (T.unpack $(codegenFile "codegen/json-body.cg")) 
            ++ concatMap (\attr -> prepareJsonInputField (attr, Map.lookup attr defaults)) attrs

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
        trOutputField (_,fr,_) = error $ "not implemented yet, sorry : " ++ show fr


updateHandler :: [Stmt] -> String
updateHandler ps = concat $ [
            updateHandlerReadJsonFields ps,
            updateHandlerMaybeCurrentTime ps,
            updateHandlerMaybeAuth ps,
            requireStmts ps,
            T.unpack $(codegenFile "codegen/rundb.cg"),
            concatMap updateHandlerRunDB $ zip [1..] ps,
            updateHandlerReturnRunDB ps,
            T.unpack $(codegenFile "codegen/update-handler-footer.cg")
        ] 


