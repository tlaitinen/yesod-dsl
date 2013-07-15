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
inputFieldRef :: Context -> InputFieldRef -> String
-- inputFieldRef ps (InputFieldNormal fn) = T.unpack $(codegenFile "codegen/input-field-normal.cg") TODO
inputFieldRef ps InputFieldAuthId = T.unpack $(codegenFile "codegen/input-field-authid.cg")
inputFieldRef ps (InputFieldPathParam i) = T.unpack $(codegenFile "codegen/input-field-path-param.cg")
inputFieldRef _ _ = undefined



updateHandlerRunDB :: Module -> Route -> [HandlerParam] -> (Int,HandlerParam) -> String
updateHandlerRunDB m r ps (pId,p) = case p of
    (Update en fr io) -> T.unpack $(codegenFile "codegen/replace.cg")
    (Insert en io) -> T.unpack $(codegenFile "codegen/insert.cg")
    _ -> ""
    where ctx = Context { ctxNames = [], ctxModule = m }

mapJsonInputField :: [InputField] -> (Entity,Field) -> String
mapJsonInputField ifields (e,f) = T.unpack $(codegenFile "codegen/map-input-field.cg")
    where 
        content = case matchInputField ifields (fieldName f) of
            Just (InputFieldNormal fn) -> T.unpack $(codegenFile "codegen/map-input-field-normal.cg")
            Just InputFieldAuthId -> T.unpack $(codegenFile "codegen/map-input-field-authid.cg")
            Just (InputFieldPathParam i) -> T.unpack $(codegenFile "codegen/map-input-field-pathparam.cg")
            Just (InputFieldConst v) -> T.unpack $(codegenFile "codegen/map-input-field-const.cg")
            Nothing -> T.unpack $(codegenFile "codegen/map-input-field-no-match.cg")
matchInputField :: [InputField] -> FieldName -> Maybe InputFieldRef
matchInputField ifields fn =  listToMaybe [ inp | (pn,inp) <- ifields, pn == fn ]
prepareJsonInputField :: [InputField] -> (Entity,Field) -> String
prepareJsonInputField ifields (e,f) = case matchInputField ifields (fieldName f) of
    Just (InputFieldNormal fn) -> T.unpack $(codegenFile "codegen/prepare-input-field-normal.cg")
    _ -> ""

 
updateHandlerDecode :: Module -> Route -> [HandlerParam] -> (Int,HandlerParam) -> String
updateHandlerDecode m r ps (pId,p) = case p of
    Update en fr io -> readInputObject (fromJust $ lookupEntity m en) io fr
    Insert en io -> let e = (fromJust $ lookupEntity m en) in 
        T.unpack $(codegenFile "codegen/read-input-object-whole.cg")
    _ -> ""
    where readInputObject e (Just fields) fr = T.unpack $(codegenFile "codegen/read-input-object-fields.cg")

          readInputObject e Nothing _ = T.unpack $(codegenFile "codegen/read-input-object-whole.cg")

          maybeSelectExisting e fields  fr
              | Nothing `elem` [ matchInputField fields (fieldName f) 
                                 | f <- entityFields e ] = let ctx = Context { ctxNames = [], ctxModule = m }  in T.unpack $(codegenFile "codegen/select-existing.cg")
              | otherwise = ""
          mapFields e fields = intercalate ",\n" $ map (mapJsonInputField fields) 
                                      [ (e,f) | f <- entityFields e ]
          prepareFields e fields = concatMap (prepareJsonInputField fields) [ (e,f) | f <- entityFields e ]                    

updateHandler :: Module -> Route -> [HandlerParam] -> String
updateHandler m r ps = (T.unpack $(codegenFile "codegen/json-body.cg"))
            ++ (concatMap (updateHandlerDecode m r ps) $ zip [1..] ps)
            ++ (T.unpack $(codegenFile "codegen/rundb.cg"))
            ++ (concatMap (updateHandlerRunDB m r ps) $ zip [1..] ps)
            ++ (T.unpack $(codegenFile "codegen/update-handler-footer.cg"))

deleteHandlerRunDB :: Module -> Route -> [HandlerParam] -> HandlerParam -> String
deleteHandlerRunDB m r ps p = T.unpack $ case p of
    DeleteFrom en vn Nothing -> let 
            maybeExpr = rstrip $ T.unpack $(codegenFile "codegen/delete-all.cg") 
            ctx = Context { ctxNames = [(en,vn, False)], ctxModule = m }
        in $(codegenFile "codegen/delete.cg")
    DeleteFrom en vn (Just e) -> 
        let maybeExpr = hsExpr ctx e 
            ctx = Context { ctxNames=  [(en,vn, False)], ctxModule = m }
        in $(codegenFile "codegen/delete.cg")
    _ -> ""

deleteHandler :: Module -> Route -> [HandlerParam] -> String
deleteHandler m r ps = 
            (T.unpack $(codegenFile "codegen/rundb.cg" ))
            ++ (concatMap (deleteHandlerRunDB m r ps) ps)
            ++ (T.unpack $(codegenFile "codegen/delete-handler-footer.cg"))

