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
inputFieldRef :: Context -> InputFieldRef -> String
-- inputFieldRef ps (InputFieldNormal fn) = T.unpack $(codegenFile "codegen/input-field-normal.cg") TODO
inputFieldRef ps InputFieldAuthId = T.unpack $(codegenFile "codegen/input-field-authid.cg")
inputFieldRef ps (InputFieldPathParam i) = T.unpack $(codegenFile "codegen/input-field-path-param.cg")
inputFieldRef _ _ = undefined



updateHandlerRunDB :: Module -> Route -> [HandlerParam] -> (Int,HandlerParam) -> String
updateHandlerRunDB m r ps (pId,p) = case p of
    (Replace en fr io) -> T.unpack $(codegenFile "codegen/replace.cg")
    (Insert en io) -> T.unpack $(codegenFile "codegen/insert.cg")
    _ -> ""
    where ctx = []

mapJsonInputField :: [InputField] -> (Entity,Field) -> String
mapJsonInputField ifields (e,f) = T.unpack $(codegenFile "codegen/map-input-field.cg")
    where 
        content = case matchInputField (fieldName f) of
            InputFieldNormal fn -> T.unpack $(codegenFile "codegen/map-input-field-normal.cg")
            InputFieldAuthId -> "authId"
            InputFieldPathParam i -> "p" ++ show i
            InputFieldConst v -> show v
        matchInputField fn = fromJust $ listToMaybe [ inp | (pn,inp) <- ifields,
                                                      pn == fn ]

updateHandlerDecode :: Module -> Route -> [HandlerParam] -> (Int,HandlerParam) -> String
updateHandlerDecode m r ps (pId,p) = case p of
    (Replace en fr io) -> let ctx = [] in readInputObject (fromJust $ lookupEntity m en) io
    (Insert en io) -> let ctx = [] in readInputObject (fromJust $ lookupEntity m en) io
    where readInputObject e (Just fields) = T.unpack $(codegenFile "codegen/read-input-object-fields.cg")

          readInputObject e Nothing = T.unpack $(codegenFile "codegen/read-input-object-whole.cg")
          mapFields e fields = intercalate ",\n" $ map (mapJsonInputField fields) 
                                      [ (e,f) | f <- entityFields e ]

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
            ctx = [(en,vn)]
        in $(codegenFile "codegen/delete.cg")
    DeleteFrom en vn (Just e) -> 
        let maybeExpr = hsExpr ctx e 
            ctx = [(en,vn)]
        in $(codegenFile "codegen/delete.cg")
    _ -> ""

deleteHandler :: Module -> Route -> [HandlerParam] -> String
deleteHandler m r ps = 
            (T.unpack $(codegenFile "codegen/rundb.cg" ))
            ++ (concatMap (deleteHandlerRunDB m r ps) ps)
            ++ (T.unpack $(codegenFile "codegen/delete-handler-footer.cg"))

