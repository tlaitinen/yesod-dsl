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
inputFieldRef ps (InputFieldAuth fn) = T.unpack $(codegenFile "codegen/input-field-auth.cg")

inputFieldRef ps (InputFieldPathParam i) = T.unpack $(codegenFile "codegen/input-field-path-param.cg")
inputFieldRef _ _ = undefined



updateHandlerRunDB :: Module -> Route -> [HandlerParam] -> (Int,HandlerParam) -> String
updateHandlerRunDB m r ps (pId,p) = indent 4 (updateHandlerDecode m r ps (pId,p)) ++ case p of
    Update en fr io          -> T.unpack $(codegenFile "codegen/replace.cg")
    Insert en io mbv          -> T.unpack $(codegenFile "codegen/insert.cg")
    DeleteFrom en vn Nothing -> let 
            maybeExpr = rstrip $ T.unpack $(codegenFile "codegen/delete-all.cg") 
            ctx = Context { ctxNames = [(en,vn, False)], ctxModule = m }
        in T.unpack $(codegenFile "codegen/delete.cg")
    DeleteFrom en vn (Just e) -> 
        let maybeExpr = hsExpr ctx e 
            ctx = Context { ctxNames=  [(en,vn, False)], ctxModule = m }
        in T.unpack $(codegenFile "codegen/delete.cg")
    _ -> ""
    where 
        ctx = Context { ctxNames = [], ctxModule = m }
        maybeBindResult (Just vn) = "result_" ++ vn ++ " <- "
        maybeBindResult _ = ""

mapJsonInputField :: [InputField] -> (Entity,Field) -> String
mapJsonInputField ifields (e,f) = T.unpack $(codegenFile "codegen/map-input-field.cg")
    where 
        maybeInput = matchInputField ifields (fieldName f)
        notNothing = case maybeInput of
           Just (InputFieldConst NothingValue) -> False
           _ -> True 
        promoteJust = fieldOptional f && isJust maybeInput && notNothing
        content = case maybeInput of
            Just (InputFieldNormal fn) -> T.unpack $(codegenFile "codegen/map-input-field-normal.cg")
            Just InputFieldAuthId -> T.unpack $(codegenFile "codegen/map-input-field-authid.cg")
            Just (InputFieldAuth fn) -> T.unpack $(codegenFile "codegen/map-input-field-auth.cg")
            Just (InputFieldPathParam i) -> T.unpack $(codegenFile "codegen/map-input-field-pathparam.cg")
            Just (InputFieldConst v) -> T.unpack $(codegenFile "codegen/map-input-field-const.cg")
            Just (InputFieldNow) -> T.unpack $(codegenFile "codegen/map-input-field-now.cg")
            Just (InputFieldLocalParam vn) -> T.unpack $(codegenFile "codegen/map-input-field-localparam.cg")
            Nothing -> T.unpack $(codegenFile "codegen/map-input-field-no-match.cg")
matchInputField :: [InputField] -> FieldName -> Maybe InputFieldRef
matchInputField ifields fn =  listToMaybe [ inp | (pn,inp) <- ifields, pn == fn ]
prepareJsonInputField :: FieldName -> String
prepareJsonInputField fn = T.unpack $(codegenFile "codegen/prepare-input-field-normal.cg")

 
updateHandlerDecode :: Module -> Route -> [HandlerParam] -> (Int,HandlerParam) -> String
updateHandlerDecode m r ps (pId,p) = case p of
    Update en fr io -> readInputObject (fromJust $ lookupEntity m en) io (Just fr)
    Insert en io _ -> readInputObject (fromJust $ lookupEntity m en) io Nothing
    _ -> ""
    where readInputObject e (Just fields) fr = T.unpack $(codegenFile "codegen/read-input-object-fields.cg")
          

          readInputObject e Nothing _ = T.unpack $(codegenFile "codegen/read-input-object-whole.cg")

          maybeSelectExisting e fields (Just fr)
              | Nothing `elem` [ matchInputField fields (fieldName f) 
                                 | f <- entityFields e ] = let ctx = Context { ctxNames = [], ctxModule = m }  in T.unpack $(codegenFile "codegen/select-existing.cg")
              | otherwise = ""
          maybeSelectExisting _ _ _ = ""    
          mapFields e fields = intercalate ",\n" $ map (mapJsonInputField fields) 
                                      [ (e,f) | f <- entityFields e ]
fieldRefToJsonAttr :: FieldRef -> Maybe FieldName
fieldRefToJsonAttr (FieldRefRequest fn) = Just fn
fieldRefToJsonAttr _ = Nothing
                         
inputFieldRefToJsonAttr :: InputFieldRef -> Maybe FieldName
inputFieldRefToJsonAttr (InputFieldNormal fn) = Just fn
inputFieldRefToJsonAttr _ = Nothing

inputFieldToJsonAttr :: InputField -> Maybe FieldName
inputFieldToJsonAttr (_,fr) = inputFieldRefToJsonAttr fr
inputFieldToJsonAttr _ = Nothing

valExprToJsonAttr :: ValExpr -> [FieldName]
valExprToJsonAttr (FieldExpr fr) = maybeToList $ fieldRefToJsonAttr fr
valExprToJsonAttr (ConcatExpr ve1 ve2) = concatMap valExprToJsonAttr [ve1,ve2]
valExprToJsonAttr _ = []

exprToJsonAttrs :: Expr -> [FieldName]
exprToJsonAttrs (AndExpr e1 e2) = concatMap exprToJsonAttrs [e1,e2]
exprToJsonAttrs (OrExpr e1 e2) = concatMap exprToJsonAttrs [e1,e2]
exprToJsonAttrs (NotExpr e) = exprToJsonAttrs e
exprToJsonAttrs (ListOpExpr fr1 _ fr2) = mapMaybe fieldRefToJsonAttr [fr1,fr2]
exprToJsonAttrs (BinOpExpr ve1 _ ve2) = concatMap valExprToJsonAttr [ve1,ve2]

getJsonAttrs :: HandlerParam -> [FieldName]
getJsonAttrs (Update _ fr io) = maybeToList (inputFieldRefToJsonAttr fr)
                            ++ (mapMaybe inputFieldToJsonAttr (fromMaybe [] io))
getJsonAttrs (Insert _ io _) = mapMaybe inputFieldToJsonAttr (fromMaybe [] io)
getJsonAttrs (DeleteFrom _ _ (Just e)) = exprToJsonAttrs e
getJsonAttrs _ = []

updateHandlerReadJsonFields :: Module -> Route -> [HandlerParam] -> String
updateHandlerReadJsonFields m r ps = concatMap prepareJsonInputField jsonAttrs
    where
        jsonAttrs = nub $ concatMap getJsonAttrs ps

handlerParamToInputFieldRefs :: HandlerParam -> [InputFieldRef]
handlerParamToInputFieldRefs (Update _ fr io) = [fr] ++ [ fr' | (_,fr') <- fromMaybe [] io]
handlerParamToInputFieldRefs (Insert _ io _) = [ fr | (_,fr) <- fromMaybe [] io ]
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

   
updateHandler :: Module -> Route -> [HandlerParam] -> String
updateHandler m r ps = (T.unpack $(codegenFile "codegen/json-body.cg"))
            ++ (updateHandlerReadJsonFields m r ps)
            ++ updateHandlerMaybeCurrentTime ps
            ++ updateHandlerMaybeAuth ps
            ++ (T.unpack $(codegenFile "codegen/rundb.cg"))
            ++ (concatMap (updateHandlerRunDB m r ps) $ zip [1..] ps)
            ++ (T.unpack $(codegenFile "codegen/update-handler-footer.cg"))


