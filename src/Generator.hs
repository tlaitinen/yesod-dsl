{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Generator (generate, hsRouteName) where

import System.IO (FilePath)
import Data.String.Utils (rstrip)    
import AST
import Text.Shakespeare.Text hiding (toText)
import qualified Data.Text as T
import Data.List
import Data.Maybe

import Data.Char
recName :: String -> String -> String
recName dt f = lowerFirst dt ++ upperFirst f


persistFieldType :: Field -> String
persistFieldType f = baseFieldType f 
                   ++ " " ++ (boolToMaybe . fieldOptional) f
                   ++ (maybeDefault . fieldDefault) f
    where maybeDefault (Just d) = " default='" ++ show d ++ "'"
          maybeDefault _ = " "
classFieldName :: Class -> Field -> String
classFieldName i f = (lowerFirst . className) i ++ (upperFirst . fieldName) f

entityFieldName :: Entity -> Field -> String
entityFieldName e f = (lowerFirst . entityName) e ++ (upperFirst . fieldName) f
entityFieldTypeName :: Entity -> Field -> String
entityFieldTypeName e f = upperFirst $ entityFieldName e f
enum :: EnumType -> String
enum e = T.unpack $(codegenFile "codegen/enum.cg")

modelField :: Field -> String
modelField f = T.unpack $(codegenFile "codegen/model-field.cg")

modelUnique :: Unique -> String
modelUnique (Unique name fields) = T.unpack $(codegenFile "codegen/model-unique.cg")

modelDeriving :: String -> String
modelDeriving d = T.unpack $(codegenFile "codegen/model-deriving.cg")

model :: Entity -> String
model e = T.unpack $(codegenFile "codegen/model-header.cg")
        ++ (concatMap modelField (entityFields e))
        ++ (concatMap modelUnique (entityUniques e)) 
        ++ (concatMap modelDeriving (entityDeriving e))

models :: Module -> String
models m = T.unpack $(codegenFile "codegen/models-header.cg")
         ++ (concatMap model (modEntities m))
         ++ (T.unpack $(codegenFile "codegen/models-footer.cg"))

classDefField :: Class -> Field -> String
classDefField c f = T.unpack $(codegenFile "codegen/class-field.cg")

classInstanceField :: Class -> Entity -> Field -> String
classInstanceField c e f = T.unpack $(codegenFile "codegen/class-instance-field.cg")

classInstance :: Class -> Entity -> String
classInstance c e = T.unpack $(codegenFile "codegen/class-instance-header.cg")
                  ++ (concatMap (classInstanceField c e) (classFields c))

classInstances :: Module -> Class -> String
classInstances m c = T.unpack $(codegenFile "codegen/class-header.cg")
                   ++ (concatMap (classDefField c) (classFields c))
                   ++ (concatMap (classInstance c) 
                                 [ e | e <- modEntities m, 
                                  (className c) `elem` (entityInstances e)])
                                                         

classes :: Module -> String
classes m = concatMap (classInstances m) (modClasses m)

hsRouteName :: [PathPiece] -> String
hsRouteName = f . routeName 
    where f ('/':'#':xs) = f xs
          f ('/':x:xs) = toUpper x : f xs
          f (x:xs) = x : f xs
          f [] = "R"

hsRouteType :: [PathPiece] -> String
hsRouteType = (intercalate " ") . (mapMaybe toType)
    where toType (PathText _) = Nothing
          toType (PathId en) = Just $ en ++ "Id -> "

routeResource :: Resource -> String
routeResource r = T.unpack $(codegenFile "codegen/route.cg")
    where handlers = intercalate " " (map (show . handlerType) (resHandlers r))

routes :: Module -> String
routes m = T.unpack $(codegenFile "codegen/routes-header.cg")
         ++ (concatMap routeResource (modResources m))
         ++ (T.unpack $(codegenFile "codegen/routes-footer.cg"))

validationFieldCheck :: Entity -> Field -> FunctionName -> String
validationFieldCheck e f func = rstrip $ T.unpack $(codegenFile "codegen/validation-field.cg")

validationEntityCheck :: Entity -> FunctionName -> String
validationEntityCheck e func = rstrip $ T.unpack $(codegenFile "codegen/validation-entity.cg")
    where fieldRef f = "(" ++ (lowerFirst . entityName) e ++ upperFirst f ++ " v)"

validationEntity :: Entity -> String
validationEntity e = T.unpack $(codegenFile "codegen/validation-entity-header.cg")
                   ++ (intercalate ",\n " $ [ validationFieldCheck e f func
                                          | f <- entityFields e,
                                            func <- fieldChecks f])
                   ++ (intercalate ",\n " $ [ validationEntityCheck e func |
                                              func <- entityChecks e ])
                   ++ (T.unpack $(codegenFile "codegen/validation-entity-footer.cg"))

type TypeName = String

validationFieldFunction :: (Field, FunctionName) -> String
validationFieldFunction (f,func) = T.unpack $(codegenFile "codegen/validation-function-field.cg")

validationEntityFunction :: (Entity, FunctionName) -> String
validationEntityFunction (e, func) = T.unpack $(codegenFile "codegen/validation-function-entity.cg")
    

lookupFieldType :: Module -> EntityName -> FieldName -> String
lookupFieldType m en fn = hsFieldType (fromJust $ lookupField m en fn)
validation :: Module -> String
validation m = T.unpack $(codegenFile "codegen/validation-header.cg")
             ++ (concatMap validationFieldFunction $ 
                    nubBy (\(_,f1) (_,f2) -> f1 == f2)
                    [ (f,func) | e <- modEntities m,
                             f <- entityFields e,
                             func <- fieldChecks f ])
             ++ (concatMap validationEntityFunction $ 
                   [ (e, func) | e <- modEntities m,   func <- entityChecks e ])
             ++ (concatMap validationEntity (modEntities m))

hsRouteParams :: [PathPiece] -> String
hsRouteParams ps = intercalate " " [("p" ++ show x) | 
                                    x <- [1..length (filter hasType ps)]]
    where hasType (PathId _) = True
          hasType _ = False

hsHandlerMethod :: HandlerType -> String          
hsHandlerMethod GetHandler    = "get"
hsHandlerMethod PutHandler    = "put"
hsHandlerMethod PostHandler   = "post"
hsHandlerMethod DeleteHandler = "delete"

maybeJust :: Bool -> String -> String
maybeJust True s = "(Just " ++ s ++ ")"
maybeJust False s = s

defaultFilterField :: (Entity, VariableName, Field) -> String
defaultFilterField (e,vn,f) = T.unpack $(codegenFile "codegen/default-filter-field.cg")

defaultFilterFields :: Module -> [HandlerParam] -> String
defaultFilterFields m ps = T.unpack $(codegenFile "codegen/default-filter-fields.cg") 
    where fields = concatMap defaultFilterField (handlerFields m ps)

defaultSortField :: (Entity, VariableName, Field) -> String    
defaultSortField (e,vn,f) = T.unpack $(codegenFile "codegen/default-sort-field.cg")
defaultSortFields :: Module -> [HandlerParam] -> String
defaultSortFields m ps = T.unpack $(codegenFile "codegen/default-sort-fields.cg")
    where fields = concatMap defaultSortField (handlerFields m ps)

getHandlerParam :: Module -> Resource -> [HandlerParam] -> HandlerParam -> String
getHandlerParam m r ps DefaultFilterSort = T.unpack $(codegenFile "codegen/default-filter-sort.cg")
    ++ (T.unpack $(codegenFile "codegen/offset-limit-param.cg"))
getHandlerParam m r ps (TextSearchFilter pn fs) = T.unpack $(codegenFile "codegen/text-search-filter-param.cg")
getHandlerParam _ _ _ _ = ""        


getHandlerJoinDef :: (JoinType, EntityName, VariableName, (Maybe (FieldRef, BinOp, FieldRef))) -> String
getHandlerJoinDef (jt, _, vn, _) = T.unpack $(codegenFile "codegen/get-handler-join-def.cg")

hsFieldRef :: [HandlerParam] -> FieldRef -> String
hsFieldRef ps (FieldRefId vn) = vn ++ " ^. " 
                 ++  (fromJust $ handlerVariableEntity ps vn) ++ "Id"
hsFieldRef ps (FieldRefNormal vn fn) = vn ++ " ^. " 
                 ++ (fromJust $ handlerVariableEntity ps vn) 
                 ++ (upperFirst fn)
hsFieldRef _ FieldRefAuthId = "(val authId)"
hsFieldRef _ (FieldRefPathParam p) = "(val p" ++ show p ++ ")"

inputFieldRef :: [HandlerParam] -> InputFieldRef -> String
-- inputFieldRef ps (InputFieldNormal fn) = T.unpack $(codegenFile "codegen/input-field-normal.cg") TODO
inputFieldRef ps InputFieldAuthId = T.unpack $(codegenFile "codegen/input-field-authid.cg")
inputFieldRef ps (InputFieldPathParam i) = T.unpack $(codegenFile "codegen/input-field-path-param.cg")
inputFieldRef _ _ = undefined


isMaybeFieldRef :: Module -> [HandlerParam] -> FieldRef -> Bool
isMaybeFieldRef m ps (FieldRefNormal vn fn) = fieldOptional $ fromJust $ lookupField m (fromJust $ handlerVariableEntity ps vn) fn
isMaybeFieldRef _ _  _ = False

hsBinOp :: BinOp -> String
hsBinOp op = case op of
    Eq -> "==."
    Ne -> "!=."
    Lt -> "<."
    Gt -> ">."
    Le -> "<=."
    Ge -> ">=."
    Like -> "like"

makeJustField :: Bool -> String -> String    
makeJustField True f = "(just " ++ f ++ ")"
makeJustField False f = f

getHandlerJoinExpr :: Module -> [HandlerParam] -> (JoinType, EntityName, VariableName, (Maybe (FieldRef, BinOp, FieldRef))) -> String
getHandlerJoinExpr m ps (_, en, vn, (Just (f1, op, f2))) = T.unpack $(codegenFile "codegen/get-handler-join-expr.cg")
    where f1just = f1maybe == False && f2maybe == True
          f2just = f2maybe == False && f1maybe == True
          f1maybe = isMaybeFieldRef m ps f1
          f2maybe = isMaybeFieldRef m ps f2
getHandlerJoinExpr m _ _ = ""

hsOrderBy :: [HandlerParam] -> (FieldRef, SortDir) -> String
hsOrderBy ps (f,d) = dir d ++ "(" ++ hsFieldRef ps f ++ ")"
    where dir SortAsc = "asc "
          dir SortDesc = "desc "


hsValExpr :: [HandlerParam] -> ValExpr -> String
hsValExpr ps ve = case ve of
    FieldExpr fr -> hsFieldRef ps fr
    ConstExpr fv -> "(val " ++ show fv ++ ")"

hsExpr :: [HandlerParam] -> Expr -> String
hsExpr ps expr = case expr of
    AndExpr e1 e2 -> "(" ++ hsExpr ps e1 ++ ") &&. (" ++ hsExpr ps e2 ++ ")"
    OrExpr e1 e2 -> "(" ++ hsExpr ps e1 ++ ") ||. (" ++ hsExpr ps e2 ++ ")"
    BinOpExpr e1 op e2 -> hsValExpr ps e1 ++ " " ++ hsBinOp op ++ hsValExpr ps e2

textSearchFilterField :: [HandlerParam] -> ParamName -> FieldRef -> String
textSearchFilterField ps pn f = T.unpack $(codegenFile "codegen/text-search-filter-field.cg")

getHandlerSQLExpr :: Module -> [HandlerParam] -> HandlerParam -> String
getHandlerSQLExpr m ps p = case p of
    DefaultFilterSort -> defaultFilterFields m ps ++ defaultSortFields m ps 
                       ++ (T.unpack $(codegenFile "codegen/offset-limit.cg"))
    TextSearchFilter pn fs -> let fields = concatMap (textSearchFilterField ps pn) fs in T.unpack $(codegenFile "codegen/text-search-filter.cg")
    (Where expr) -> T.unpack $(codegenFile "codegen/get-handler-where-expr.cg")
    OrderBy fields -> T.unpack $(codegenFile "codegen/get-handler-order-by.cg")
    _ -> ""

getHandlerSQLReturn :: [HandlerParam] -> String
getHandlerSQLReturn ps = T.unpack $ case handlerReturn ps of
    Left vn -> $(codegenFile "codegen/select-return-entity.cg")
    Right fs -> let fields = [ fr | (_,fr) <- fs ] in $(codegenFile "codegen/select-return-fields.cg") 
    
getReturn :: [HandlerParam] -> String
getReturn ps = T.unpack $ case handlerReturn ps of
    Left vn -> $(codegenFile "codegen/return-entity.cg")
    Right fs -> let
        params = [ pn | (pn,_) <- fs ]
        fields = map mapReturnField params
        in $(codegenFile "codegen/return-fields.cg")
    where
        mapReturnField pn = rstrip $ T.unpack $(codegenFile "codegen/return-field.cg")

getHandler :: Module -> Resource -> [HandlerParam] -> String
getHandler m r ps = 
    (concatMap (getHandlerParam m r ps) ps)
    ++ (T.unpack $(codegenFile "codegen/get-handler-select.cg"))
    ++ (concatMap (getHandlerJoinExpr m ps) rjoins)
    ++ (concatMap (getHandlerSQLExpr m ps) ps)
    ++ (getHandlerSQLReturn ps)
    ++ (getReturn ps)
    where 
        (selectFromEntity, selectFromVariable) = fromJust $ handlerSelectFrom ps
        joins = handlerJoins ps 
        rjoins = reverse joins

updateHandlerRunDB :: Module -> Resource -> [HandlerParam] -> (Int,HandlerParam) -> String
updateHandlerRunDB m r ps (pId,p) = case p of
    (Replace en fr io) -> T.unpack $(codegenFile "codegen/replace.cg")
    (Insert en io) -> T.unpack $(codegenFile "codegen/insert.cg")
    _ -> ""

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

updateHandlerDecode :: Module -> Resource -> [HandlerParam] -> (Int,HandlerParam) -> String
updateHandlerDecode m r ps (pId,p) = case p of
    (Replace en fr io) -> readInputObject (fromJust $ lookupEntity m en) io
    (Insert en io) -> readInputObject (fromJust $ lookupEntity m en) io
    where readInputObject e (Just fields) = T.unpack $(codegenFile "codegen/read-input-object-fields.cg")

          readInputObject e Nothing = T.unpack $(codegenFile "codegen/read-input-object-whole.cg")
          mapFields e fields = intercalate ",\n" $ map (mapJsonInputField fields) 
                                      [ (e,f) | f <- entityFields e ]

updateHandler :: Module -> Resource -> [HandlerParam] -> String
updateHandler m r ps = (T.unpack $(codegenFile "codegen/json-body.cg"))
            ++ (concatMap (updateHandlerDecode m r ps) $ zip [1..] ps)
            ++ (T.unpack $(codegenFile "codegen/rundb.cg"))
            ++ (concatMap (updateHandlerRunDB m r ps) $ zip [1..] ps)
            ++ (T.unpack $(codegenFile "codegen/update-handler-footer.cg"))

deleteHandlerRunDB :: Module -> Resource -> [HandlerParam] -> HandlerParam -> String
deleteHandlerRunDB m r ps p = T.unpack $ case p of
    DeleteFrom en vn Nothing -> let maybeExpr = rstrip $ T.unpack $(codegenFile "codegen/delete-all.cg") in $(codegenFile "codegen/delete.cg")
    DeleteFrom en vn (Just e) -> let maybeExpr = hsExpr ps e in $(codegenFile "codegen/delete.cg")
    _ -> ""

deleteHandler :: Module -> Resource -> [HandlerParam] -> String
deleteHandler m r ps = 
            (T.unpack $(codegenFile "codegen/rundb.cg" ))
            ++ (concatMap (deleteHandlerRunDB m r ps) ps)
            ++ (T.unpack $(codegenFile "codegen/delete-handler-footer.cg"))
handler :: Module -> Resource -> Handler -> String
handler m r (Handler ht ps) = T.unpack $(codegenFile "codegen/handler-header.cg")
    ++ if Public `elem` ps 
            then "" 
            else (T.unpack $(codegenFile "codegen/handler-requireauth.cg"))
    ++ (case ht of
            GetHandler -> getHandler m r ps
            PutHandler -> updateHandler m r ps
            PostHandler -> updateHandler m r ps
            DeleteHandler -> deleteHandler m r ps)

generate :: Module -> String
generate m = T.unpack $(codegenFile "codegen/header.cg")
         ++ (concatMap enum $ modEnums m)
         ++ models m
         ++ classes m
         ++ routes m
         ++ validation m
         ++ (T.unpack $(codegenFile "codegen/json-wrapper.cg"))
         ++ (concat [ handler m r h | r <- modResources m, h <- resHandlers r ])
                            



