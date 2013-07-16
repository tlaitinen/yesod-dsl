{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Esqueleto where
import AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import Generator.Common
import Data.String.Utils (lstrip, rstrip)
hsBinOp :: BinOp -> String
hsBinOp op = case op of
    Eq -> "==."
    Ne -> "!=."
    Lt -> "<."
    Gt -> ">."
    Le -> "<=."
    Ge -> ">=."
    Like -> "`like`"
    Ilike -> "`ilike`"

hsListOp op = case op of
    In -> "`in_`"
    NotIn -> "`notIn`"

data Context = Context {
    ctxNames :: [(EntityName, VariableName, MaybeFlag)],
    ctxModule :: Module
}

ctxLookupEntity :: Context -> VariableName -> Maybe EntityName
ctxLookupEntity ctx vn = maybe Nothing (\(en,_,_) -> Just en) $ find (\(_,vn',_) -> vn == vn') (ctxNames ctx) 

ctxLookupVariable :: Context -> EntityName -> Maybe VariableName
ctxLookupVariable ctx en = maybe Nothing (\(_,vn,_) -> Just vn) $ find (\(en',_,_) -> en == en') (ctxNames ctx) 

ctxLookupField :: Context -> VariableName -> FieldName -> Maybe Field
ctxLookupField ctx vn fn = do
    en <- ctxLookupEntity ctx vn
    lookupField (ctxModule ctx) en fn

ctxIsMaybe :: Context -> VariableName -> Bool
ctxIsMaybe ctx vn = maybe False (\(_,_,f) -> f) $ find (\(_,vn',_) -> vn == vn') (ctxNames ctx)


coerceType :: Maybe BinOp -> String -> String
coerceType (Just op)  s
    | op `elem` [Like, Ilike] = "(" ++ s ++ " :: Text)"
    | otherwise = s
coerceType _ s = s 

projectField :: MaybeFlag -> String
projectField True = " ?. "
projectField False = " ^. "

hsFieldRef :: Context -> Maybe BinOp -> FieldRef -> String
hsFieldRef ctx _ (FieldRefId vn) = vn ++ projectField (ctxIsMaybe ctx vn)
                 ++  (fromJust $ ctxLookupEntity ctx vn) ++ "Id"
hsFieldRef ctx _  (FieldRefNormal vn fn) = vn ++ projectField (ctxIsMaybe ctx vn)
                 ++ (fromJust $ ctxLookupEntity ctx vn) 
                 ++ (upperFirst fn)
hsFieldRef _ _ FieldRefAuthId = "(val authId)"
hsFieldRef _ op  (FieldRefPathParam p) = "(val " ++ coerceType op ("p" ++ show p) ++ ")"
hsFieldRef _ op FieldRefLocalParam = "(val " ++ coerceType op "localParam" ++ ")"


hsOrderBy :: Context -> (FieldRef, SortDir) -> String
hsOrderBy ctx (f,d) = dir d ++ "(" ++ hsFieldRef ctx Nothing f ++ ")"
    where dir SortAsc = "asc "
          dir SortDesc = "desc "

hsValExpr :: Context -> BinOp -> ValExpr -> String
hsValExpr ctx op ve =  case ve of
    FieldExpr fr -> hsFieldRef ctx (Just op) fr
    ConstExpr fv ->  "(val " ++ show fv ++  ")" 
    ConcatExpr e1 e2 -> "(" ++ hsValExpr ctx op e1 ++ ") ++. (" ++ hsValExpr ctx op e2 ++ ")"

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

fieldRefMaybeLevel :: Context -> FieldRef -> Int
fieldRefMaybeLevel ctx (FieldRefId vn) = boolToInt (ctxIsMaybe ctx vn)
fieldRefMaybeLevel ctx (FieldRefNormal vn fn) = boolToInt (ctxIsMaybe ctx vn) + boolToInt (fromMaybe False optional)
    where optional = ctxLookupField ctx vn fn >>= Just . fieldOptional
fieldRefMaybeLevel ctx _ = 0

exprMaybeLevel :: Context -> ValExpr -> Int
exprMaybeLevel ctx ve = case ve of
    FieldExpr fr -> fieldRefMaybeLevel ctx fr
    ConstExpr _ -> 0
    ConcatExpr e1 e2 -> max (exprMaybeLevel ctx e1) (exprMaybeLevel ctx e2)

hsListFieldRef :: Context -> FieldRef -> String
hsListFieldRef ctx (FieldRefId vn) = vn ++ " ^. " 
                 ++  (fromJust $ ctxLookupEntity ctx vn) ++ "Id"
hsListFieldRef ctx  (FieldRefNormal vn fn) = vn ++ " ^. " 
                 ++ (fromJust $ ctxLookupEntity ctx vn) 
                 ++ (upperFirst fn)
hsListFieldRef _ FieldRefAuthId = "(valList authId"
hsListFieldRef _  (FieldRefPathParam p) = "(valList p" ++ show p ++ ")"
hsListFieldRef _ FieldRefLocalParam = "(valList localParam)"
hsListFieldRef ctx (FieldRefSubQuery sq) = "(" ++ subQuery ctx sq ++ ")"

mapJoinExpr :: Module -> Context -> Join -> String
mapJoinExpr m ctx (Join _ en vn (Just expr)) = T.unpack $(codegenFile "codegen/join-expr.cg")
mapJoinExpr m _ _ = ""

selectFieldRefs :: Module -> Context -> SelectField -> [FieldRef]
selectFieldRefs m ctx (SelectAllFields vn) =  [ FieldRefNormal vn (fieldName f) | 
                                                f <- entityFields e ]
    where  
           en = fromJust $ ctxLookupEntity ctx vn
           e = fromJust $ lookupEntity m en    
selectFieldRefs m ctx (SelectField vn fn _) = [FieldRefNormal vn fn]
selectFieldRefs m ctx (SelectIdField vn _) = [FieldRefId vn ]



selectReturnFields :: Module -> Context -> SelectQuery -> String
selectReturnFields m ctx sq = T.unpack $(codegenFile "codegen/select-return-fields.cg")
    where fields = concatMap (selectFieldRefs m ctx) (sqFields sq)
        
joinDef :: Join-> String
joinDef (Join jt _ vn _) = rstrip $ T.unpack $(codegenFile "codegen/join-def.cg")



subQuery :: Context -> SelectQuery -> String
subQuery ctx' sq = "subList_select $ from $ \\(" ++ vn ++ 
    (concatMap joinDef (sqJoins sq)) ++ ") -> do { " ++
    (concatMap (makeInline . (mapJoinExpr m ctx)) (reverse (sqJoins sq)))
    ++ " ; " ++ (makeInline $ selectReturnFields m ctx sq)
    ++ " }"

    where
        makeInline = (++" ;") . lstrip . rstrip
        m = ctxModule ctx
        (en, vn) = sqFrom sq
        ctx = ctx' {
                ctxNames = sqAliases sq
            } 


hsExpr :: Context-> Expr -> String
hsExpr ctx expr = case expr of
    AndExpr e1 e2 -> "(" ++ hsExpr ctx e1 ++ ") &&. (" ++ hsExpr ctx e2 ++ ")"
    OrExpr e1 e2 ->"(" ++ hsExpr ctx e1 ++ ") ||. (" ++ hsExpr ctx e2 ++ ")"
    NotExpr e -> "not_ (" ++ hsExpr ctx e ++ ")"
    BinOpExpr e1 op e2 -> binOpExpr e1 op e2 
    ListOpExpr fr1 op fr2 -> "(" ++ hsListFieldRef ctx fr1 ++ ") " ++ hsListOp op ++ " (" ++ hsListFieldRef ctx fr2 ++ ")"
    where
        binOpExpr e1 op e2 = promoteJust e1 e2 ("(" ++ (hsValExpr ctx op e1) ++ ")") ++ " " ++ hsBinOp op ++ " " ++ (promoteJust e2 e1 ("(" ++ (hsValExpr ctx op e2) ++ ")"))
        promoteJust :: ValExpr -> ValExpr -> String -> String
        promoteJust e1 e2 content = let
            lvl1 = exprMaybeLevel ctx e1
            lvl2 = exprMaybeLevel ctx e2
            count = max (lvl2 - lvl1) 0
            in makeJust count content

