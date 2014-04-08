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
    Is -> "`is`"

hsListOp op = case op of
    In -> "`in_`"
    NotIn -> "`notIn`"

data Context = Context {
    ctxNames :: [(EntityName, VariableName, MaybeFlag)],
    ctxModule :: Module,
    ctxHandlerParams :: [HandlerParam]
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

extractSubField :: FieldName -> String
extractSubField fn = case fn of
    "century" -> "CENTURY"
    "day"     -> "DAY"
    "decade"  -> "DECADE"
    "dow"     -> "DOW"
    "doy"     -> "DOY"
    "epoch"   -> "EPOCH"
    "hour"    -> "HOUR"
    "isodow"  -> "ISODOW"
    "microseconds" -> "MICROSECONDS"
    "millennium" -> "MILLENNIUM"
    "millseconds" -> "MILLISECONDS"
    "minute"    -> "MINUTE"
    "month"     -> "MONTH"
    "quarter"   -> "QUARTER"
    "second"    -> "SECOND"
    "timezone"  -> "TIMEZONE"
    "timezone_hour" -> "TIMEZONE_HOUR"
    "timezone_minute" -> "TIMEZONE_MINUTE"
    "week"      -> "WEEK"
    "year"      -> "YEAR"
    fn' -> error $ "Unknown subfield : " ++ fn'
hsFieldRef :: Context -> Maybe BinOp -> FieldRef -> String
hsFieldRef ctx _ (FieldRefId vn) = vn ++ projectField (ctxIsMaybe ctx vn)
                 ++  (fromJust $ ctxLookupEntity ctx vn) ++ "Id"
hsFieldRef ctx _  (FieldRefNormal vn fn) = vn ++ projectField (ctxIsMaybe ctx vn)
                 ++ (fromJust $ ctxLookupEntity ctx vn) 
                 ++ (upperFirst fn)
hsFieldRef ctx _ (FieldRefExtract efn vn fn) = "(extractSubField " ++ (quote $ extractSubField efn) ++ " $ " ++ (hsFieldRef ctx Nothing (FieldRefNormal vn fn)) ++ ")"
hsFieldRef _ _ FieldRefAuthId = "(val authId)"
hsFieldRef _ op  (FieldRefPathParam p) = "(val " ++ coerceType op ("p" ++ show p) ++ ")"
hsFieldRef _ op FieldRefLocalParam = "(val " ++ coerceType op "localParam" ++ ")"
hsFieldRef _ _ (FieldRefRequest fn) = "(val attr_" ++ fn ++ ")"
hsFieldRef _ _ (FieldRefEnum en vn) = "(val " ++ en ++ vn ++ ")"
hsOrderBy :: Context -> (FieldRef, SortDir) -> String
hsOrderBy ctx (f,d) = dir d ++ "(" ++ hsFieldRef ctx Nothing f ++ ")"
    where dir SortAsc = "asc "
          dir SortDesc = "desc "

hsValBinOp :: ValBinOp -> String
hsValBinOp vo = case vo of
    Div -> "/."
    Mul -> "*."
    Add -> "+."
    Sub -> "-."
          
hsValExpr :: Context -> Maybe BinOp -> ValExpr -> String
hsValExpr ctx op ve =  case ve of
    FieldExpr fr -> hsFieldRef ctx op fr
    ConstExpr (fv@(NothingValue)) -> fieldValueToEsqueleto fv
    ConstExpr fv ->  "(val " ++ fieldValueToEsqueleto fv ++  ")" 
    ConcatExpr e1 e2 -> "(" ++ hsValExpr ctx op e1 ++ ") ++. (" ++ hsValExpr ctx op e2 ++ ")"
    ConcatManyExpr ves -> "(concat_ [" ++ (intercalate ", " $ map (hsValExpr ctx op) ves) ++ "])"
    ValBinOpExpr e1 vop e2 -> "(" ++ hsValExpr ctx op e1 ++ ") " ++ hsValBinOp vop ++ " (" ++ hsValExpr ctx op e2 ++ ")"


boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

fieldRefMaybeLevel :: Context -> FieldRef -> Int
fieldRefMaybeLevel ctx (FieldRefId vn) = boolToInt (ctxIsMaybe ctx vn)
fieldRefMaybeLevel ctx (FieldRefNormal vn fn) = boolToInt (ctxIsMaybe ctx vn) + boolToInt (fromMaybe False optional)
    where optional = ctxLookupField ctx vn fn >>= Just . fieldOptional
fieldRefMaybeLevel ctx (FieldRefSubQuery sq) = fromMaybe 0 $ listToMaybe $ map (exprMaybeLevel ctx') fieldExprs
    where fieldExprs = concatMap (selectFieldExprs m ctx') (sqFields sq)
          m = ctxModule ctx
          ctx' = ctx {
               ctxNames = sqAliases sq
          } 

 
fieldRefMaybeLevel ctx _ = 0

exprMaybeLevel :: Context -> ValExpr -> Int
exprMaybeLevel ctx ve = case ve of
    FieldExpr fr -> fieldRefMaybeLevel ctx fr
    ConstExpr NothingValue -> 1
    ConstExpr _ -> 0
    ConcatExpr e1 e2 -> max (exprMaybeLevel ctx e1) (exprMaybeLevel ctx e2)

hsListFieldRef :: Context -> FieldRef -> Int -> String
hsListFieldRef ctx (FieldRefId vn) mkJust = makeJust mkJust $ vn 
                 ++ projectField (ctxIsMaybe ctx vn)
                 ++  (fromJust $ ctxLookupEntity ctx vn) ++ "Id"
hsListFieldRef ctx  (FieldRefNormal vn fn) mkJust = makeJust mkJust $ vn 
                 ++ projectField (ctxIsMaybe ctx vn)
                 ++ (fromJust $ ctxLookupEntity ctx vn) 
                 ++ (upperFirst fn)
hsListFieldRef _ FieldRefAuthId mkJust = "(valList authId)"
hsListFieldRef _  (FieldRefPathParam p) mkJust =  "(valList p" ++ show p ++ ")"
hsListFieldRef _ FieldRefLocalParam mkJust = if mkJust > 0
    then "(valList $ map Just localParam)"
    else "(valList localParam)"
hsListFieldRef _ (FieldRefRequest fn) mkJust = let content = "(fromMaybe [] $ attr_" ++ fn ++ ")" in if mkJust > 0 then "(valList $ map Just " ++ content ++ ")" else "(valList " ++ content ++ ")"
hsListFieldRef ctx (FieldRefSubQuery sq) mkJust = "(" ++ subQuery ctx sq mkJust ++ ")"

mapJoinExpr :: Module -> Context -> Join -> String
mapJoinExpr m ctx (Join _ en vn (Just expr)) = T.unpack $(codegenFile "codegen/join-expr.cg")
mapJoinExpr m _ _ = ""

selectFieldExprs :: Module -> Context -> SelectField -> [ValExpr]
selectFieldExprs m ctx (SelectAllFields vn) =  [ FieldExpr $ FieldRefNormal vn (fieldName f) | 
                                                f <- entityFields e,
                                                fieldInternal f == False ]
    where  
           en = fromJust $ ctxLookupEntity ctx vn
           e = fromJust $ lookupEntity m en    
selectFieldExprs m ctx (SelectField vn fn _) = [ FieldExpr $ FieldRefNormal vn fn]
selectFieldExprs m ctx (SelectIdField vn _) = [ FieldExpr $ FieldRefId vn ]
selectFieldExprs m ctx (SelectValExpr ve _) = [ ve ]

selectReturnFields :: Module -> Context -> SelectQuery -> Int -> String
selectReturnFields m ctx sq mkJust = T.unpack $(codegenFile "codegen/select-return-fields.cg")
    where fieldExprs = concatMap (selectFieldExprs m ctx) (sqFields sq)
        
joinDef :: Join-> String
joinDef (Join jt _ vn _) = rstrip $ T.unpack $(codegenFile "codegen/join-def.cg")



subQuery :: Context -> SelectQuery -> Int ->  String
subQuery ctx' sq mkJust = "subList_select $ from $ \\(" ++ vn ++ 
    (concatMap joinDef (sqJoins sq)) ++ ") -> do { " ++
    (concatMap (makeInline . (mapJoinExpr m ctx)) (reverse (sqJoins sq)))
    ++ " ; " ++ maybeWhere 
    ++ " ; " ++ (makeInline $ selectReturnFields m ctx sq mkJust)
    ++ " }"

    where
        makeInline = (++" ;") . lstrip . rstrip
        m = ctxModule ctx
        (en, vn) = sqFrom sq
        ctx = ctx' {
                ctxNames = sqAliases sq
            } 
        maybeWhere = case sqWhere sq of
            Just expr -> (makeInline $ T.unpack $(codegenFile "codegen/where-expr.cg"))
            Nothing -> ""


hsExpr :: Context-> Expr -> String
hsExpr ctx expr = case expr of
    AndExpr e1 e2 -> "(" ++ hsExpr ctx e1 ++ ") &&. (" ++ hsExpr ctx e2 ++ ")"
    OrExpr e1 e2 ->"(" ++ hsExpr ctx e1 ++ ") ||. (" ++ hsExpr ctx e2 ++ ")"
    NotExpr e -> "not_ (" ++ hsExpr ctx e ++ ")"
    BinOpExpr e1 op e2 -> binOpExpr e1 op e2 
    ListOpExpr fr1 op fr2 -> listOpExpr fr1 op fr2 
    where
        listOpExpr fr1 op fr2 = ("(" ++ hsListFieldRef ctx fr1 (justLevel2 fr1 fr2) ++ ")") ++ " " ++ hsListOp op ++ " " ++ ("(" ++ hsListFieldRef ctx fr2 (justLevel2 fr2 fr1) ++ ")")
        justLevel2 :: FieldRef -> FieldRef -> Int
        justLevel2 fr1 fr2 = max (fieldRefMaybeLevel ctx fr2
                                  - fieldRefMaybeLevel ctx fr1) 0
    
        binOpExpr e1 op e2 = promoteJust e1 e2 ("(" ++ (hsValExpr ctx (Just op) e1) ++ ")") ++ " " ++ hsBinOp op ++ " " ++ (promoteJust e2 e1 ("(" ++ (hsValExpr ctx (Just op) e2) ++ ")"))
        promoteJust :: ValExpr -> ValExpr -> String -> String
        promoteJust e1 e2 content = let
            lvl1 = exprMaybeLevel ctx e1
            lvl2 = exprMaybeLevel ctx e2
            count = max (lvl2 - lvl1) 0
            in makeJust count content

