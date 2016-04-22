{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module YesodDsl.Generator.Esqueleto where
import YesodDsl.AST
import Data.Maybe
import Data.List
import YesodDsl.Generator.Common
import Data.String.Utils (lstrip, rstrip)
import Control.Monad.Reader
import qualified Data.Map as Map

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
    In -> "`in_`"
    NotIn -> "`notIn`"
    Div -> "/."
    Mul -> "*."
    Add -> "+."
    Sub -> "-."
    Concat -> "++."
    And -> "&&."
    Or -> "||."

hsUnOp :: UnOp -> String
hsUnOp op = case op of
    Not -> "not_"
    Floor -> "floor_"
    Ceiling -> "ceiling_"
    Extract fn -> "extractSubField " ++ (quote $ extractSubField fn) 

type TypeName = String
data Context = Context {
    ctxNames :: Map.Map VariableName (Entity,MaybeFlag),
    ctxExprType :: Maybe String,
    ctxExprListValue :: Bool
}
emptyContext :: Context
emptyContext = Context {
    ctxNames = Map.empty,
    ctxExprType = Nothing,
    ctxExprListValue = False
}

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

annotateType :: Bool -> Maybe String -> String -> String
annotateType listValue (Just exprType)  s = "(" ++ s ++ " :: " ++ (if listValue then "[" ++ exprType ++ "]" else exprType) ++ ")"
annotateType _ Nothing s = s 

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

valueOrValueList :: Bool -> Int -> String
valueOrValueList listValue promoteJust = if listValue
        then "valList" ++ (if promoteJust > 0 then " $ map Just" else "")
        else "val" ++ (if promoteJust > 0 then " $ Just" else "")
normalFieldRef :: MaybeLevel -> String -> Reader Context String
normalFieldRef ml content = do
    lv <- asks ctxExprListValue
    et <- asks ctxExprType
    
    return $ brackets (isJust et) $ valueOrValueList lv ml ++" " ++ annotateType  lv et content

hsFieldRef :: MaybeLevel -> FieldRef -> Reader Context String
hsFieldRef ml (SqlId (Var vn (Right e) mf)) = do
    return $ makeJust ml $ vn ++ projectField mf ++ entityName e ++ "Id"
hsFieldRef ml (SqlField (Var vn (Right e) mf) fn) = do
    return $ makeJust ml $ vn ++ projectField mf ++ entityName e ++ (upperFirst fn)
hsFieldRef ml AuthId = do
    lv <- asks ctxExprListValue
    return $ valueOrValueList lv ml ++ " authId"
hsFieldRef ml (PathParam p) = normalFieldRef ml $ "p" ++ show p
hsFieldRef ml LocalParam   = normalFieldRef ml "localParam"
hsFieldRef ml (LocalParamField (Var vn (Right e) _) fn) = normalFieldRef ml $ entityName e ++ upperFirst fn ++ " $ result_" ++ vn
hsFieldRef ml (RequestField fn)  = normalFieldRef ml $ "attr_" ++ fn
hsFieldRef ml (NamedLocalParam vn) = normalFieldRef ml $ "result_" ++ vn
hsFieldRef ml (Const (fv@(NothingValue))) = return $  makeJust ml $ fieldValueToEsqueleto fv
hsFieldRef ml (Const fv) = return $ makeJust ml $ "(val " ++ fieldValueToEsqueleto fv ++  ")" 
hsFieldRef _ fr = return $ show fr
hsOrderBy :: (Maybe FunctionName, [FieldRef], SortDir) -> Reader Context String
hsOrderBy ob = case ob of
    (Nothing, fs,d) -> simple fs d
    (Just fn, fs, d) -> aggr fn fs d
    where 
        dir SortAsc = "asc "
        dir SortDesc = "desc "
        simple fs d = do
            contents <- forM fs $ hsFieldRef 0 
            return $ intercalate ", " [ dir d ++ "(" ++ content ++ ")" | content <- contents ]
        aggr fn fs d = do
            contents <- forM fs $ hsFieldRef 0
            return $ dir d ++ "(" ++ fn ++ " (" ++ (intercalate ") (" contents) ++ "))" 

        
hsExpr :: MaybeLevel -> Expr -> Reader Context String
hsExpr ml ve = do
    c <- content
    maybePromoteJust c
    where 
        maybePromoteJust c = case ve of
            SubQueryExpr _ -> return c
            FieldExpr fr -> case fr of
                _ -> return c
            _ -> do
                return $ makeJust ml c
        content = case ve of
            FieldExpr fr -> hsFieldRef ml fr
            ConcatManyExpr ves -> local noListValue $ do
                rs <- mapM (hsExpr 0) ves
                return $ "(concat_ [" ++ intercalate ", " rs ++ "])"
            BinOpExpr e1 op e2 -> if op `elem` [ Add , Sub , Div , Mul , Concat, And, Or ] 
                then local noListValue $ do
                    r1 <- hsExpr 0 e1
                    r2 <- hsExpr 0 e2
                    return $ "(" ++ r1 ++ ") " ++ hsBinOp op ++ " (" ++ r2 ++ ")" 
                else do
                    let e1m = exprMaybeLevel e1
                        e2m = exprMaybeLevel e2
                        e1rt = exprReturnType e1
                        e2rt = exprReturnType e2
                    r1 <- local 
                            (\ctx -> ctx { 
                                ctxExprType = e2rt
                            } )
                            (hsExpr (max 0 $ e2m - e1m) e1)
                    r2 <- local 
                            (\ctx -> ctx { 
                                ctxExprType = case op of
                                    Ilike -> Just "Text"
                                    Like -> Just "Text"
                                    _ -> e1rt,
                                ctxExprListValue = op `elem` [In, NotIn]
                            }) 
                           (hsExpr (max 0 $ e1m - e2m) e2)
                    return $ "(" ++ r1 ++ ") " ++ hsBinOp op ++ " (" ++ r2 ++ ")"
         
            SubQueryExpr sq -> local noListValue $ subQuery "subList_select" sq
            UnOpExpr op e -> local noListValue $ do
                r <- hsExpr 0 e
                return $ "(" ++ hsUnOp op ++ " $ " ++ r ++ ")"
            ExistsExpr sq -> local noListValue $ subQuery "exists" sq
            ExternExpr ee ps -> local noListValue $ do
                ps' <- mapM externExprParam ps
                return $ intercalate " " $ [ee] ++ map ((++ ")"). ("("++)) ps'
        externExprParam (FieldRefParam fr) = hsFieldRef 0 fr
        externExprParam (VerbatimParam v) = return v 
        noListValue ctx = ctx { ctxExprListValue = False }
fieldRefMaybeLevel :: FieldRef -> Int
fieldRefMaybeLevel (SqlId (Var _ _ mf)) = boolToInt mf
fieldRefMaybeLevel (SqlField (Var _ (Right e) mf) fn) = boolToInt mf + (fromMaybe 0 $ lookupField e fn >>= \f -> Just $ boolToInt $ fieldOptional f)
fieldRefMaybeLevel (Const NothingValue) =  1
fieldRefMaybeLevel _ =  0


exprMaybeLevel :: Expr -> Int
exprMaybeLevel ve = case ve of
    FieldExpr fr -> fieldRefMaybeLevel fr
    ConcatManyExpr _ -> 0
    BinOpExpr e1 _ e2 -> 0
    UnOpExpr _ e -> 0
    SubQueryExpr sq -> fromMaybe 0 $ listToMaybe $ map exprMaybeLevel $ concatMap (selectFieldExprs) $ sqFields sq
    _ -> 0
    where
        selectFieldExprs sf = case sf of
            (SelectField vn fn _) -> [ FieldExpr $ SqlField vn fn]
            (SelectIdField vn _) -> [ FieldExpr $ SqlId vn ]
            (SelectExpr ve' _) -> [ ve' ]
            _  -> []
 
exprReturnType :: Expr -> Maybe String
exprReturnType e = case e of
    UnOpExpr Floor _ -> Just "Double"
    UnOpExpr Ceiling _ -> Just "Double"
    UnOpExpr (Extract _) _ -> Just "Double"
    _ -> Nothing

mapJoinExpr :: Join -> Reader Context String
mapJoinExpr (Join _ _ _ (Just expr)) = do
    e <- hsExpr 0 expr
    return $ "on (" ++ e ++ ")\n"
mapJoinExpr _  = return ""


selectReturnFields :: SelectQuery -> Reader Context String
selectReturnFields sq = do
    ves <- forM (sqFields sq) $ \sf -> case sf of
        SelectField vn fn _ -> hsExpr 0 $ FieldExpr $ SqlField vn fn
        SelectIdField vn _  -> hsExpr 0 $ FieldExpr $ SqlId vn
        SelectExpr ve _  -> hsExpr 0 ve
        _ -> return ""
    return $ "return (" ++ (intercalate ", " ves) ++ ")"
    where
joinDef :: Join-> String
joinDef (Join jt _ vn _) = "`" ++ show jt ++ "` " ++ vn

subQuery :: String -> SelectQuery -> Reader Context String
subQuery sqFunc sq = withScope (sqAliases sq) $ do
    jes <- liftM (concat . (map makeInline)) $ mapM mapJoinExpr (reverse $ sqJoins sq)
    rfs <- selectReturnFields sq
    maybeWhere <- case sqWhere sq of
        Just expr -> do
            e <- hsExpr 0 expr
            return $ "where_ (" ++ e ++ ")"
        Nothing -> return ""
    return $ sqFunc ++ " $ from $ \\(" ++ vn ++ 
        (concatMap joinDef (sqJoins sq)) ++ ") -> do { " ++
        jes
        ++ " ; " ++ maybeWhere
        ++ " ; " ++ (makeInline $ rfs)
        ++ " }" 

    where
        makeInline = (++" ;") . lstrip . rstrip
        (_, vn) = sqFrom sq

withScope :: Map.Map VariableName (Entity, MaybeFlag) -> Reader Context a -> Reader Context a 
withScope names = local $ \ctx -> ctx { ctxNames = Map.union names $ ctxNames ctx }


scopedExpr :: Map.Map VariableName (Entity, MaybeFlag) -> Expr -> String
scopedExpr names e = runReader (hsExpr 0 e) $ emptyContext { ctxNames = names }

