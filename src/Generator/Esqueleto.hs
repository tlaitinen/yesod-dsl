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
import Control.Monad.State

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

data Context = Context {
    ctxNames :: [(EntityName, VariableName, MaybeFlag)],
    ctxModule :: Module,
    ctxRoute :: Maybe Route,
    ctxHandlerParams :: [HandlerParam],
    ctxExprType :: Maybe String,
    ctxExprJust :: Bool,
    ctxExprListValue :: Bool
}

emptyContext :: Module -> Context
emptyContext m = Context {
    ctxNames = [],
    ctxModule = m,
    ctxRoute = Nothing,
    ctxHandlerParams = [],
    ctxExprType = Nothing,
    ctxExprJust = False,
    ctxExprListValue = False
}

ctxLookupEntity :: VariableName -> State Context (Maybe EntityName)
ctxLookupEntity vn = do
    names <- gets ctxNames
    return $ maybe Nothing (\(en,_,_) -> Just en) $ find (\(_,vn',_) -> vn == vn') names

ctxLookupVariable :: EntityName -> State Context (Maybe VariableName)
ctxLookupVariable en = do
    names <- gets ctxNames
    return $ maybe Nothing (\(_,vn,_) -> Just vn) $ find (\(en',_,_) -> en == en') names

ctxLookupField :: VariableName -> FieldName -> State Context (Maybe Field)
ctxLookupField vn fn = do
    m <- gets ctxModule
    men <- ctxLookupEntity vn
    return $ men >>= \en -> lookupField m en fn

ctxIsMaybe :: VariableName -> State Context Bool
ctxIsMaybe vn = do
    names <- gets ctxNames
    return $ maybe False (\(_,_,f) -> f) $ find (\(_,vn',_) -> vn == vn') names

annotateType :: Maybe String -> String -> String
annotateType (Just exprType)  s = "(" ++ s ++ " :: " ++ exprType ++ ")"
annotateType Nothing s = s 

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

valueOrValueList :: Bool -> Bool -> String
valueOrValueList listValue promoteJust = if listValue
        then "valList" ++ if promoteJust then " $ map Just" else ""
        else "val" ++ if listValue then " $ just" else ""
normalFieldRef :: String -> State Context String
normalFieldRef content = do
    j <- gets ctxExprJust
    lv <- gets ctxExprListValue
    et <- gets ctxExprType
    return $ "(" ++ valueOrValueList lv j ++" " ++ annotateType et content ++ ")"

hsFieldRef :: FieldRef -> State Context String
hsFieldRef (FieldRefId vn) = do
    j <- gets ctxExprJust
    m <- ctxIsMaybe vn
    men <- ctxLookupEntity vn 
    return $ makeJust j $ vn ++ projectField m ++ fromMaybe "(Nothing)" men ++ "Id"
hsFieldRef (FieldRefNormal vn fn) = do
    j <- gets ctxExprJust
    m <- ctxIsMaybe vn
    men <- ctxLookupEntity vn
    return $ makeJust j $ vn ++ projectField m ++ fromMaybe "(Nothing)" men
                 ++ (upperFirst fn)
hsFieldRef FieldRefAuthId = do
    j <- gets ctxExprJust
    lv <- gets ctxExprListValue
    return $ "(" ++ valueOrValueList lv j ++ " authId)"
hsFieldRef (FieldRefPathParam p) = normalFieldRef $ "p" ++ show p
hsFieldRef FieldRefLocalParam    = normalFieldRef $ "localParam"
hsFieldRef (FieldRefRequest fn)  = normalFieldRef $ "attr_" ++ fn
hsFieldRef (FieldRefEnum en vn)  = normalFieldRef $ en ++ vn
hsOrderBy :: (FieldRef, SortDir) -> State Context String
hsOrderBy (f,d) = do
    content <- hsFieldRef f
    return $ dir d ++ "(" ++ content ++ ")"
    where dir SortAsc = "asc "
          dir SortDesc = "desc "

hsValBinOp :: ValBinOp -> String
hsValBinOp vo = case vo of
    Div -> "/."
    Mul -> "*."
    Add -> "+."
    Sub -> "-."
          
hsValExpr :: ValExpr -> State Context String
hsValExpr ve = do
    c <- content
    maybePromoteJust c
    where 
        maybePromoteJust c = case ve of
            SubQueryExpr _ -> return c
            FieldExpr _ -> return c
            _ -> do
                j <- gets ctxExprJust
                return $ makeJust j c
        content = case ve of
            FieldExpr fr -> do
                r <- hsFieldRef fr
                return $ "(" ++ r ++ ")"
            ConstExpr (fv@(NothingValue)) -> return $ fieldValueToEsqueleto fv
            ConstExpr fv -> return $ "(val " ++ fieldValueToEsqueleto fv ++  ")" 
            ConcatExpr e1 e2 -> do
                r1 <- hsValExpr e1
                r2 <- hsValExpr e2
                return $ "(" ++ r1 ++ ") ++. (" ++ r2 ++ ")"
            ConcatManyExpr ves -> do
                rs <- mapM hsValExpr ves
                return $ "(concat_ [" ++ intercalate ", " rs ++ "])"
            ValBinOpExpr e1 vop e2 -> do
                r1 <- hsValExpr e1
                r2 <- hsValExpr e2
                return $ "(" ++ r1 ++ ") " ++ hsValBinOp vop ++ " (" ++ r2 ++ ")"
            RandomExpr -> return "random_"
            FloorExpr ve -> do
                r <- hsValExpr ve
                return $ "(floor_ " ++ r ++ ")"
            CeilingExpr ve -> do
                r <- hsValExpr ve
                return $ "(ceiling_" ++ r ++ ")"
            ExtractExpr fn ve -> do
                r <- hsValExpr ve
                return $ "(extractSubField " ++ (quote $ extractSubField fn) ++ " " ++ r++ ")"
            SubQueryExpr sq -> subQuery sq
{-
fieldRefMaybeLevel :: Context -> FieldRef -> Int
fieldRefMaybeLevel ctx (FieldRefId vn) = boolToInt (ctxIsMaybe ctx vn)
fieldRefMaybeLevel ctx (FieldRefNormal vn fn) = boolToInt (ctxIsMaybe ctx vn) + boolToInt (fromMaybe False optional)
    where optional = ctxLookupField ctx vn fn >>= Just . fieldOptional
 
fieldRefMaybeLevel ctx _ = 0
-}

{-
exprMaybeLevel :: Context -> ValExpr -> Int
exprMaybeLevel ctx ve = case ve of
    FieldExpr fr -> fieldRefMaybeLevel ctx fr
    ConstExpr NothingValue -> 1
    ConstExpr _ -> 0
    ConcatExpr e1 e2 -> max (exprMaybeLevel ctx e1) (exprMaybeLevel ctx e2)
    ConcatManyExpr ves -> maximum $ map (exprMaybeLevel ctx) ves
    ValBinOpExpr ve1 _ ve2 -> max (exprMaybeLevel ctx ve1) (exprMaybeLevel ctx ve2)
    FloorExpr ve -> exprMaybeLevel ctx ve
    CeilingExpr ve -> exprMaybeLevel ctx ve
    ExtractExpr _ ve -> exprMaybeLevel ctx ve
    SubQueryExpr sq -> fromMaybe 0 $ listToMaybe $ map (exprMaybeLevel ctx') fieldExprs
        where fieldExprs = concatMap (selectFieldExprs m ctx') (sqFields sq)
              m = ctxModule ctx
              ctx' = ctx {
                   ctxNames = sqAliases sq
              } 
-}

mapJoinExpr :: Join -> State Context String
mapJoinExpr (Join _ en vn (Just expr)) = do
    e <- hsBoolExpr expr
    return $ "on (" ++ e ++ ")"
mapJoinExpr _  = return ""

selectFieldExprs :: SelectField -> State Context [ValExpr]
selectFieldExprs sf = do
    ctx <- get
    m <- gets ctxModule

    case sf of
        (SelectAllFields vn) -> do
            men <- ctxLookupEntity vn
            let me = men >>= \en -> lookupEntity m en
            return $ case me of
                Just e -> [ FieldExpr $ FieldRefNormal vn (fieldName f) 
                    |  f <- entityFields e,
                       fieldInternal f == False ]
                Nothing -> []
        (SelectField vn fn _) -> return [ FieldExpr $ FieldRefNormal vn fn]
        (SelectIdField vn _) -> return [ FieldExpr $ FieldRefId vn ]
        (SelectValExpr ve _) -> return [ ve ]

selectReturnFields :: SelectQuery -> State Context String
selectReturnFields sq = do
    fieldExprs <- liftM concat $ mapM selectFieldExprs (sqFields sq)
    ves <- mapM hsValExpr fieldExprs
    return $ "return (" ++ (intercalate ", " ves) ++ ")"
        
joinDef :: Join-> String
joinDef (Join jt _ vn _) = "`" ++ show jt ++ "` " ++ vn

subQuery :: SelectQuery -> State Context String
subQuery sq = do
    ctx <- get
    put $ ctx { ctxNames = sqAliases sq }
    jes <- liftM concat $ mapM mapJoinExpr (reverse $ sqJoins sq)
    rfs <- selectReturnFields sq
    maybeWhere <- case sqWhere sq of
        Just expr -> do
            e <- hsBoolExpr expr
            return $ "where_ (" ++ e ++ ")"
        Nothing -> return ""
    put $ ctx
    return $ "subList_select $ from $ \\(" ++ vn ++ 
        (concatMap joinDef (sqJoins sq)) ++ ") -> do { " ++
        jes
        ++ " ; " ++ maybeWhere
        ++ " ; " ++ (makeInline $ rfs)
        ++ " }" 

    where
        makeInline = (++" ;") . lstrip . rstrip
        (en, vn) = sqFrom sq

hsBoolExpr :: BoolExpr -> State Context String
hsBoolExpr expr = case expr of
    AndExpr e1 e2 -> do
        r1 <- hsBoolExpr e1
        r2 <- hsBoolExpr e2
        return $ "(" ++ r1 ++ ") &&. (" ++ r2 ++ ")"
    OrExpr e1 e2 -> do
        r1 <- hsBoolExpr e1
        r2 <- hsBoolExpr e2 
        return $ "(" ++ r1 ++ ") ||. (" ++ r2 ++ ")"
    NotExpr e -> do
        r <- hsBoolExpr e
        return $ "not_ (" ++ r ++ ")"
    BinOpExpr e1 op e2 -> do
        r1 <- hsValExpr e1
        r2 <- hsValExpr e2
        return $ "(" ++ r1 ++ ") " ++ hsBinOp op ++ " (" ++ r2 ++ ")"
