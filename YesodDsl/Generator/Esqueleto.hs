{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module YesodDsl.Generator.Esqueleto where
import YesodDsl.AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import YesodDsl.Generator.Common
import Data.String.Utils (lstrip, rstrip)
import Control.Monad.State
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

type TypeName = String
data Context = Context {
    ctxNames :: [(EntityName, VariableName, MaybeFlag)],
    ctxModule :: Module,
    ctxHandlerParams :: [HandlerParam],
    ctxExprType :: Maybe String,
    ctxExprMaybeLevel :: Int,
    ctxExprListValue :: Bool
}
emptyContext :: Module -> Context
emptyContext m = Context {
    ctxNames = [],
    ctxModule = m,
    ctxHandlerParams = [],
    ctxExprType = Nothing,
    ctxExprMaybeLevel = 0,
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

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

ctxMaybeLevel :: VariableName -> State Context Int
ctxMaybeLevel vn = do
    names <- gets ctxNames
    return $ boolToInt $ maybe False (\(_,_,f) -> f) $ find (\(_,vn',_) -> vn == vn') names

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
normalFieldRef :: String -> State Context String
normalFieldRef content = do
    j <- gets ctxExprMaybeLevel
    lv <- gets ctxExprListValue
    et <- gets ctxExprType
    
    return $ brackets (isJust et) $ valueOrValueList lv j ++" " ++ annotateType  lv et content

hsFieldRef :: FieldRef -> State Context String
hsFieldRef (FieldRefId vn) = do
    j <- gets ctxExprMaybeLevel
    m <- ctxMaybeLevel vn
    men <- ctxLookupEntity vn 
    return $ makeJust j $ vn ++ projectField (m > 0) ++ fromMaybe "(Nothing)" men ++ "Id"
hsFieldRef (FieldRefNormal vn fn) = do
    j <- gets ctxExprMaybeLevel
    m <- ctxMaybeLevel vn
    men <- ctxLookupEntity vn
    return $ makeJust j $ vn ++ projectField (m > 0) ++ fromMaybe "(Nothing)" men
                 ++ (upperFirst fn)
hsFieldRef FieldRefAuthId = do
    j <- gets ctxExprMaybeLevel
    lv <- gets ctxExprListValue
    return $ valueOrValueList lv j ++ " authId"
hsFieldRef (FieldRefPathParam p) = normalFieldRef $ "p" ++ show p
hsFieldRef FieldRefLocalParam    = normalFieldRef $ "localParam"
hsFieldRef (FieldRefRequest fn)  = normalFieldRef $ "attr_" ++ fn
hsFieldRef (FieldRefEnum en vn)  = normalFieldRef $ en ++ vn
hsFieldRef (FieldRefNamedLocalParam vn) = normalFieldRef $ "result_" ++ vn
hsFieldRef (FieldRefParamField vn pn) = return $ "{- param field " ++ vn ++ " " ++ pn ++ "-}"
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
    Concat -> "++."

resetMaybe :: State Context String -> State Context String
resetMaybe = localCtx $ \ctx -> ctx { ctxExprMaybeLevel = 0 }
    
    
hsValExpr :: ValExpr -> State Context String
hsValExpr ve = do
    c <- content
    maybePromoteJust c
    where 
        maybePromoteJust c = case ve of
            SubQueryExpr _ -> return c
            FieldExpr _ -> return c
            _ -> do
                j <- gets ctxExprMaybeLevel
                return $ makeJust j c
        content = case ve of
            FieldExpr fr -> hsFieldRef fr
            ConstExpr (fv@(NothingValue)) -> do
                return $ fieldValueToEsqueleto fv
            ConstExpr fv -> do
                return $ "(val " ++ fieldValueToEsqueleto fv ++  ")" 
            ConcatManyExpr ves -> resetMaybe $ do
                rs <- mapM hsValExpr ves
                return $ "(concat_ [" ++ intercalate ", " rs ++ "])"
            ValBinOpExpr e1 vop e2 -> resetMaybe $ do
                r1 <- hsValExpr e1
                r2 <- hsValExpr e2
                return $ "(" ++ r1 ++ ") " ++ hsValBinOp vop ++ " (" ++ r2 ++ ")"
            RandomExpr -> return "random_"
            FloorExpr ve -> resetMaybe $ do
                r <- hsValExpr ve
                return $ "(floor_ $  " ++ r ++ ")"
            CeilingExpr ve -> resetMaybe $ do
                r <- hsValExpr ve
                return $ "(ceiling_ $ " ++ r ++ ")"
            ExtractExpr fn ve -> resetMaybe $ do
                r <- hsValExpr ve
                return $ "(extractSubField " ++ (quote $ extractSubField fn) ++ " $ " ++ r++ ")"
            SubQueryExpr sq -> subQuery "subList_select" sq

fieldRefMaybeLevel :: FieldRef -> State Context Int
fieldRefMaybeLevel (FieldRefId vn) = ctxMaybeLevel vn
fieldRefMaybeLevel (FieldRefNormal vn fn) = do
    m <- ctxMaybeLevel vn
    mf <- ctxLookupField vn fn
    return $ m + (boolToInt $ fromMaybe False $ mf >>= return . fieldOptional)
fieldRefMaybeLevel _ = return 0

exprMaybeLevel :: ValExpr -> State Context Int
exprMaybeLevel ve = case ve of
    FieldExpr fr -> fieldRefMaybeLevel fr
    ConstExpr NothingValue -> return 1
    ConstExpr _ -> return 0
    ConcatManyExpr ves -> do
        es <- mapM exprMaybeLevel ves
        return $ maximum es
    ValBinOpExpr e1 _ e2 -> do
        e1m <- exprMaybeLevel e1
        e2m <- exprMaybeLevel e2
        return $ max e1m e2m
    FloorExpr e -> exprMaybeLevel e
    CeilingExpr e -> exprMaybeLevel e
    ExtractExpr _ e -> exprMaybeLevel e
    SubQueryExpr sq -> do
        ctx <- get
        withScope (sqAliases sq) $ do
            fs <- liftM concat $ mapM selectFieldExprs $ sqFields sq
            mls <- mapM exprMaybeLevel fs
            return $ fromMaybe 0 $ listToMaybe mls

exprReturnType :: ValExpr -> State Context (Maybe String)
exprReturnType e = return $ case e of
    FloorExpr _ -> Just "Double"
    CeilingExpr _ -> Just "Double"
    ExtractExpr _ _ -> Just "Double"
    _ -> Nothing

mapJoinExpr :: Join -> State Context String
mapJoinExpr (Join _ en vn (Just expr)) = do
    e <- hsBoolExpr expr
    return $ "on (" ++ e ++ ")\n"
mapJoinExpr _  = return ""

selectFieldExprs :: SelectField -> State Context [ValExpr]
selectFieldExprs sf = do
    ctx <- get
    m <- gets ctxModule

    case sf of
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

subQuery :: String -> SelectQuery -> State Context String
subQuery sqFunc sq = withScope (sqAliases sq) $ do
    jes <- liftM (concat . (map makeInline)) $ mapM mapJoinExpr (reverse $ sqJoins sq)
    rfs <- selectReturnFields sq
    maybeWhere <- case sqWhere sq of
        Just expr -> do
            e <- hsBoolExpr expr
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
        (en, vn) = sqFrom sq

withScope :: [(Entity, VariableName, MaybeFlag)] -> State Context a -> State Context a 
withScope names = localCtx (\ctx -> ctx { ctxNames = ctxNames ctx ++ [ (entityName e, vn, mf) | (e, vn, mf) <- names ] })


localCtx :: (Context -> Context) -> (State Context a) -> (State Context a)
localCtx f st = do
    ctx <- get
    put $ f ctx
    r <- st
    ctx' <- get
    put ctx
    return r



hsBoolExpr :: BoolExpr -> State Context String
hsBoolExpr expr = localCtx (\ctx -> ctx { ctxExprListValue = False}) $
    case expr of
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
            e1m <- exprMaybeLevel e1
            e2m <- exprMaybeLevel e2
            e1rt <- exprReturnType e1
            e2rt <- exprReturnType e2
            r1 <- localCtx 
                    (\ctx -> ctx { 
                        ctxExprMaybeLevel = max 0 $ e2m - e1m ,
                        ctxExprType = e2rt
                    } )
                    (hsValExpr e1)
            r2 <- localCtx 
                    (\ctx -> ctx { 
                        ctxExprType = case op of
                            Ilike -> Just "Text"
                            Like -> Just "Text"
                            _ -> e1rt,
                        ctxExprMaybeLevel = max 0 $ e1m - e2m,
                        ctxExprListValue = op `elem` [In, NotIn]
                    }) 
                   (hsValExpr e2)
            return $ "(" ++ r1 ++ ") " ++ hsBinOp op ++ " (" ++ r2 ++ ")"
        ExistsExpr sq -> subQuery "exists" sq
        ExternExpr ee ps -> do
            ps' <- mapM externExprParam ps
            return $ intercalate " " $ [ee] ++ map ((++ ")"). ("("++)) ps'
    where
        externExprParam (FieldRefParam fr) = hsFieldRef fr
        externExprParam (VerbatimParam v) = return v 
