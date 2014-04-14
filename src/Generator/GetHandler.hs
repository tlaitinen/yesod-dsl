{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Generator.GetHandler where

import System.IO (FilePath, writeFile)
import System.FilePath (joinPath)    
import System.Directory (createDirectoryIfMissing)
import Data.String.Utils (rstrip)    
import AST
import Text.Shakespeare.Text hiding (toText)
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Data.Char
import Generator.Common
import Generator.Esqueleto
import Generator.Models
import Generator.Require
import Control.Monad.State
getHandlerParam :: HandlerParam -> State Context String
getHandlerParam DefaultFilterSort = return $ T.unpack $(codegenFile "codegen/default-filter-sort-param.cg")
    ++ (T.unpack $(codegenFile "codegen/offset-limit-param.cg"))
getHandlerParam (IfFilter (pn,_,_,useFlag)) = return $ T.unpack $(codegenFile "codegen/get-filter-param.cg")
    where forceType = if useFlag == True then (""::String) else " :: Maybe Text"
getHandlerParam _ = return ""      

ctxFields :: State Context [(Entity, VariableName, Field)]
ctxFields = do
    m <- gets ctxModule
    names <- gets ctxNames
    return [ (e,vn,f) | e <- modEntities m,
                     (en,vn,_) <- names,
                     entityName e == en,
                     f <- entityFields e ]


defaultFilterField :: (Entity, VariableName, Field) -> State Context String
defaultFilterField (e,vn,f) = do
    baseMaybeLevel <- ctxMaybeLevel vn
    let maybeLevel = baseMaybeLevel + boolToInt (fieldOptional f)
        isMaybe = baseMaybeLevel > 0
    return $ T.unpack $(codegenFile "codegen/default-filter-field.cg")

defaultFilterFields :: State Context String
defaultFilterFields = do
    fs <- ctxFields
    fields <- liftM concat $ mapM defaultFilterField fs
    return $ T.unpack $(codegenFile "codegen/default-filter-fields.cg") 

defaultSortField :: (Entity, VariableName, Field, ParamName) -> State Context String    
defaultSortField (e,vn,f,pn) = do
    maybeLevel <- ctxMaybeLevel vn
    let isMaybe = maybeLevel > 0
    return $ T.unpack $(codegenFile "codegen/default-sort-field.cg")

defaultSortFields :: SelectQuery -> State Context String
defaultSortFields sq = do
    sortFields <- liftM concat $ mapM fromSelectField (sqFields sq)
    fields <- liftM concat $ mapM defaultSortField sortFields
    staticSortFields <- mapM hsOrderBy $ sqOrderBy sq
    return $ T.unpack $(codegenFile "codegen/default-sort-fields.cg")
    where 
          fromSelectField (SelectAllFields vn) = do
              m <- gets ctxModule
              en <- ctxLookupEntity vn >>= return . (fromMaybe "(Nothing)")
              return [ (e,vn, f, fieldName f)
                 | e <- modEntities m, 
                   entityName e == en,
                   f <- entityFields e]
          fromSelectField (SelectField vn fn an) = do
              m <- gets ctxModule
              en <- ctxLookupEntity vn >>= return . (fromMaybe "(Nothing)")
              return [ (e,vn, f, maybe (fieldName f) id an)
                 | e <- modEntities m, 
                   entityName e == en, 
                   f <- entityFields e,
                   fieldName f == fn ]
          fromSelectField (SelectIdField en an) = return [] -- TODO
          fromSelectField _ = return []         



isMaybeFieldRef :: FieldRef -> State Context Bool
isMaybeFieldRef (FieldRefNormal vn fn) = do
    mf <- ctxLookupField vn fn 
    return (fromMaybe False $ mf >>= return . fieldOptional)
isMaybeFieldRef _  = return False

implicitJoinExpr :: Join -> State Context String
implicitJoinExpr (Join _ en vn (Just expr)) = do
    e <- hsBoolExpr expr
    return $ "where_ (" ++ e ++ ")\n"
implicitJoinExpr _ = return ""


baseIfFilter :: VariableName -> IfFilterParams -> State Context String
baseIfFilter selectVar (pn,joins,bExpr,useFlag) = do
    ctx <- get
    put $ ctx { ctxNames = ctxNames ctx ++ [(joinEntity j, joinAlias j, isOuterJoin $ joinType j) | j <- joins] }
    joinExprs <- liftM concat $ mapM implicitJoinExpr joins
    expr <- hsBoolExpr bExpr
    put ctx
    return $ T.unpack $ if useFlag
        then $(codegenFile "codegen/base-if-filter.cg")
        else $(codegenFile "codegen/base-if-filter-nouse.cg")
    where 
          maybeFrom = if null joins 
                        then "do"
                        else T.unpack $(codegenFile "codegen/if-filter-from.cg")    
getSelectQuery :: State Context (Maybe SelectQuery)
getSelectQuery = do
    ps <- gets ctxHandlerParams
    return $ ((listToMaybe . (filter isSelect)) ps) >>= \(Select sq) -> return sq
    where
        isSelect (Select _) = True
        isSelect _ = False

getHandlerSelect :: State Context String
getHandlerSelect = do
    ctx <- get
    ps <- gets ctxHandlerParams
    msq <- getSelectQuery
    case msq of
        Just sq -> do
            let defaultFilterSort = DefaultFilterSort `elem` ps
                ifFilters = map (\(IfFilter f) -> f) $ filter isIfFilter ps
                isIfFilter (IfFilter _) = True
                isIfFilter _ = False
                (limit, offset) = sqLimitOffset sq
                (selectEntity, selectVar) = sqFrom sq 
                maybeDefaultLimitOffset = 
                     if defaultFilterSort 
                          then T.unpack $(codegenFile "codegen/default-offset-limit.cg")
                          else ""
            put $ ctx { ctxNames = sqAliases sq }
            maybeWhere <- case sqWhere sq of
                Just expr -> do
                    e <- hsBoolExpr expr
                    return $ "where_ (" ++ e ++ ")\n"
                Nothing -> return ""
            joinExprs <- liftM concat $ mapM mapJoinExpr $ reverse $ sqJoins sq
            ifFiltersStr <- liftM concat $ mapM (baseIfFilter selectVar) ifFilters
            filterFieldsStr <- defaultFilterFields
            returnFieldsStr <- selectReturnFields sq
            maybeDefaultSortFields <- if defaultFilterSort
                then defaultSortFields sq
                else return ""
            ret <- getHandlerReturn sq
            put ctx
            return $ (T.unpack $(codegenFile "codegen/base-select-query.cg"))
                ++ (if defaultFilterSort 
                    then filterFieldsStr 
                         ++ ifFiltersStr
                    else "")
               ++ (indent 8 returnFieldsStr)
               ++ (T.unpack $(codegenFile "codegen/select-count.cg"))
               ++ (T.unpack $(codegenFile "codegen/select-results.cg"))
               ++ ret 
        Nothing -> return ""

getHandlerReturn :: SelectQuery -> State Context String
getHandlerReturn sq = do
    ctx <- get
    put $ ctx { ctxNames = sqAliases sq }
    fieldNames' <- liftM concat $ mapM expand $ sqFields sq
    put $ ctx 
    let fieldNames = zip fieldNames' ([1..]::[Int])
        mappedResultFields = concatMap mapResultField $ fieldNames
        resultFields = map (\(_,i) -> "(Database.Esqueleto.Value f"++ show i ++ ")")  fieldNames
    return $ T.unpack $(codegenFile "codegen/get-handler-return.cg")
    where 
          expand (SelectAllFields vn) = do
            en <- ctxLookupEntity vn >>= return . (fromMaybe "(Nothing)")
            m <- gets ctxModule
            let e = fromJust $ lookupEntity m en 
            return $ map fieldName $ [ f | f <- entityFields e, 
                                           (not . fieldInternal) f ]
          expand (SelectField _ fn an') = return [ maybe fn id an' ]
          expand (SelectIdField _ an') = return [ maybe "id" id an' ]
          expand (SelectValExpr ve an) = return [ an ]
          expand (SelectParamField _ _ _) = return []
          mapResultField (fn,i) = T.unpack $(codegenFile "codegen/map-result-field.cg")

valExprRefs :: ValExpr -> [FieldRef]
valExprRefs (FieldExpr fr) = [fr]
valExprRefs (ConstExpr _) = []
valExprRefs (ConcatManyExpr ves) = concatMap valExprRefs ves
valExprRefs (ValBinOpExpr ve1 _ ve2) = concatMap valExprRefs [ve1,ve2]
valExprRefs RandomExpr = []
valExprRefs (FloorExpr ve) = valExprRefs ve
valExprRefs (CeilingExpr ve) = valExprRefs ve
valExprRefs (ExtractExpr _ ve) = valExprRefs ve
valExprRefs (SubQueryExpr sq) = sqFieldRefs sq
valExprRefs (ApplyExpr _ _) = [] 
exprFieldRefs :: BoolExpr -> [FieldRef]
exprFieldRefs (AndExpr e1 e2) = concatMap exprFieldRefs [e1,e2]
exprFieldRefs (OrExpr e1 e2) = concatMap exprFieldRefs [e1,e2]
exprFieldRefs (NotExpr e) = exprFieldRefs e
exprFieldRefs (BinOpExpr ve1 _ ve2) = valExprRefs ve1 ++ (valExprRefs ve2)
          
joinFieldRefs :: Join -> [FieldRef]
joinFieldRefs j = maybe [] exprFieldRefs (joinExpr j)

sqFieldRefs :: SelectQuery -> [FieldRef]
sqFieldRefs sq = concatMap joinFieldRefs (sqJoins sq) ++ case sqWhere sq of
    Just e -> exprFieldRefs e
    _ -> []

getHandlerParamFieldRefs :: HandlerParam-> [FieldRef]
getHandlerParamFieldRefs h = case h of
    (Select sq) -> sqFieldRefs sq
    (IfFilter (_,joins,e,_)) -> concatMap joinFieldRefs joins ++ exprFieldRefs e
    _ -> []

getHandlerMaybeAuth :: [HandlerParam] -> String
getHandlerMaybeAuth ps 
    | (not . null) (filter isAuthField fieldRefs) = T.unpack $(codegenFile "codegen/load-auth.cg")
    | otherwise = ""
        where fieldRefs = concatMap getHandlerParamFieldRefs ps
              isAuthField (FieldRefAuth _) = True
              isAuthField _ =False
    
getHandler :: State Context String
getHandler = do
    ps <- gets ctxHandlerParams
    liftM concat $ sequence [
            return $ getHandlerMaybeAuth ps,
            liftM concat $ mapM getHandlerParam ps,
            requireStmts,
            getHandlerSelect
        ]
    
