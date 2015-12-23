{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator.GetHandler where
import Data.String.Utils (rstrip)    
import YesodDsl.AST
import Text.Shakespeare.Text hiding (toText)
import qualified Data.Text as T
import Data.List
import Data.Maybe
import YesodDsl.Generator.Common
import YesodDsl.Generator.Esqueleto
import YesodDsl.Generator.Models
import YesodDsl.Generator.Require
import YesodDsl.Generator.Input
import Control.Monad.Reader
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map
getStmt :: Stmt -> String
getStmt DefaultFilterSort = T.unpack $(codegenFile "codegen/default-filter-sort-param.cg")
    ++ (T.unpack $(codegenFile "codegen/offset-limit-param.cg"))
getStmt (IfFilter (pn,_,_,_,useFlag)) = T.unpack $(codegenFile "codegen/get-filter-param.cg")
    where forceType = if useFlag == True then (""::String) else " :: Maybe Text"
getStmt _ = ""      

ctxFields :: SelectQuery -> Reader Context [(Entity, VariableName, Field, VariableName, MaybeFlag)]
ctxFields sq = do
    names <- asks ctxNames
    let fields = [ (e,vn,f,mf) | (vn,(e,mf)) <- Map.toList names, f <- entityFields e, fieldInternal f == False ]
        usage = Map.fromListWith (+) [ (fieldJsonName f,1::Int) | (_,_,f,_) <- fields ]
    return $ [
            (e,vn,f,if Map.findWithDefault 1 (fieldJsonName f) usage == 1 || vn == fromVn then fieldJsonName f else vn ++ "." ++ fieldJsonName f,mf)
            | (e,vn,f,mf) <- fields 
        ]    
        where
            (_,fromVn) = sqFrom sq


defaultFilterField :: (Entity, VariableName, Field,VariableName,MaybeFlag) -> Reader Context String
defaultFilterField (e,vn,f,alias,isMaybe) = do
    let maybeLevel = boolToInt isMaybe + boolToInt (fieldOptional f)
    return $ T.unpack $ if maybeLevel > 0
        then $(codegenFile "codegen/default-filter-field-maybe.cg")
        else $(codegenFile "codegen/default-filter-field.cg")

defaultFilterFields :: SelectQuery -> Reader Context String
defaultFilterFields sq = do
    fs <- ctxFields sq
    fields' <- liftM concat $ mapM defaultFilterField fs

    let (er,vn) = sqFrom sq 
        en = entityRefName er
    let fields = (T.unpack $(codegenFile "codegen/default-filter-id-field.cg")
                 ++ fields')
    return $ T.unpack $(codegenFile "codegen/default-filter-fields.cg")
defaultSortField :: (Entity, VariableName, Field, ParamName, MaybeFlag) -> Reader Context String    
defaultSortField (e,vn,f,pn,isMaybe) = do
    return $ T.unpack $(codegenFile "codegen/default-sort-field.cg")

defaultSortFields :: [IfFilterParams] -> SelectQuery -> Reader Context String
defaultSortFields ifFilters sq = do
    sortFields <- liftM concat $ mapM fromSelectField (sqFields sq)
    fields <- liftM concat $ mapM defaultSortField sortFields
    staticSortFields <- mapM hsOrderBy $ sqOrderBy sq
    return $ T.unpack $(codegenFile "codegen/default-sort-fields.cg")
    where 
        ifFilterOrderBys = [ "filterParam_" ++ pn | (pn, _, _, obs, _) <- ifFilters, null obs == False ] 
        checkIfFilterOrderBy 
            | null ifFilterOrderBys = ""
            | otherwise = rstrip (T.unpack $(codegenFile "codegen/default-sort-fields-check-if-filter-order-by.cg")) ++ " "
          
        fromSelectField (SelectField (Var vn (Right e) mf) fn an) = do
              return [ (e,vn, f, maybe (fieldName f) id an,mf)
                 | f <- entityFields e,
                   fieldName f == fn ]
        fromSelectField (SelectIdField _ _) = return [] -- TODO
        fromSelectField _ = return []         




implicitJoinExpr :: Join -> Reader Context String
implicitJoinExpr (Join _ _ _ (Just expr)) = do
    e <- hsExpr 0 expr
    return $ "where_ (" ++ e ++ ")\n"
implicitJoinExpr _ = return ""


baseIfFilter :: IfFilterParams -> Reader Context String
baseIfFilter (pn,joins,bExpr,obs,useFlag) = withScope 
    (Map.fromList $ catMaybes [ either (\_ -> Nothing) (\e -> Just (joinAlias j, (e, isOuterJoin $ joinType j))) $ joinEntity j | j <- joins]) $ do
        joinExprs <- liftM concat $ mapM implicitJoinExpr joins
        expr <- hsExpr 0 bExpr
        sortFields <- mapM hsOrderBy obs
        
        return $ T.unpack $ if useFlag
            then $(codegenFile "codegen/base-if-filter.cg")
            else $(codegenFile "codegen/base-if-filter-nouse.cg")
    where 
        maybeOrderBy [] = ""
        maybeOrderBy sortFields = T.unpack $(codegenFile "codegen/if-filter-order-by.cg")
        maybeFrom = if null joins 
                      then "do"
                      else T.unpack $(codegenFile "codegen/if-filter-from.cg")    
getHandlerSelect :: [Stmt] -> String
getHandlerSelect ps = 
    case listToMaybe [ sq | Select sq <- universeBi ps ] of
        Just sq -> runReader (do
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
            maybeWhere <- case sqWhere sq of
                Just expr -> do
                    e <- hsExpr 0 expr
                    return $ "where_ (" ++ e ++ ")\n"
                Nothing -> return ""
            joinExprs <- liftM concat $ mapM mapJoinExpr $ reverse $ sqJoins sq
            ifFiltersStr <- liftM concat $ mapM baseIfFilter  ifFilters
            filterFieldsStr <- defaultFilterFields sq
            returnFieldsStr <- selectReturnFields sq
            maybeDefaultSortFields <- if defaultFilterSort
                then defaultSortFields ifFilters sq
                else do
                    sortFields <- mapM hsOrderBy $ sqOrderBy sq
                    return $ T.unpack $(codegenFile "codegen/static-order-by.cg")
            return $ concat [
                (T.unpack $(codegenFile "codegen/base-select-query.cg")),
                (if defaultFilterSort 
                    then filterFieldsStr 
                         ++ ifFiltersStr
                    else ""),
                (indent 8 returnFieldsStr),
                (T.unpack $(codegenFile "codegen/select-count.cg")),
                (T.unpack $(codegenFile "codegen/select-results.cg")),
                getHandlerReturn sq [ fn | MapJson fn <- universeBi ps ]
                ]) (emptyContext { ctxNames = sqAliases sq })
        Nothing -> ""

getHandlerReturn :: SelectQuery -> [FunctionName] -> String
getHandlerReturn sq jsonMappers = T.unpack $(codegenFile "codegen/get-handler-return.cg")
    where 
        prependBind = prepend " >>= "
        fieldNames' = concat $ map expand $ sqFields sq
        fieldNames = zip fieldNames' ([1..]::[Int])
        mappedResultFields = concatMap mapResultField $ fieldNames
        resultFields = map (\(_,i) -> "(Database.Esqueleto.Value f"++ show i ++ ")")  fieldNames
        expand (SelectField _ fn an') = [ maybe fn id an' ]
        expand (SelectIdField _ an') = [ maybe "id" id an' ]
        expand (SelectExpr _ an) = [ an ]
        expand _ = []
        mapResultField (fn,i) = T.unpack $(codegenFile "codegen/map-result-field.cg")



getHandlerMaybeAuth :: [Stmt] -> String
getHandlerMaybeAuth ps 
    | (not . null) (filter isAuthField fieldRefs) = T.unpack $(codegenFile "codegen/load-auth.cg")
    | otherwise = ""
        where fieldRefs = concatMap universeBi ps
              isAuthField (AuthField _) = True
              isAuthField _ =False
   
callStmts :: [Stmt] -> String
callStmts ps = concatMap f $ zip ([1..] :: [Int]) ps
    where 
        f (_,(Call fn frs)) = 
            let ifrs = map inputFieldRef frs
            in T.unpack $(codegenFile "codegen/get-call.cg")
        f _ = ""
getHandlerReadRequestFields :: [Stmt] -> String
getHandlerReadRequestFields ps = 
    let attrs = nub $ concatMap getJsonAttrs ps
        defaults = getParamDefaults ps
        prepareRequestInputField fn Nothing = T.unpack $(codegenFile "codegen/prepare-request-input-field.cg")
        prepareRequestInputField fn (Just d) = T.unpack $(codegenFile "codegen/prepare-request-input-field-default.cg")
    in if null attrs
        then ""
        else concatMap (\attr -> prepareRequestInputField attr (Map.lookup attr defaults)) attrs

getHandler :: [Stmt] -> String
getHandler ps = concat [
            getHandlerMaybeAuth ps,
            concatMap getStmt ps,
            getHandlerReadRequestFields ps,
            requireStmts ps,
            callStmts ps,
            getHandlerSelect ps
        ]
    
