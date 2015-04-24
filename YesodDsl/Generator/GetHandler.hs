{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator.GetHandler where
import Data.Either
import System.IO (FilePath, writeFile)
import System.FilePath (joinPath)    
import System.Directory (createDirectoryIfMissing)
import Data.String.Utils (rstrip)    
import YesodDsl.AST
import Text.Shakespeare.Text hiding (toText)
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Data.Char
import YesodDsl.Generator.Common
import YesodDsl.Generator.Esqueleto
import YesodDsl.Generator.Models
import YesodDsl.Generator.Require
import YesodDsl.Generator.Input
import Control.Monad.State
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map
getHandlerParam :: HandlerParam -> State Context String
getHandlerParam DefaultFilterSort = return $ T.unpack $(codegenFile "codegen/default-filter-sort-param.cg")
    ++ (T.unpack $(codegenFile "codegen/offset-limit-param.cg"))
getHandlerParam (IfFilter (pn,_,_,useFlag)) = return $ T.unpack $(codegenFile "codegen/get-filter-param.cg")
    where forceType = if useFlag == True then (""::String) else " :: Maybe Text"
getHandlerParam _ = return ""      

ctxFields :: State Context [(Entity, VariableName, Field, VariableName)]
ctxFields = do
    m <- gets ctxModule
    names <- gets ctxNames
    let fields = [ (e,vn,f) | e <- modEntities m,
                     (en,vn,_) <- names,
                     entityName e == en,
                     f <- entityFields e ]
        usage = Map.fromListWith (+) [ (fieldName f,1) | (_,_,f) <- fields ]
    return $ [
            (e,vn,f,if Map.findWithDefault 1 (fieldName f) usage == 1 then fieldName f else vn ++ "." ++ fieldName f)
            | (e,vn,f) <- fields 
        ]    


defaultFilterField :: (Entity, VariableName, Field,VariableName) -> State Context String
defaultFilterField (e,vn,f,alias) = do
    baseMaybeLevel <- ctxMaybeLevel vn
    let maybeLevel = baseMaybeLevel + boolToInt (fieldOptional f)
        isMaybe = baseMaybeLevel > 0
    return $ T.unpack $(codegenFile "codegen/default-filter-field.cg")

defaultFilterFields :: SelectQuery -> State Context String
defaultFilterFields sq = do
    fs <- ctxFields
    fields' <- liftM concat $ mapM defaultFilterField fs

    let (er,vn) = sqFrom sq 
        en = entityRefName er
    let fields = (T.unpack $(codegenFile "codegen/default-filter-id-field.cg")
                 ++ fields')
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
baseIfFilter selectVar (pn,joins,bExpr,useFlag) = withScope 
    (catMaybes [ either (\_ -> Nothing) (\e -> Just (e, joinAlias j, isOuterJoin $ joinType j)) $ joinEntity j | j <- joins]) $ do
        joinExprs <- liftM concat $ mapM implicitJoinExpr joins
        expr <- hsBoolExpr bExpr
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
        Just sq -> withScope (sqAliases sq) $ do
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
                    e <- hsBoolExpr expr
                    return $ "where_ (" ++ e ++ ")\n"
                Nothing -> return ""
            joinExprs <- liftM concat $ mapM mapJoinExpr $ reverse $ sqJoins sq
            ifFiltersStr <- liftM concat $ mapM (baseIfFilter selectVar) ifFilters
            filterFieldsStr <- defaultFilterFields sq
            returnFieldsStr <- selectReturnFields sq
            maybeDefaultSortFields <- if defaultFilterSort
                then defaultSortFields sq
                else return ""
            ret <- getHandlerReturn sq
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
    put $ ctx { ctxNames = map (\(e,vn,mf) -> (entityName e, vn, mf)) $ sqAliases sq }
    fieldNames' <- liftM concat $ mapM expand $ sqFields sq
    put $ ctx 
    let fieldNames = zip fieldNames' ([1..]::[Int])
        mappedResultFields = concatMap mapResultField $ fieldNames
        resultFields = map (\(_,i) -> "(Database.Esqueleto.Value f"++ show i ++ ")")  fieldNames
    return $ T.unpack $(codegenFile "codegen/get-handler-return.cg")
    where 
          expand (SelectField _ fn an') = return [ maybe fn id an' ]
          expand (SelectIdField _ an') = return [ maybe "id" id an' ]
          expand (SelectValExpr ve an) = return [ an ]
          expand (SelectParamField _ _ _) = return []
          mapResultField (fn,i) = T.unpack $(codegenFile "codegen/map-result-field.cg")


getHandlerParamFieldRefs :: HandlerParam-> [FieldRef]
getHandlerParamFieldRefs  = universeBi 

getHandlerMaybeAuth :: [HandlerParam] -> String
getHandlerMaybeAuth ps 
    | (not . null) (filter isAuthField fieldRefs) = T.unpack $(codegenFile "codegen/load-auth.cg")
    | otherwise = ""
        where fieldRefs = concatMap getHandlerParamFieldRefs ps
              isAuthField (FieldRefAuth _) = True
              isAuthField _ =False
   
callStmts :: State Context String
callStmts = do
    ps <- gets ctxHandlerParams
    liftM concat $ mapM f $ zip ([1..] :: [Int]) ps
    where 
        f (callId,(Call fn frs)) = do
            ifrs <- mapM inputFieldRef frs
            return $ T.unpack $(codegenFile "codegen/get-call.cg")
        f _ = return ""     
getHandlerReadRequestFields :: State Context String
getHandlerReadRequestFields = do
    m <- gets ctxModule
    ps <- gets ctxHandlerParams
    let attrs = jsonAttrs m ps
    if null attrs
        then return ""
        else return $
            concatMap prepareRequestInputField attrs
    where
        jsonAttrs m ps = nub $ concatMap getJsonAttrs ps
        prepareRequestInputField fn = T.unpack $(codegenFile "codegen/prepare-request-input-field.cg")


getHandler :: State Context String
getHandler = do
    ps <- gets ctxHandlerParams
    liftM concat $ sequence [
            return $ getHandlerMaybeAuth ps,
            liftM concat $ mapM getHandlerParam ps,
            getHandlerReadRequestFields,
            requireStmts,
            callStmts,
            getHandlerSelect
        ]
    
