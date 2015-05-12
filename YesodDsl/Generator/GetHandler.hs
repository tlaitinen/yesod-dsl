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
import Control.Monad.Reader
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map
getStmt :: Stmt -> Reader Context String
getStmt DefaultFilterSort = return $ T.unpack $(codegenFile "codegen/default-filter-sort-param.cg")
    ++ (T.unpack $(codegenFile "codegen/offset-limit-param.cg"))
getStmt (IfFilter (pn,_,_,useFlag)) = return $ T.unpack $(codegenFile "codegen/get-filter-param.cg")
    where forceType = if useFlag == True then (""::String) else " :: Maybe Text"
getStmt _ = return ""      

ctxFields :: Reader Context [(Entity, VariableName, Field, VariableName, MaybeFlag)]
ctxFields = do
    names <- asks ctxNames
    let fields = [ (e,vn,f,mf) | (vn,(e,mf)) <- Map.toList names, f <- entityFields e ]
        usage = Map.fromListWith (+) [ (fieldName f,1) | (_,_,f,_) <- fields ]
    return $ [
            (e,vn,f,if Map.findWithDefault 1 (fieldName f) usage == 1 then fieldName f else vn ++ "." ++ fieldName f,mf)
            | (e,vn,f,mf) <- fields 
        ]    


defaultFilterField :: (Entity, VariableName, Field,VariableName,MaybeFlag) -> Reader Context String
defaultFilterField (e,vn,f,alias,isMaybe) = do
    let maybeLevel = boolToInt isMaybe + boolToInt (fieldOptional f)
    return $ T.unpack $(codegenFile "codegen/default-filter-field.cg")

defaultFilterFields :: SelectQuery -> Reader Context String
defaultFilterFields sq = do
    fs <- ctxFields
    fields' <- liftM concat $ mapM defaultFilterField fs

    let (er,vn) = sqFrom sq 
        en = entityRefName er
    let fields = (T.unpack $(codegenFile "codegen/default-filter-id-field.cg")
                 ++ fields')
    return $ T.unpack $(codegenFile "codegen/default-filter-fields.cg")
defaultSortField :: (Entity, VariableName, Field, ParamName, MaybeFlag) -> Reader Context String    
defaultSortField (e,vn,f,pn,isMaybe) = do
    return $ T.unpack $(codegenFile "codegen/default-sort-field.cg")

defaultSortFields :: SelectQuery -> Reader Context String
defaultSortFields sq = do
    sortFields <- liftM concat $ mapM fromSelectField (sqFields sq)
    fields <- liftM concat $ mapM defaultSortField sortFields
    staticSortFields <- mapM hsOrderBy $ sqOrderBy sq
    return $ T.unpack $(codegenFile "codegen/default-sort-fields.cg")
    where 
          fromSelectField (SelectField (Var vn (Right e) mf) fn an) = do
              m <- asks ctxModule
              let en = entityName e
              return [ (e,vn, f, maybe (fieldName f) id an,mf)
                 | f <- entityFields e,
                   fieldName f == fn ]
          fromSelectField (SelectIdField en an) = return [] -- TODO
          fromSelectField _ = return []         



isMaybeFieldRef :: FieldRef -> Reader Context Bool
isMaybeFieldRef (SqlField (Var vn (Right e) _) fn) = do
    return (fromMaybe False $ lookupField e fn >>= return . fieldOptional)
isMaybeFieldRef _  = return False

implicitJoinExpr :: Join -> Reader Context String
implicitJoinExpr (Join _ en vn (Just expr)) = do
    e <- hsBoolExpr expr
    return $ "where_ (" ++ e ++ ")\n"
implicitJoinExpr _ = return ""


baseIfFilter :: VariableName -> IfFilterParams -> Reader Context String
baseIfFilter selectVar (pn,joins,bExpr,useFlag) = withScope 
    (Map.fromList $ catMaybes [ either (\_ -> Nothing) (\e -> Just (joinAlias j, (e, isOuterJoin $ joinType j))) $ joinEntity j | j <- joins]) $ do
        joinExprs <- liftM concat $ mapM implicitJoinExpr joins
        expr <- hsBoolExpr bExpr
        return $ T.unpack $ if useFlag
            then $(codegenFile "codegen/base-if-filter.cg")
            else $(codegenFile "codegen/base-if-filter-nouse.cg")
    where 
          maybeFrom = if null joins 
                        then "do"
                        else T.unpack $(codegenFile "codegen/if-filter-from.cg")    
getSelectQuery :: Reader Context (Maybe SelectQuery)
getSelectQuery = do
    ps <- asks ctxStmts
    return $ ((listToMaybe . (filter isSelect)) ps) >>= \(Select sq) -> return sq
    where
        isSelect (Select _) = True
        isSelect _ = False

getHandlerSelect :: Reader Context String
getHandlerSelect = do
    ps <- asks ctxStmts
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

getHandlerReturn :: SelectQuery -> Reader Context String
getHandlerReturn sq = do
    fieldNames' <- liftM concat $ mapM expand $ sqFields sq
    let fieldNames = zip fieldNames' ([1..]::[Int])
        mappedResultFields = concatMap mapResultField $ fieldNames
        resultFields = map (\(_,i) -> "(Database.Esqueleto.Value f"++ show i ++ ")")  fieldNames
    return $ T.unpack $(codegenFile "codegen/get-handler-return.cg")
    where 
          expand (SelectField _ fn an') = return [ maybe fn id an' ]
          expand (SelectIdField _ an') = return [ maybe "id" id an' ]
          expand (SelectValExpr ve an) = return [ an ]
          mapResultField (fn,i) = T.unpack $(codegenFile "codegen/map-result-field.cg")



getHandlerMaybeAuth :: [Stmt] -> String
getHandlerMaybeAuth ps 
    | (not . null) (filter isAuthField fieldRefs) = T.unpack $(codegenFile "codegen/load-auth.cg")
    | otherwise = ""
        where fieldRefs = concatMap universeBi ps
              isAuthField (AuthField _) = True
              isAuthField _ =False
   
callStmts :: Reader Context String
callStmts = do
    ps <- asks ctxStmts
    liftM concat $ mapM f $ zip ([1..] :: [Int]) ps
    where 
        f (callId,(Call fn frs)) = do
            ifrs <- mapM inputFieldRef frs
            return $ T.unpack $(codegenFile "codegen/get-call.cg")
        f _ = return ""     
getHandlerReadRequestFields :: Reader Context String
getHandlerReadRequestFields = do
    m <- asks ctxModule
    ps <- asks ctxStmts
    let attrs = jsonAttrs m ps
        defaults = getParamDefaults ps
    if null attrs
        then return ""
        else return $
            concatMap (\attr -> prepareRequestInputField attr (Map.lookup attr defaults)) attrs
    where
        jsonAttrs m ps = nub $ concatMap getJsonAttrs ps
        prepareRequestInputField fn Nothing = T.unpack $(codegenFile "codegen/prepare-request-input-field.cg")
        prepareRequestInputField fn (Just d) = T.unpack $(codegenFile "codegen/prepare-request-input-field-default.cg")

getHandler :: Reader Context String
getHandler = do
    ps <- asks ctxStmts
    liftM concat $ sequence [
            return $ getHandlerMaybeAuth ps,
            liftM concat $ mapM getStmt ps,
            getHandlerReadRequestFields,
            requireStmts,
            callStmts,
            getHandlerSelect
        ]
    
