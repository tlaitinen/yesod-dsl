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
getHandlerParam :: Module -> Route -> Context -> HandlerParam -> String
getHandlerParam m r ps DefaultFilterSort = T.unpack $(codegenFile "codegen/default-filter-sort-param.cg")
    ++ (T.unpack $(codegenFile "codegen/offset-limit-param.cg"))
getHandlerParam m r ps (IfFilter (pn,_,_,useFlag)) = T.unpack $(codegenFile "codegen/get-filter-param.cg")
    where forceType = if useFlag == True then (""::String) else " :: Maybe Text"
getHandlerParam _ _ _ _ = ""      


ctxFields :: Module -> Context -> [(Entity, VariableName, Field)]
ctxFields m ctx = [ (e,vn,f) | e <- modEntities m,
                                  (en,vn,_) <- ctxNames ctx,
                                  entityName e == en,
                                  f <- entityFields e ]

defaultFilterField :: Context -> (Entity, VariableName, Field) -> String
defaultFilterField ctx (e,vn,f) = T.unpack $(codegenFile "codegen/default-filter-field.cg")

defaultFilterFields :: Module -> Context -> String
defaultFilterFields m ctx = T.unpack $(codegenFile "codegen/default-filter-fields.cg") 
    where fields = concatMap (defaultFilterField ctx) (ctxFields m ctx)

defaultSortField :: Context -> (Entity, VariableName, Field, ParamName) -> String    
defaultSortField ctx (e,vn,f,pn) = T.unpack $(codegenFile "codegen/default-sort-field.cg")
defaultSortFields :: Module -> Context -> SelectQuery -> String
defaultSortFields m ctx sq  = T.unpack $(codegenFile "codegen/default-sort-fields.cg")
    where fields = concatMap (defaultSortField ctx) sortFields
          sortFields = concatMap fromSelectField (sqFields sq)
          fromSelectField (SelectAllFields vn) = 
                [ (e,vn, f, fieldName f)
                 | e <- modEntities m, 
                   entityName e == (fromJust $ ctxLookupEntity ctx vn), 
                   f <- entityFields e]
          fromSelectField (SelectField vn fn an) = 
                [ (e,vn, f, maybe (fieldName f) id an)
                 | e <- modEntities m, 
                   entityName e == (fromJust $ ctxLookupEntity ctx vn), 
                   f <- entityFields e,
                   fieldName f == fn ]
          fromSelectField (SelectIdField en an) = [] -- TODO
          fromSelectField _ = []         



isMaybeFieldRef :: Module -> Context -> FieldRef -> Bool
isMaybeFieldRef m ctx (FieldRefNormal vn fn) = fieldOptional $ fromJust $ lookupField m (fromJust $ ctxLookupEntity ctx vn) fn
isMaybeFieldRef _ _  _ = False

makeJustField :: Bool -> String -> String    
makeJustField True f = "(just " ++ f ++ ")"
makeJustField False f = f

implicitJoinExpr :: Module -> Context -> Join -> String
implicitJoinExpr m ctx (Join _ en vn (Just expr)) = T.unpack $(codegenFile "codegen/where-expr.cg")
implicitJoinExpr m _ _ = ""





    
baseDefaultFilterSort :: Module -> Context -> String
baseDefaultFilterSort = defaultFilterFields

baseIfFilter :: Module -> Context -> VariableName -> IfFilterParams -> String
baseIfFilter m ctx' selectVar (pn,joins,expr,useFlag) = T.unpack $ if useFlag
    then $(codegenFile "codegen/base-if-filter.cg")
    else $(codegenFile "codegen/base-if-filter-nouse.cg")
    where ctx = ctx' { ctxNames = ctxNames ctx' 
              ++ [(joinEntity j, joinAlias j, isOuterJoin $ joinType j) | j <- joins] }
          maybeFrom = if null joins 
                        then "do"
                        else T.unpack $(codegenFile "codegen/if-filter-from.cg")    

   
    
getHandlerSelect :: Module -> SelectQuery -> Bool -> [IfFilterParams] -> String
getHandlerSelect m sq defaultFilterSort ifFilters = 
    (T.unpack $(codegenFile "codegen/base-select-query.cg"))
   ++ (if defaultFilterSort 
        then baseDefaultFilterSort m ctx  
             ++ (concatMap (baseIfFilter m ctx selectVar) ifFilters)
        else "")
   ++ (selectReturnFields m ctx sq 0)
   ++ (T.unpack $(codegenFile "codegen/select-count.cg"))
   ++ (T.unpack $(codegenFile "codegen/select-results.cg"))
    where 
          orderByFields = sq
          (limit, offset) = sqLimitOffset sq
          ctx = Context {
              ctxNames = sqAliases sq,
              ctxModule = m,
              ctxHandlerParams = []
          }
          (selectEntity, selectVar) = sqFrom sq 
          maybeWhere = case sqWhere sq of
             Just expr -> T.unpack $(codegenFile "codegen/where-expr.cg")
             Nothing -> ""
          maybeDefaultSortFields = if defaultFilterSort 
            then    defaultSortFields m ctx sq
            else ""
          maybeDefaultLimitOffset = 
               if defaultFilterSort 
                    then T.unpack $(codegenFile "codegen/default-offset-limit.cg")
                    else ""




getHandlerReturn :: Module -> SelectQuery -> String
getHandlerReturn m sq = T.unpack $(codegenFile "codegen/get-handler-return.cg")
    where 
          ctx = Context { ctxNames = sqAliases sq, ctxModule = m, ctxHandlerParams = [] }
          fieldNames = zip (concatMap expand (sqFields sq)) ([1..]:: [Int])
          expand (SelectAllFields vn) = map fieldName $ publicFields
                where en = fromJust $ ctxLookupEntity ctx vn
                      e = fromJust $ lookupEntity m en    
                      publicFields = [ f | f <- entityFields e, (not . fieldInternal) f ]
          expand (SelectField _ fn an') = [ maybe fn id an' ]
          expand (SelectIdField _ an') = [ maybe "id" id an' ]
          expand (SelectValExpr ve an) = [ an ]
          resultFields = map (\(_,i) -> "(Database.Esqueleto.Value f"++ show i ++ ")")  fieldNames
          mappedResultFields = concatMap mapResultField fieldNames
          mapResultField (fn,i) = T.unpack $(codegenFile "codegen/map-result-field.cg")

valExprRefs :: ValExpr -> [FieldRef]
valExprRefs (FieldExpr fr) = [fr]
valExprRefs (ConstExpr _) = []
valExprRefs (ConcatExpr ve1 ve2) = concatMap valExprRefs [ve1,ve2]

exprFieldRefs :: Expr -> [FieldRef]
exprFieldRefs (AndExpr e1 e2) = concatMap exprFieldRefs [e1,e2]
exprFieldRefs (OrExpr e1 e2) = concatMap exprFieldRefs [e1,e2]
exprFieldRefs (NotExpr e) = exprFieldRefs e
exprFieldRefs (BinOpExpr ve1 _ ve2) = valExprRefs ve1 ++ (valExprRefs ve2)
exprFieldRefs (ListOpExpr fr1 _ fr2) = [fr1,fr2]
          
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
    
getHandler :: Module -> Route -> [HandlerParam] -> String
getHandler m r ps = 
    getHandlerMaybeAuth ps ++ 
    (concatMap (getHandlerParam m r ctx) ps)
    ++ (requireStmts m ps)
    ++ (getHandlerSelect m sq defaultFilterSort ifFilters)
    ++ (getHandlerReturn m sq)
    where 
        (Select sq) = (fromJust . listToMaybe . (filter isSelect)) ps
        ctx = Context { ctxNames = sqAliases sq, ctxModule = m, ctxHandlerParams = [] }
        isSelect (Select _) = True
        isSelect _ = False
    
        defaultFilterSort = DefaultFilterSort `elem` ps

        ifFilters = map (\(IfFilter f) -> f) $ filter isIfFilter ps
        isIfFilter (IfFilter _) = True
        isIfFilter _ = False

