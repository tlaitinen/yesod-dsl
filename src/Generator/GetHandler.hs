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
getHandlerParam :: Module -> Route -> Context -> HandlerParam -> String
getHandlerParam m r ps DefaultFilterSort = T.unpack $(codegenFile "codegen/default-filter-sort-param.cg")
    ++ (T.unpack $(codegenFile "codegen/offset-limit-param.cg"))
getHandlerParam m r ps (IfFilter (pn,_,_)) = T.unpack $(codegenFile "codegen/get-filter-param.cg")
getHandlerParam _ _ _ _ = ""      


ctxFields :: Module -> Context -> [(Entity, VariableName, Field)]
ctxFields m ctx = [ (e,vn,f) | e <- modEntities m,
                                  (en,vn,_) <- ctx,
                                  entityName e == en,
                                  f <- entityFields e ]

defaultFilterField :: (Entity, VariableName, Field) -> String
defaultFilterField (e,vn,f) = T.unpack $(codegenFile "codegen/default-filter-field.cg")

defaultFilterFields :: Module -> Context -> String
defaultFilterFields m ctx = T.unpack $(codegenFile "codegen/default-filter-fields.cg") 
    where fields = concatMap defaultFilterField (ctxFields m ctx)

defaultSortField :: (Entity, VariableName, Field, ParamName) -> String    
defaultSortField (e,vn,f,pn) = T.unpack $(codegenFile "codegen/default-sort-field.cg")
defaultSortFields :: Module -> Context -> SelectQuery -> String
defaultSortFields m ctx sq  = T.unpack $(codegenFile "codegen/default-sort-fields.cg")
    where fields = concatMap defaultSortField sortFields
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


joinDef :: Join-> String
joinDef (Join jt _ vn _) = rstrip $ T.unpack $(codegenFile "codegen/join-def.cg")


isMaybeFieldRef :: Module -> Context -> FieldRef -> Bool
isMaybeFieldRef m ctx (FieldRefNormal vn fn) = fieldOptional $ fromJust $ lookupField m (fromJust $ ctxLookupEntity ctx vn) fn
isMaybeFieldRef _ _  _ = False

makeJustField :: Bool -> String -> String    
makeJustField True f = "(just " ++ f ++ ")"
makeJustField False f = f

mapJoinExpr :: Module -> Context -> Join -> String
mapJoinExpr m ctx (Join _ en vn (Just expr)) = T.unpack $(codegenFile "codegen/join-expr.cg")
mapJoinExpr m _ _ = ""

implicitJoinExpr :: Module -> Context -> Join -> String
implicitJoinExpr m ctx (Join _ en vn (Just expr)) = T.unpack $(codegenFile "codegen/where-expr.cg")
implicitJoinExpr m _ _ = ""





    
baseDefaultFilterSort :: Module -> Context -> String
baseDefaultFilterSort = defaultFilterFields

baseIfFilter :: Module -> Context -> VariableName -> IfFilterParams -> String
baseIfFilter m ctx' selectVar (pn,joins,expr) = T.unpack $(codegenFile "codegen/base-if-filter.cg")
    where ctx = ctx' 
              ++ [(joinEntity j, joinAlias j, isOuterJoin $ joinType j) | j <- joins]
          maybeFrom = if null joins 
                        then "do"
                        else T.unpack $(codegenFile "codegen/if-filter-from.cg")    

   
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
        

    
getHandlerSelect :: Module -> SelectQuery -> Bool -> [IfFilterParams] -> String
getHandlerSelect m sq defaultFilterSort ifFilters = 
    (T.unpack $(codegenFile "codegen/base-select-query.cg"))
   ++ (if defaultFilterSort 
        then baseDefaultFilterSort m ctx  
             ++ (concatMap (baseIfFilter m ctx selectVar) ifFilters)
        else "")
   ++ (selectReturnFields m ctx sq)
   ++ (T.unpack $(codegenFile "codegen/select-count.cg"))
   ++ (T.unpack $(codegenFile "codegen/select-results.cg"))
    where 
          orderByFields = sq
          (limit, offset) = sqLimitOffset sq
          ctx = sqAliases sq
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
          ctx = sqAliases sq
          fieldNames = zip (concatMap expand (sqFields sq)) ([1..]:: [Int])
          expand (SelectAllFields vn) = map fieldName $ entityFields e
                where en = fromJust $ ctxLookupEntity ctx vn
                      e = fromJust $ lookupEntity m en    
          expand (SelectField _ fn an') = [ maybe fn id an' ]
          expand (SelectIdField _ an') = [ maybe "id" id an' ]
          resultFields = map (\(_,i) -> "(Database.Esqueleto.Value f"++ show i ++ ")")  fieldNames
          mappedResultFields = concatMap mapResultField fieldNames
          mapResultField (fn,i) = T.unpack $(codegenFile "codegen/map-result-field.cg")
            
getHandler :: Module -> Route -> [HandlerParam] -> String
getHandler m r ps = 
    (concatMap (getHandlerParam m r ctx) ps)
    ++ (getHandlerSelect m sq defaultFilterSort ifFilters)
    ++ (getHandlerReturn m sq)
    where 
        (Select sq) = (fromJust . listToMaybe . (filter isSelect)) ps
        ctx = sqAliases sq
        isSelect (Select _) = True
        isSelect _ = False
    
        defaultFilterSort = DefaultFilterSort `elem` ps

        ifFilters = map (\(IfFilter f) -> f) $ filter isIfFilter ps
        isIfFilter (IfFilter _) = True
        isIfFilter _ = False

