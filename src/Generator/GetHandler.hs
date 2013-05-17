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
getHandlerParam :: Module -> Resource -> Context -> HandlerParam -> String
getHandlerParam m r ps DefaultFilterSort = T.unpack $(codegenFile "codegen/default-filter-sort-param.cg")
    ++ (T.unpack $(codegenFile "codegen/offset-limit-param.cg"))
getHandlerParam m r ps (TextSearchFilter (pn, fs)) = T.unpack $(codegenFile "codegen/text-search-filter-param.cg")
getHandlerParam _ _ _ _ = ""      


ctxFields :: Module -> Context -> [(Entity, VariableName, Field)]
ctxFields m ctx = [ (e,vn,f) | e <- modEntities m,
                                  (en,vn) <- ctx,
                                  entityName e == en,
                                  f <- entityFields e ]

defaultFilterField :: (Entity, VariableName, Field) -> String
defaultFilterField (e,vn,f) = T.unpack $(codegenFile "codegen/default-filter-field.cg")

defaultFilterFields :: Module -> Context -> String
defaultFilterFields m ctx = T.unpack $(codegenFile "codegen/default-filter-fields.cg") 
    where fields = concatMap defaultFilterField (ctxFields m ctx)

defaultSortField :: (Entity, VariableName, Field) -> String    
defaultSortField (e,vn,f) = T.unpack $(codegenFile "codegen/default-sort-field.cg")
defaultSortFields :: Module -> Context -> String
defaultSortFields m ps = T.unpack $(codegenFile "codegen/default-sort-fields.cg")
    where fields = concatMap defaultSortField (ctxFields m ps)


joinDef :: Join-> String
joinDef (Join jt _ vn _) = rstrip $ T.unpack $(codegenFile "codegen/join-def.cg")


isMaybeFieldRef :: Module -> Context -> FieldRef -> Bool
isMaybeFieldRef m ctx (FieldRefNormal vn fn) = fieldOptional $ fromJust $ lookupField m (fromJust $ ctxLookupEntity ctx vn) fn
isMaybeFieldRef _ _  _ = False

makeJustField :: Bool -> String -> String    
makeJustField True f = "(just " ++ f ++ ")"
makeJustField False f = f

mapJoinExpr :: Module -> Context -> Join -> String
mapJoinExpr m ctx (Join _ en vn (Just (f1, op, f2))) = T.unpack $(codegenFile "codegen/join-expr.cg")
    where f1just = f1maybe == False && f2maybe == True
          f2just = f2maybe == False && f1maybe == True
          f1maybe = isMaybeFieldRef m ctx f1
          f2maybe = isMaybeFieldRef m ctx f2
mapJoinExpr m _ _ = ""

indent :: Int -> String -> String
indent x = unlines . (map ((replicate x ' ')++)) . lines



textSearchFilterField :: Context -> ParamName -> FieldRef -> String
textSearchFilterField ctx pn f = rstrip $ T.unpack $(codegenFile "codegen/text-search-filter-field.cg")

    
baseSelectQuery :: Module -> Context -> SelectQuery -> String
baseSelectQuery m ctx sq = T.unpack $(codegenFile "codegen/base-select-query.cg")
    where (selectEntity, selectVar) = sqFrom sq 
          maybeWhere = case sqWhere sq of
             Just expr -> T.unpack $(codegenFile "codegen/where-expr.cg")
             Nothing -> ""

baseDefaultFilterSort :: Module -> Context -> String
baseDefaultFilterSort = defaultFilterFields

baseIfFilter :: Module -> Context -> VariableName -> IfFilterParams -> String
baseIfFilter m ctx' selectVar (pn,joins,expr) = T.unpack $(codegenFile "codegen/base-if-filter.cg")
    where ctx = ctx' 
              ++ [(joinEntity j, joinAlias j) | j <- joins]

textSearchFilter :: Module -> Context -> TextSearchParams -> String
textSearchFilter m ctx (pn, fieldRefs) = T.unpack $(codegenFile "codegen/text-search-filter.cg")
    where fields = map (textSearchFilterField ctx pn)
                       fieldRefs
selectOrderingLimitOffset :: Module -> Bool -> SelectQuery -> String
selectOrderingLimitOffset m defaultFilterSort sq = 
    (T.unpack $(codegenFile "codegen/offset-limit.cg"))
    ++ (if defaultFilterSort
            then T.unpack $(codegenFile "codegen/default-offset-limit.cg")
            else "")
    where (limit,offset) = sqLimitOffset sq
    
selectFieldRefs :: Module -> Context -> SelectField -> [FieldRef]
selectFieldRefs m ctx (SelectAllFields vn) =  [ FieldRefNormal vn (fieldName f) | 
                                                f <- entityFields e ]
    where  
           en = fromJust $ ctxLookupEntity ctx vn
           e = fromJust $ lookupEntity m en    
selectFieldRefs m ctx (SelectField vn fn _) = [FieldRefNormal vn fn]
                                


selectReturnFields :: Module -> Context -> SelectQuery -> String
selectReturnFields m ctx sq = T.unpack $(codegenFile "codegen/select-return-fields.cg")
    where fields = concatMap (selectFieldRefs m ctx) (sqFields sq)
        

    
getHandlerSelect :: Module -> SelectQuery -> Bool -> [IfFilterParams] -> [TextSearchParams] -> String
getHandlerSelect m sq defaultFilterSort ifFilters textSearches = 
    baseSelectQuery m ctx sq
   ++ (if defaultFilterSort 
        then baseDefaultFilterSort m ctx  
             ++ (concatMap (baseIfFilter m ctx selectVar) ifFilters)
        else "")
   ++ (concatMap (textSearchFilter m ctx) textSearches)  
   ++ (selectReturnFields m ctx sq)
   ++ (T.unpack $(codegenFile "codegen/select-count.cg"))
   ++ (T.unpack $(codegenFile "codegen/select-results.cg"))
    where (_,selectVar) = sqFrom sq
          ctx = sqAliases sq
          orderingLimitOffset = selectOrderingLimitOffset m defaultFilterSort sq
    


getHandlerReturn :: Module -> SelectQuery -> String
getHandlerReturn m sq = T.unpack $(codegenFile "codegen/get-handler-return.cg")
    where 
          ctx = sqAliases sq
          fieldNames = zip (concatMap expand (sqFields sq)) ([1..]:: [Int])
          expand (SelectAllFields vn) = map fieldName $ entityFields e
                where en = fromJust $ ctxLookupEntity ctx vn
                      e = fromJust $ lookupEntity m en    
          expand (SelectField _ fn an') = [ maybe fn id an' ]
          resultFields = map (\(_,i) -> "f"++ show i)  fieldNames
          mappedResultFields = concatMap mapResultField fieldNames
          mapResultField (fn,i) = T.unpack $(codegenFile "codegen/map-result-field.cg")
            
getHandler :: Module -> Resource -> [HandlerParam] -> String
getHandler m r ps = 
    (concatMap (getHandlerParam m r ctx) ps)
    ++ (getHandlerSelect m sq defaultFilterSort ifFilters textSearches)
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

        textSearches = map (\(TextSearchFilter p) -> p) $ filter isTextSearch ps
        isTextSearch (TextSearchFilter _) = True
        isTextSearch _ = False
        
   --     (selectFromEntity, selectFromVariable) = fromJust $ handlerSelectFrom ps
       -- joins = handlerJoins ps 
       -- rjoins = reverse joins

