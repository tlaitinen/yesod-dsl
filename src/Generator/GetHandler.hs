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
handlerEntities :: [HandlerParam] -> [(EntityName, VariableName)]
handlerEntities = concatMap match
    where match (Select sq) = sqAliases sq
          match _ = []


handlerFields :: Module -> [HandlerParam] -> [(Entity, VariableName, Field)]
handlerFields m ps = [ (e,vn,f) | e <- modEntities m,
                                  (en,vn) <- handlerEntities ps,
                                  entityName e == en,
                                  f <- entityFields e ]

defaultFilterField :: (Entity, VariableName, Field) -> String
defaultFilterField (e,vn,f) = T.unpack $(codegenFile "codegen/default-filter-field.cg")

defaultFilterFields :: Module -> [HandlerParam] -> String
defaultFilterFields m ps = T.unpack $(codegenFile "codegen/default-filter-fields.cg") 
    where fields = concatMap defaultFilterField (handlerFields m ps)

defaultSortField :: (Entity, VariableName, Field) -> String    
defaultSortField (e,vn,f) = T.unpack $(codegenFile "codegen/default-sort-field.cg")
defaultSortFields :: Module -> [HandlerParam] -> String
defaultSortFields m ps = T.unpack $(codegenFile "codegen/default-sort-fields.cg")
    where fields = concatMap defaultSortField (handlerFields m ps)

getHandlerParam :: Module -> Resource -> [HandlerParam] -> HandlerParam -> String
getHandlerParam m r ps DefaultFilterSort = T.unpack $(codegenFile "codegen/default-filter-sort-param.cg")
    ++ (T.unpack $(codegenFile "codegen/offset-limit-param.cg"))
getHandlerParam m r ps (TextSearchFilter (pn, fs)) = T.unpack $(codegenFile "codegen/text-search-filter-param.cg")
getHandlerParam _ _ _ _ = ""        


joinDef :: Join-> String
joinDef (Join jt _ vn _) = rstrip $ T.unpack $(codegenFile "codegen/join-def.cg")


isMaybeFieldRef :: Module -> [HandlerParam] -> FieldRef -> Bool
isMaybeFieldRef m ps (FieldRefNormal vn fn) = fieldOptional $ fromJust $ lookupField m (fromJust $ handlerVariableEntity ps vn) fn
isMaybeFieldRef _ _  _ = False

makeJustField :: Bool -> String -> String    
makeJustField True f = "(just " ++ f ++ ")"
makeJustField False f = f

mapJoinExpr :: Module -> [HandlerParam] -> Join -> String
mapJoinExpr m ps (Join _ en vn (Just (f1, op, f2))) = T.unpack $(codegenFile "codegen/join-expr.cg")
    where f1just = f1maybe == False && f2maybe == True
          f2just = f2maybe == False && f1maybe == True
          f1maybe = isMaybeFieldRef m ps f1
          f2maybe = isMaybeFieldRef m ps f2
mapJoinExpr m _ _ = ""

indent :: [String] -> [String]
indent = map ("   "++)
mapJoinExprIndent :: Module -> [HandlerParam] -> Join -> String
mapJoinExprIndent m ps = unlines . indent . lines . (mapJoinExpr m ps)


textSearchFilterField :: [HandlerParam] -> ParamName -> FieldRef -> String
textSearchFilterField ps pn f = rstrip $ T.unpack $(codegenFile "codegen/text-search-filter-field.cg")

    
baseSelectQuery :: Module -> [HandlerParam] -> SelectQuery -> String
baseSelectQuery m ps sq = T.unpack $(codegenFile "codegen/base-select-query.cg")
    where (selectEntity, selectVar) = sqFrom sq 
          maybeWhere = case sqWhere sq of
             Just expr -> T.unpack $(codegenFile "codegen/where-expr.cg")
             Nothing -> ""

baseDefaultFilterSort :: Module -> [HandlerParam] -> String
baseDefaultFilterSort = defaultFilterFields

baseIfFilter :: Module -> [HandlerParam] -> VariableName -> IfFilterParams -> String
baseIfFilter m ps selectVar (pn,joins,expr) = T.unpack $(codegenFile "codegen/base-if-filter.cg")

getHandlerSelect :: Module -> [HandlerParam] -> SelectQuery -> Bool -> [IfFilterParams] -> String
getHandlerSelect m ps sq defaultFilterSort ifFilters = 
    baseSelectQuery m ps sq
   ++ (if defaultFilterSort 
        then baseDefaultFilterSort m ps  
             ++ (concatMap (baseIfFilter m ps selectVar) ifFilters)
        else "")
    where (_,selectVar) = sqFrom sq
    
    

getHandler :: Module -> Resource -> [HandlerParam] -> String
getHandler m r ps = 
    (concatMap (getHandlerParam m r ps) ps)
    ++ (getHandlerSelect m ps sq defaultFilterSort ifFilters)

 --   ++ (let result = "count" :: String in  T.unpack $(codegenFile "codegen/get-handler-select.cg"))
 --   ++ (concatMap (mapJoinExpr m ps) rjoins)
  --  ++ (concatMap (getHandlerCountExpr m ps) ps)
   -- ++ (T.unpack $(codegenFile "codegen/select-return-count.cg"))   
  --  ++ (let result = "result" :: String in T.unpack $(codegenFile "codegen/get-handler-select.cg"))
  --  ++ (concatMap (mapJoinExpr m ps) rjoins)
  --  ++ (concatMap (getHandlerSQLExpr m ps) ps)
  --  ++ (getHandlerSQLReturn ps)
  --  ++ (getReturn ps)
    where 
        (Select sq) = (fromJust . listToMaybe . (filter isSelect)) ps
        isSelect (Select _) = True
        isSelect _ = False
    
        defaultFilterSort = DefaultFilterSort `elem` ps

        ifFilters = map (\(IfFilter f) -> f) $ filter isIfFilter ps
        isIfFilter (IfFilter _) = True
        isIfFilter _ = False
        
   --     (selectFromEntity, selectFromVariable) = fromJust $ handlerSelectFrom ps
       -- joins = handlerJoins ps 
       -- rjoins = reverse joins

