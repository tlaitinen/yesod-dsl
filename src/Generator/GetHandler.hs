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
getHandlerParam m r ps DefaultFilterSort = T.unpack $(codegenFile "codegen/default-filter-sort.cg")
    ++ (T.unpack $(codegenFile "codegen/offset-limit-param.cg"))
getHandlerParam m r ps (TextSearchFilter pn fs) = T.unpack $(codegenFile "codegen/text-search-filter-param.cg")
getHandlerParam _ _ _ _ = ""        


getHandlerJoinDef :: (JoinType, EntityName, VariableName, (Maybe (FieldRef, BinOp, FieldRef))) -> String
getHandlerJoinDef (jt, _, vn, _) = T.unpack $(codegenFile "codegen/get-handler-join-def.cg")


isMaybeFieldRef :: Module -> [HandlerParam] -> FieldRef -> Bool
isMaybeFieldRef m ps (FieldRefNormal vn fn) = fieldOptional $ fromJust $ lookupField m (fromJust $ handlerVariableEntity ps vn) fn
isMaybeFieldRef _ _  _ = False

makeJustField :: Bool -> String -> String    
makeJustField True f = "(just " ++ f ++ ")"
makeJustField False f = f

getHandlerJoinExpr :: Module -> [HandlerParam] -> (JoinType, EntityName, VariableName, (Maybe (FieldRef, BinOp, FieldRef))) -> String
getHandlerJoinExpr m ps (_, en, vn, (Just (f1, op, f2))) = T.unpack $(codegenFile "codegen/get-handler-join-expr.cg")
    where f1just = f1maybe == False && f2maybe == True
          f2just = f2maybe == False && f1maybe == True
          f1maybe = isMaybeFieldRef m ps f1
          f2maybe = isMaybeFieldRef m ps f2
getHandlerJoinExpr m _ _ = ""
textSearchFilterField :: [HandlerParam] -> ParamName -> FieldRef -> String
textSearchFilterField ps pn f = rstrip $ T.unpack $(codegenFile "codegen/text-search-filter-field.cg")

getHandlerCountExpr :: Module -> [HandlerParam] -> HandlerParam -> String
getHandlerCountExpr m ps p = case p of
    DefaultFilterSort -> defaultFilterFields m ps
    TextSearchFilter pn fs -> let fields = map (textSearchFilterField ps pn) fs in T.unpack $(codegenFile "codegen/text-search-filter.cg")
    -- (Where expr) -> T.unpack $(codegenFile "codegen/get-handler-where-expr.cg")
    _ -> ""

getHandlerSQLExpr :: Module -> [HandlerParam] -> HandlerParam -> String
getHandlerSQLExpr m ps p = case p of
    DefaultFilterSort -> defaultFilterFields m ps ++ defaultSortFields m ps 
                       ++ (T.unpack $(codegenFile "codegen/offset-limit.cg"))
    TextSearchFilter pn fs -> let fields = map (textSearchFilterField ps pn) fs in T.unpack $(codegenFile "codegen/text-search-filter.cg")
    --(Where expr) -> T.unpack $(codegenFile "codegen/get-handler-where-expr.cg")
   -- OrderBy fields -> T.unpack $(codegenFile "codegen/get-handler-order-by.cg")
    --Limit limit -> T.unpack $(codegenFile "codegen/get-handler-limit.cg")
   -- Offset offset -> T.unpack $(codegenFile "codegen/get-handler-offset.cg")
    _ -> ""

getHandlerSQLReturn :: [HandlerParam] -> String
getHandlerSQLReturn ps = ""
--T.unpack $ case handlerReturn ps of
--    Left vn -> $(codegenFile "codegen/select-return-entity.cg")
--    Right fs -> let fields = [ fr | (_,fr) <- fs ] in $(codegenFile "codegen/select-return-fields.cg") 
   
    
getReturn :: [HandlerParam] -> String
getReturn ps =""
--T.unpack $ case handlerReturn ps of
--    Left vn -> $(codegenFile "codegen/return-entity.cg")
--    Right fs -> let
--        params = [ pn | (pn,_) <- fs ]
--        fields = map mapReturnField params
 --       in $(codegenFile "codegen/return-fields.cg")
--    where
 --       mapReturnField pn = rstrip $ T.unpack $(codegenFile "codegen/return-field.cg")

getHandler :: Module -> Resource -> [HandlerParam] -> String
getHandler m r ps = 
    (concatMap (getHandlerParam m r ps) ps)
 --   ++ (let result = "count" :: String in  T.unpack $(codegenFile "codegen/get-handler-select.cg"))
 --   ++ (concatMap (getHandlerJoinExpr m ps) rjoins)
  --  ++ (concatMap (getHandlerCountExpr m ps) ps)
   -- ++ (T.unpack $(codegenFile "codegen/select-return-count.cg"))   
  --  ++ (let result = "result" :: String in T.unpack $(codegenFile "codegen/get-handler-select.cg"))
  --  ++ (concatMap (getHandlerJoinExpr m ps) rjoins)
  --  ++ (concatMap (getHandlerSQLExpr m ps) ps)
  --  ++ (getHandlerSQLReturn ps)
  --  ++ (getReturn ps)
    where 
   --     (selectFromEntity, selectFromVariable) = fromJust $ handlerSelectFrom ps
       -- joins = handlerJoins ps 
       -- rjoins = reverse joins

