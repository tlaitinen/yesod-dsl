{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Esqueleto where
import AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)

hsBinOp :: BinOp -> String
hsBinOp op = case op of
    Eq -> "==."
    Ne -> "!=."
    Lt -> "<."
    Gt -> ">."
    Le -> "<=."
    Ge -> ">=."
    Like -> "like"


handlerVariableEntity :: [HandlerParam] -> VariableName -> Maybe EntityName
handlerVariableEntity ps vn = case filter match ps of
    ((Select sq):_) -> fromSq sq
    ((DeleteFrom en _ _):_) -> Just en
    _ -> Nothing
   where match (Select sq) = (isJust . fromSq) sq 
         match (DeleteFrom _ vn' _) = vn == vn' 
         match _= False
         fromSq sq = listToMaybe [ en | (en,vn') <- sqAliases sq, vn == vn' ]

hsFieldRef :: [HandlerParam] -> FieldRef -> String
hsFieldRef ps (FieldRefId vn) = vn ++ " ^. " 
                 ++  (fromJust $ handlerVariableEntity ps vn) ++ "Id"
hsFieldRef ps (FieldRefNormal vn fn) = vn ++ " ^. " 
                 ++ (fromJust $ handlerVariableEntity ps vn) 
                 ++ (upperFirst fn)
hsFieldRef _ FieldRefAuthId = "(val authId)"
hsFieldRef _ (FieldRefPathParam p) = "(val p" ++ show p ++ ")"


hsOrderBy :: [HandlerParam] -> (FieldRef, SortDir) -> String
hsOrderBy ps (f,d) = dir d ++ "(" ++ hsFieldRef ps f ++ ")"
    where dir SortAsc = "asc "
          dir SortDesc = "desc "


hsValExpr :: [HandlerParam] -> ValExpr -> String
hsValExpr ps ve = case ve of
    FieldExpr fr -> hsFieldRef ps fr
    ConstExpr fv -> "(val " ++ show fv ++ ")"

hsExpr :: [HandlerParam] -> Expr -> String
hsExpr ps expr = case expr of
    AndExpr e1 e2 -> "(" ++ hsExpr ps e1 ++ ") &&. (" ++ hsExpr ps e2 ++ ")"
    OrExpr e1 e2 -> "(" ++ hsExpr ps e1 ++ ") ||. (" ++ hsExpr ps e2 ++ ")"
    BinOpExpr e1 op e2 -> hsValExpr ps e1 ++ " " ++ hsBinOp op ++ hsValExpr ps e2


