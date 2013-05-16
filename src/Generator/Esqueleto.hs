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


type Context = [(EntityName, VariableName)]    

ctxLookupEntity :: Context -> VariableName -> Maybe EntityName
ctxLookupEntity ctx vn = maybe Nothing (\(en,_) -> Just en) $ find (\(_,vn') -> vn == vn') ctx 

hsFieldRef :: Context -> FieldRef -> String
hsFieldRef ctx (FieldRefId vn) = vn ++ " ^. " 
                 ++  (fromJust $ ctxLookupEntity ctx vn) ++ "Id"
hsFieldRef ctx (FieldRefNormal vn fn) = vn ++ " ^. " 
                 ++ (fromJust $ ctxLookupEntity ctx vn) 
                 ++ (upperFirst fn)
hsFieldRef _ FieldRefAuthId = "(val authId)"
hsFieldRef _ (FieldRefPathParam p) = "(val p" ++ show p ++ ")"
hsFieldRef _ FieldRefLocalParam = "(val (fromPathPiece localParam))"

hsOrderBy :: Context -> (FieldRef, SortDir) -> String
hsOrderBy ctx (f,d) = dir d ++ "(" ++ hsFieldRef ctx f ++ ")"
    where dir SortAsc = "asc "
          dir SortDesc = "desc "


hsValExpr :: Context -> ValExpr -> String
hsValExpr ctx ve = case ve of
    FieldExpr fr -> hsFieldRef ctx fr
    ConstExpr fv -> "(val " ++ show fv ++ ")"

hsExpr :: Context-> Expr -> String
hsExpr ctx expr = case expr of
    AndExpr e1 e2 -> "(" ++ hsExpr ctx e1 ++ ") &&. (" ++ hsExpr ctx e2 ++ ")"
    OrExpr e1 e2 -> "(" ++ hsExpr ctx e1 ++ ") ||. (" ++ hsExpr ctx e2 ++ ")"
    BinOpExpr e1 op e2 -> hsValExpr ctx e1 ++ " " ++ hsBinOp op ++ hsValExpr ctx e2


