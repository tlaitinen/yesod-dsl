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
    Like -> "`like`"
    Ilike -> "`ilike`"

hsListOp op = case op of
    In -> "`in_`"
    NotIn -> "`notIn`"
type Context = [(EntityName, VariableName, MaybeFlag)]    

ctxLookupEntity :: Context -> VariableName -> Maybe EntityName
ctxLookupEntity ctx vn = maybe Nothing (\(en,_,_) -> Just en) $ find (\(_,vn',_) -> vn == vn') ctx 

ctxLookupVariable :: Context -> EntityName -> Maybe VariableName
ctxLookupVariable ctx en = maybe Nothing (\(_,vn,_) -> Just vn) $ find (\(en',_,_) -> en == en') ctx 

ctxIsMaybe :: Context -> VariableName -> Bool
ctxIsMaybe ctx vn = maybe False (\(_,_,f) -> f) $ find (\(_,vn',_) -> vn == vn') ctx


coerceType :: Maybe BinOp -> String -> String
coerceType (Just op)  s
    | op `elem` [Like, Ilike] = "(" ++ s ++ " :: Text)"
    | otherwise = s
coerceType _ s = s 

projectField :: MaybeFlag -> String
projectField True = " ?. "
projectField False = " ^. "

hsFieldRef :: Context -> Maybe BinOp -> FieldRef -> String
hsFieldRef ctx _ (FieldRefId vn) = vn ++ projectField (ctxIsMaybe ctx vn)
                 ++  (fromJust $ ctxLookupEntity ctx vn) ++ "Id"
hsFieldRef ctx _  (FieldRefNormal vn fn) = vn ++ projectField (ctxIsMaybe ctx vn)
                 ++ (fromJust $ ctxLookupEntity ctx vn) 
                 ++ (upperFirst fn)
hsFieldRef _ _ FieldRefAuthId = "(val authId)"
hsFieldRef _ op  (FieldRefPathParam p) = "(val " ++ coerceType op ("p" ++ show p) ++ ")"
hsFieldRef _ op FieldRefLocalParam = "(val " ++ coerceType op "localParam" ++ ")"


hsOrderBy :: Context -> (FieldRef, SortDir) -> String
hsOrderBy ctx (f,d) = dir d ++ "(" ++ hsFieldRef ctx Nothing f ++ ")"
    where dir SortAsc = "asc "
          dir SortDesc = "desc "

hsValExpr :: Context -> BinOp -> ValExpr -> String
hsValExpr ctx op ve =  case ve of
    FieldExpr fr -> hsFieldRef ctx (Just op) fr
    ConstExpr fv ->  "(val " ++ show fv ++  ")" 
    ConcatExpr e1 e2 -> "(" ++ hsValExpr ctx op e1 ++ ") ++. (" ++ hsValExpr ctx op e2 ++ ")"

isMaybeFieldRef :: Context -> FieldRef -> Bool
isMaybeFieldRef ctx (FieldRefId vn) = ctxIsMaybe ctx vn
isMaybeFieldRef ctx (FieldRefNormal vn fn) = ctxIsMaybe ctx vn
isMaybeFieldRef ctx _ = False

isMaybeExpr :: Context -> ValExpr -> Bool
isMaybeExpr ctx ve = case ve of
    FieldExpr fr -> isMaybeFieldRef ctx fr
    ConstExpr _ -> False
    ConcatExpr e1 e2 -> isMaybeExpr ctx e1 || isMaybeExpr ctx e2

hsListFieldRef :: Context -> FieldRef -> String
hsListFieldRef ctx (FieldRefId vn) = vn ++ " ^. " 
                 ++  (fromJust $ ctxLookupEntity ctx vn) ++ "Id"
hsListFieldRef ctx  (FieldRefNormal vn fn) = vn ++ " ^. " 
                 ++ (fromJust $ ctxLookupEntity ctx vn) 
                 ++ (upperFirst fn)
hsListFieldRef _ FieldRefAuthId = "(valList authId"
hsListFieldRef _  (FieldRefPathParam p) = "(valList p" ++ show p ++ ")"
hsListFieldRef _ FieldRefLocalParam = "(valList localParam)"




hsExpr :: Context-> Expr -> String
hsExpr ctx expr = case expr of
    AndExpr e1 e2 -> "(" ++ hsExpr ctx e1 ++ ") &&. (" ++ hsExpr ctx e2 ++ ")"
    OrExpr e1 e2 ->"(" ++ hsExpr ctx e1 ++ ") ||. (" ++ hsExpr ctx e2 ++ ")"
    NotExpr e -> "not_ (" ++ hsExpr ctx e ++ ")"
    BinOpExpr e1 op e2 -> binOpExpr e1 op e2 
    ListOpExpr fr1 op fr2 -> "(" ++ hsListFieldRef ctx fr1 ++ ") " ++ hsListOp op ++ " (" ++ hsListFieldRef ctx fr2 ++ ")"
    where
        
        binOpExpr e1 op e2 = promoteJust 0 e1 e2 ("(" ++ (hsValExpr ctx op e1) ++ ")") ++ " " ++ hsBinOp op ++ " " ++ (promoteJust  1 e1 e2 ("(" ++ (hsValExpr ctx op e2) ++ ")"))
        promoteJust :: Int -> ValExpr -> ValExpr -> String -> String
        promoteJust pos e1 e2 content = if (isMaybeExpr ctx e1 == False && isMaybeExpr ctx e2 == True && pos == 0) || 
                (isMaybeExpr ctx e1 == True && isMaybeExpr ctx e2 == False && pos == 1)
            then "(just " ++ content ++ ")"
            else content



