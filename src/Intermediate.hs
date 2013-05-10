module Intermediate where

import qualified AST as A
import Data.Either

data Enum = Enum {
    enumLoc          :: A.Location,
    enumName         :: String,
    enumValues       :: [String]    
}

data Class = Class {
    classLoc         :: A.Location,
    className        :: String,
    classFields      :: Field,
    classUniques     :: [Unique]
}

data Entity = Entity {
    entityLoc        :: A.Location,
    entityName       :: String,
    entityImplements :: [Class],
    entityFields     :: [Field],
    entityUniques    :: [Unique],
    entityDeriving   :: [A.ClassName],
    entityChecks     :: [A.FunctionName]
} 

data Field = Field {
    fieldOptional    :: Bool,
    fieldName        :: String,
    fieldType        :: Either A.FieldType Entity,
    fieldChecks      :: [A.FunctionName],
    fieldDefault     :: Maybe A.FieldValue,
    fieldOwner       :: Entity
}

data Unique = Unique A.UniqueName [Field]
                  
data Resource = Resource {
    resLoc           :: A.Location,
    resRoute         :: [PathPiece],
    resHandlers      :: [Handler]
}    

data PathPiece = PathText String
               | PathId Entity

data Handler = GetHandler GetHandlerParams
             | PutHandler PutHandlerParams
             | PostHandler PostHandlerParams
             | DeleteHandler DeleteHandlerParams

data GetHandlerParams = GetHandlerParams {
    ghPublic            :: Bool,
    ghDefaultFilterSort :: Bool,
    ghTextSearchFilter  :: (A.ParamName, [FieldRef]),
    ghSelectFrom        :: (Entity, A.VariableName),
    ghJoins             :: [Join],
    ghWhere             :: Expr,
    ghPostTransform     :: A.FunctionName,
    ghOrderBy           :: [(FieldRef, A.SortDir)],
    ghPreHook           :: A.FunctionName,
    ghPostHook          :: A.FunctionName
}

data PutHandlerParams = PutHandlerParams {
    puthPublic          :: Bool,
    puthPreTransform    :: A.FunctionName,
    puthPreHook         :: A.FunctionName,
    puthPostHook        :: A.FunctionName,
    puthEntity          :: Entity  
}

data PostHandlerParams = PostHandlerParams {
    posthPublic         :: Bool,
    posthPreTransform   :: A.FunctionName,
    posthPreHook        :: A.FunctionName,
    posthPostHook       :: A.FunctionName,
    posthEntity         :: Entity
}

data DeleteHandlerParams = DeleteHandlerParams {
    dhPublic            :: Bool,
    dhPreHook           :: A.FunctionName,
    dhPostHook          :: A.FunctionName,
    dhEntity            :: Entity
}

data Join = Join {
    joinType            :: A.JoinType,
    joinEntity          :: Entity,
    joinAlias           :: A.VariableName,
    joinExpr            :: Maybe (FieldRef, A.BinOp, FieldRef)
}

data FieldRef = FieldRefId Entity
              | FieldRefNormal Field

data Expr = AndExpr Expr Expr
          | OrExpr Expr Expr
          | BinOpExpr ValExpr ValExpr

data ValExpr = FieldExpr FieldRef
             | ConstExpr A.FieldValue
