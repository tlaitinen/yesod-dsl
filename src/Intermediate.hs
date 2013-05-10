module Intermediate where

import qualified AST as A
import Data.Either

data Module = Module {
    modEntities :: [Entity],
    modClasses  :: [Class],
    modEnums    :: [A.EnumType],
    modResources :: [Resource]
}

data Class = Class {
    classLoc         :: A.Location,
    className        :: String,
    classFields      :: [Field],
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
    fieldType        :: FieldType,
    fieldChecks      :: [A.FunctionName],
    fieldDefault     :: Maybe A.FieldValue
}

data FieldType = DataField A.FieldType
               | ERefField A.EntityName
               | CRefField A.ClassName
               | EnumField A.EnumType
               
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
    ghPublic             :: Bool,
    ghDefaultFilterSort  :: Bool,
    ghTextSearchFilters  :: [(A.ParamName, [FieldRef])],
    ghSelectFrom         :: (Entity, A.VariableName),
    ghJoins              :: [Join],
    ghWhere              :: [Expr],
    ghPostTransforms     :: [A.FunctionName],
    ghOrderBy            :: [(FieldRef, A.SortDir)],
    ghPreHooks           :: [A.FunctionName],
    ghPostHooks          :: [A.FunctionName],
    ghReturn             :: Either Entity [(A.ParamName, FieldRef)]
}

data PutHandlerParams = PutHandlerParams {
    puthPublic          :: Bool,
    puthPreTransforms   :: [A.FunctionName],
    puthPreHooks        :: [A.FunctionName],
    puthPostHooks       :: [A.FunctionName],
    puthEntity          :: Entity  
}

data PostHandlerParams = PostHandlerParams {
    posthPublic         :: Bool,
    posthPreTransforms  :: [A.FunctionName],
    posthPreHooks       :: [A.FunctionName],
    posthPostHooks      :: [A.FunctionName],
    posthEntity         :: Entity
}

data DeleteHandlerParams = DeleteHandlerParams {
    dhPublic            :: Bool,
    dhPreHooks          :: [A.FunctionName],
    dhPostHooks         :: [A.FunctionName],
    dhEntity            :: Entity
}

data Join = Join {
    joinType            :: A.JoinType,
    joinEntity          :: Entity,
    joinAlias           :: A.VariableName,
    joinExpr            :: Maybe (FieldRef, A.BinOp, FieldRef)
}

data FieldRef = FieldRefId Entity
              | FieldRefNormal Entity Field

data Expr = AndExpr Expr Expr
          | OrExpr Expr Expr
          | BinOpExpr ValExpr ValExpr

data ValExpr = FieldExpr FieldRef
             | ConstExpr A.FieldValue
