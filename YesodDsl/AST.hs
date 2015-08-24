{-# LANGUAGE DeriveDataTypeable #-}
module YesodDsl.AST where

import Data.Maybe
import Data.List
import Data.Char
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map


data Module = Module {
    modName      :: Maybe String,  
    modEntities  :: [Entity],      
    modClasses   :: [Class],       
    modEnums     :: [EnumType],    
    modRoutes    :: [Route],       
    modImports   :: [Import]
} deriving (Show, Data, Typeable)

moduleName :: Module -> String
moduleName m = fromMaybe "" (modName m)

emptyModule :: Module
emptyModule = Module {
    modName = Nothing,
    modEntities = [],
    modClasses = [],
    modEnums = [],
    modRoutes = [],
    modImports = []
}


data Import = Import {
    importModule    :: String,
    importFunctions :: [FunctionName]
} deriving (Show, Data, Typeable)

type ClassName = String
type ParamName = String
type EntityName = String
type EnumName = String
type EnumValue = String
type FunctionName = String
type FieldName = String 

data FieldType = FTWord32 | FTWord64 | FTInt | FTInt32 | FTInt64 | FTText 
               | FTBool | FTDouble | FTTimeOfDay | FTDay | FTUTCTime 
               | FTCheckmark deriving (Eq,Show,Data,Typeable)

type RowNum = Int
type ColNum = Int
data Location = Loc FilePath RowNum ColNum deriving (Eq,Data,Typeable)

instance Show Location where
    show (Loc path row col) = path ++ ":" ++ show row ++ ":" ++ show col


data Unique = Unique {
    uniqueName :: String,
    uniqueFields :: [FieldName]
} deriving (Show, Eq, Data, Typeable)

data HandlerType = GetHandler 
                 | PutHandler 
                 | PostHandler 
                 | DeleteHandler 
                 deriving (Eq, Data, Typeable) 

instance Show HandlerType where
    show GetHandler = "GET"
    show PutHandler = "PUT"
    show PostHandler = "POST"
    show DeleteHandler = "DELETE"
type VariableName = String
data JoinType = InnerJoin 
              | CrossJoin
              | LeftOuterJoin
              | RightOuterJoin
              | FullOuterJoin
              deriving (Show, Eq, Data, Typeable)
isOuterJoin :: JoinType -> Bool
isOuterJoin LeftOuterJoin = True
isOuterJoin RightOuterJoin = True
isOuterJoin FullOuterJoin = True
isOuterJoin _ = False

data BinOp = Eq | Ne | Lt | Gt | Le | Ge | Like | Ilike | Is | In | NotIn  deriving (Show,Eq, Data,Typeable)     
data ValBinOp = Add | Sub | Div | Mul | Concat deriving (Show,Eq, Data,Typeable)    

data BoolExpr = AndExpr BoolExpr BoolExpr
          | OrExpr BoolExpr BoolExpr
          | NotExpr BoolExpr
          | BinOpExpr ValExpr BinOp ValExpr 
          | ExistsExpr SelectQuery
          | ExternExpr FunctionName [FunctionParam]
          deriving (Show, Eq, Data, Typeable)

data FunctionParam = FieldRefParam FieldRef
                   | VerbatimParam String
    deriving (Show, Eq, Data, Typeable)

type MaybeLevel = Int
    
data ValExpr = FieldExpr FieldRef
           | ConcatManyExpr [ValExpr]
           | ValBinOpExpr ValExpr ValBinOp ValExpr 
           | RandomExpr
           | FloorExpr ValExpr
           | CeilingExpr ValExpr
           | ExtractExpr FieldName ValExpr
           | SubQueryExpr SelectQuery 
           deriving (Show, Eq, Data, Typeable)

type EntityRef = Either EntityName Entity
entityRefName :: EntityRef -> EntityName
entityRefName (Left en) = en
entityRefName (Right e) = entityName e
         
data Stmt = Public 
                  | ParamDefault ParamName FieldValue
                  | DefaultFilterSort
                  | Select SelectQuery 
                  | IfFilter IfFilterParams
                  | DeleteFrom EntityRef VariableName (Maybe BoolExpr)
                  | GetById EntityRef FieldRef VariableName
                  | Update EntityRef FieldRef (Maybe [FieldRefMapping])
                  | Insert EntityRef (Maybe (Maybe VariableName, [FieldRefMapping])) (Maybe VariableName)
                  | Return [FieldRefMapping]
                  | Require SelectQuery
                  | For VariableName FieldRef [Stmt]
                  | Call FunctionName [FieldRef]
                  deriving (Show, Eq, Data, Typeable) 
type UseParamFlag = Bool    
type IfFilterParams = (ParamName,[Join],BoolExpr,UseParamFlag)

data SelectQuery = SelectQuery {
    sqFields       :: [SelectField],
    sqFrom         :: (EntityRef, VariableName),
    sqJoins        :: [Join],
    sqWhere        :: Maybe BoolExpr,
    sqOrderBy        :: [(FieldRef, SortDir)],
    sqLimitOffset  :: (Int, Int)
} deriving (Show, Eq, Data, Typeable)    

type MaybeFlag = Bool
sqAliases :: SelectQuery -> Map.Map VariableName (Entity,MaybeFlag)
sqAliases sq = Map.fromList $ catMaybes $ (either (\_ -> Nothing) (\e -> Just (vn,(e,False))) er) : [ either (\_ -> Nothing) (\e -> Just (joinAlias j, (e, isOuterJoin $ joinType j))) $ joinEntity j 
                             | j <- sqJoins sq ]
    where (er,vn) = sqFrom sq                             


data SelectField = SelectAllFields VariableRef
                 | SelectField VariableRef FieldName (Maybe VariableName)
                 | SelectIdField VariableRef (Maybe VariableName)
                 | SelectValExpr ValExpr VariableName
                 deriving (Show, Eq, Data, Typeable)

    
data Join = Join {
    joinType   :: JoinType,
    joinEntity :: EntityRef,
    joinAlias  :: VariableName,
    joinExpr   :: Maybe BoolExpr
} deriving (Show, Eq, Data, Typeable)

type FieldRefMapping = (ParamName, FieldRef, Maybe FunctionName)

data CheckmarkValue = Active | Inactive
                    deriving (Show, Eq, Ord, Data, Typeable)
   
data SortDir = SortAsc | SortDesc deriving (Show, Eq, Data, Typeable)                   

data Handler = Handler {
    handlerLoc	  :: Location,
    handlerType   :: HandlerType,
    handlerStmts :: [Stmt] 
} deriving (Show, Eq, Data, Typeable)

data Entity = Entity {
    entityLoc        :: Location,
    entityName       :: String,
    entityTable      :: Maybe String,
    entityInstances :: [ClassName],
    entityFields     :: [Field],
    entityClassFields :: [Field],
    entityUniques    :: [Unique],
    entityDeriving   :: [ClassName],
    entityChecks     :: [FunctionName]
} deriving (Show, Eq, Data, Typeable)


data Route = Route {
    routeLoc :: Location,
    routePath :: [PathPiece],
    routeHandlers :: [Handler]
} deriving (Show, Eq, Data, Typeable)

routePathParams :: Route -> [PathPiece]
routePathParams = (filter isPathParam) . routePath

isPathParam :: PathPiece -> Bool
isPathParam (PathId _ _) = True
isPathParam _ = False

        

handlerName :: Route -> Handler -> String
handlerName r h = routeName (routePath r) ++ " " ++ show (handlerType h)

routeName :: [PathPiece] -> String
routeName ps = "/" ++ intercalate "/" (map show ps)

data PathPiece = PathText String
               | PathId Location EntityName
               deriving (Eq, Data, Typeable)
instance Show PathPiece where
    show (PathText s) = s
    show (PathId _ en) = "#" ++ en ++ "Id"
    
data FieldRef = SqlId VariableRef
              | SqlField VariableRef FieldName 
              | AuthId
              | AuthField FieldName
              | LocalParam
              | LocalParamField VariableRef FieldName
              | EnumValueRef EnumName FieldName
              | PathParam Int 
              | RequestField FieldName
              | NamedLocalParam VariableName
              | Const FieldValue 
              | Now -- temporarily here
              deriving (Show, Eq, Data, Typeable) 

data VariableRef = Var VariableName EntityRef MaybeFlag
                 deriving (Show, Eq, Data, Typeable)

entityFieldByName :: Entity -> FieldName -> Field
entityFieldByName e fn = maybe (error $ "No field " ++ fn ++ " in " ++ entityName e) id
                               (find (\f -> fieldName f == fn) (entityFields e))

data EnumType = EnumType {
    enumLoc :: Location,
    enumName :: String,
    enumValues :: [String]
} deriving (Show, Eq, Data, Typeable)

data Class = Class {
    classLoc     :: Location,
    className    :: String,
    classFields  :: [Field],
    classUniques :: [Unique]
} deriving (Show, Eq, Data, Typeable)

type DefaultValue = String
type IsListFlag = Bool
data FieldContent = NormalField FieldType 
                    | EntityField EntityName 
                    | EnumField EnumName 
                deriving (Show,Eq, Data, Typeable)
   

data Field = Field {
    fieldLoc       :: Location,
    fieldOptional  :: Bool,
    fieldName      :: FieldName,
    fieldContent   :: FieldContent,
    fieldOptions   :: [FieldOption],
    fieldClassName :: Maybe (ClassName,FieldName)
} deriving (Show,Eq, Data, Typeable)

fieldInternal :: Field -> Bool
fieldInternal = (FieldInternal `elem`) . fieldOptions

data FieldOption = FieldCheck FunctionName
                 | FieldDefault FieldValue
                 | FieldColumnName FieldName
                 | FieldInternal
                 deriving (Show, Eq, Data, Typeable)


data FieldValue = StringValue String
                | IntValue Int
                | FloatValue Double
                | BoolValue Bool
                | NothingValue
                | CheckmarkValue CheckmarkValue
                | EnumFieldValue EnumName EnumValue
                | EmptyList
                deriving (Show, Eq, Ord, Data, Typeable)
fieldValueToSql :: FieldValue -> String    
fieldValueToSql fv = case fv of
    (StringValue s) -> "'" ++ s ++ "'"
    (IntValue i) -> show i
    (FloatValue d) -> show d
    (BoolValue b) -> show b
    NothingValue -> "NULL"
    CheckmarkValue Active -> "True"
    CheckmarkValue Inactive -> "NULL"
    EnumFieldValue _ ev ->  "'" ++ ev ++ "'"
    EmptyList -> "'[]'"
   
fieldValueToEsqueleto :: FieldValue -> String    
fieldValueToEsqueleto fv = case fv of
    (StringValue s) -> "\"" ++ s ++ "\""
    (IntValue i) -> show i
    (FloatValue d) -> show d
    (BoolValue b) -> show b
    NothingValue -> "nothing"
    CheckmarkValue Active -> "Active"
    CheckmarkValue Inactive -> "Inactive"
    EnumFieldValue en ev -> en ++ ev
    EmptyList -> "[]"

fieldValueToHs :: FieldValue -> String
fieldValueToHs fv = case fv of
    StringValue s -> "\"" ++ s ++ "\""
    IntValue i -> show i
    FloatValue d -> show d
    BoolValue b -> show b
    NothingValue -> "Nothing"
    CheckmarkValue Active -> "Active"
    CheckmarkValue Inactive -> "Inactive"
    EnumFieldValue en ev ->  en ++ ev
    EmptyList -> "[]"


        
   
fieldDefault :: Field -> Maybe FieldValue
fieldDefault f = listToMaybe [ fv | FieldDefault fv <- universeBi f ]

fieldColumnName :: Field -> Maybe FieldName
fieldColumnName f = listToMaybe [ cn | FieldColumnName cn <- universeBi f ]

fieldChecks :: Field -> [FunctionName]
fieldChecks f = [ func | FieldCheck func <- universeBi f ]

lookupField :: Entity -> FieldName -> Maybe Field         
lookupField e fn = find ((==fn) . fieldName) $ entityFields e
lowerFirst :: String -> String
lowerFirst (a:b) = (toLower a):b
lowerFirst a = a

upperFirst :: String -> String
upperFirst (a:b) = (toUpper a):b
upperFirst a = a
