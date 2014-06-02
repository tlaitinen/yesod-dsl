{-# LANGUAGE DeriveDataTypeable #-}
-- | Abstract Syntax Tree of yesod-dsl definition.
module YesodDsl.AST where

import YesodDsl.Lexer
import Data.Maybe
import Data.List
import Data.Char
import Data.Generics.Uniplate.Data
import Data.Data (Data)
import Data.Typeable (Typeable)
-- | definitions in single file form a 'Module'
data Module = Module {
    modName      :: Maybe String,  -- ^ top-level module must have a name
    modImports   :: [FilePath],    -- ^ modules may import other modules
    modEntities  :: [Entity],      -- ^ database entity definitions
    modClasses   :: [Class],       -- ^ entity classes
    modEnums     :: [EnumType],    -- ^ enumerated field types 
    modRoutes    :: [Route],       -- ^ HTTP routes
    modDefines   :: [Define]       -- ^ common expressions
} deriving (Show, Data, Typeable)

moduleName :: Module -> String
moduleName = fromJust . modName

emptyModule = Module {
    modName = Nothing,
    modImports = [],
    modEntities = [],
    modClasses = [],
    modEnums = [],
    modRoutes = [],
    modDefines = []
}

type ClassName = String
type ParamName = String
type EntityName = String
type EnumName = String

data FieldType = FTWord32 | FTWord64 | FTInt32 | FTInt64 | FTText 
               | FTBool | FTDouble | FTTimeOfDay | FTDay | FTUTCTime 
               | FTZonedTime deriving (Eq,Show,Data,Typeable)

type FieldName = String 
type PathName = String
type UniqueName = String
type QueryName = String
data Location = Loc FilePath Int Int deriving (Eq,Data,Typeable)

instance Show Location where
    show (Loc path row col) = path ++ ":" ++ show row ++ ":" ++ show col

mkLoc t = Loc "" (tokenLineNum t) (tokenColNum t)

-- | macro definition, currently used only to define parametrized 
-- sub-select-queries
data Define = Define {
    defineName :: String,
    defineLoc :: Location,
    defineParams :: [ParamName],
    defineContent :: DefineContent
} deriving (Show, Eq, Data, Typeable)

data DefineContent = DefineSubQuery SelectQuery 
                     deriving (Show, Eq, Data, Typeable)

data Unique = Unique {
    uniqueName :: UniqueName,
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
          | BinOpExpr ValExpr BinOp ValExpr deriving (Show, Eq, Data, Typeable)
data ValExpr = FieldExpr FieldRef
           | ConstExpr FieldValue 
           | ConcatManyExpr [ValExpr]
           | ValBinOpExpr ValExpr ValBinOp ValExpr 
           | RandomExpr
           | FloorExpr ValExpr
           | CeilingExpr ValExpr
           | ExtractExpr FieldName ValExpr
           | SubQueryExpr SelectQuery 
           | ApplyExpr FunctionName [ParamName]
           deriving (Show, Eq, Data, Typeable)
data HandlerParam = Public 
                  | DefaultFilterSort
                  | Select SelectQuery 
                  | IfFilter IfFilterParams
                  | DeleteFrom EntityName VariableName (Maybe BoolExpr)
                  | GetById EntityName InputFieldRef VariableName
                  | Update EntityName InputFieldRef (Maybe [InputField])
                  | Insert EntityName (Maybe [InputField]) (Maybe VariableName)
                  | Return [OutputField]
                  | Require SelectQuery
                  | For VariableName InputFieldRef [HandlerParam]
                  | Call FunctionName [InputFieldRef]
                  deriving (Show, Eq, Data, Typeable) 
type UseParamFlag = Bool    
type IfFilterParams = (ParamName,[Join],BoolExpr,UseParamFlag)

data SelectQuery = SelectQuery {
    sqFields       :: [SelectField],
    sqFrom         :: (EntityName, VariableName),
    sqJoins        :: [Join],
    sqWhere        :: Maybe BoolExpr,
    sqOrderBy        :: [(FieldRef, SortDir)],
    sqLimitOffset  :: (Int, Int)
} deriving (Show, Eq, Data, Typeable)    

type MaybeFlag = Bool
sqAliases :: SelectQuery -> [(EntityName, VariableName, MaybeFlag)]
sqAliases sq = (en,vn,False) : [ (joinEntity j, joinAlias j, 
                                  isOuterJoin (joinType j)) 
                             | j <- sqJoins sq]
    where (en,vn) = sqFrom sq                             

data SelectField = SelectAllFields VariableName
                 | SelectField VariableName FieldName (Maybe VariableName)
                 | SelectIdField VariableName (Maybe VariableName)
                 | SelectParamField VariableName ParamName (Maybe VariableName)
                 | SelectValExpr ValExpr VariableName
                 deriving (Show, Eq, Data, Typeable)

    
data Join = Join {
    joinType   :: JoinType,
    joinEntity :: EntityName,
    joinAlias  :: VariableName,
    joinExpr   :: Maybe BoolExpr
} deriving (Show, Eq, Data, Typeable)

type InputField = (ParamName, InputFieldRef)

data InputFieldRef = InputFieldNormal FieldName
                   | InputFieldAuthId
                   | InputFieldAuth FieldName
                   | InputFieldPathParam Int
                   | InputFieldLocalParam VariableName
                   | InputFieldLocalParamField VariableName FieldName
                   | InputFieldConst FieldValue
                   | InputFieldNow
                    deriving (Show, Eq, Ord, Data, Typeable)
                    
type OutputField = (ParamName, OutputFieldRef)
data OutputFieldRef = OutputFieldLocalParam VariableName 
    deriving (Show,Eq, Data, Typeable)

data SortDir = SortAsc | SortDesc deriving (Show, Eq, Data, Typeable)                   

data Handler = Handler {
    handlerLoc	  :: Location,
    handlerType   :: HandlerType,
    handlerParams :: [HandlerParam] 
} deriving (Show, Eq, Data, Typeable)

data Entity = Entity {
    entityLoc        :: Location,
    entityName       :: String,
    entityInstances :: [ClassName],
    entityFields     :: [Field],
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
    where isPathParam (PathId _) = True
          isPathParam _ = False

        

handlerName :: Route -> Handler -> String
handlerName r h = routeName (routePath r) ++ " " ++ show (handlerType h)

routeName :: [PathPiece] -> String
routeName ps = "/" ++ intercalate "/" (map show ps)

data PathPiece = PathText String
               | PathId EntityName
               deriving (Eq, Data, Typeable)
instance Show PathPiece where
    show (PathText s) = s
    show (PathId en) = "#" ++ en ++ "Id"
    
data FieldRef = FieldRefId VariableName
              | FieldRefNormal VariableName FieldName
              | FieldRefAuthId
              | FieldRefAuth FieldName
              | FieldRefLocalParam
              | FieldRefEnum EnumName FieldName
              | FieldRefPathParam Int 
              | FieldRefRequest FieldName
              | FieldRefNamedLocalParam VariableName
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
data FieldContent = NormalField FieldType [FieldOption]
                    | EntityField EntityName 
                    | EnumField EnumName
                deriving (Show,Eq, Data, Typeable)
   

data Field = Field {
    fieldOptional :: Bool,
    fieldInternal :: Bool,
    fieldName     :: FieldName,
    fieldContent  :: FieldContent
} deriving (Show,Eq, Data, Typeable)

type FunctionName = String

data FieldOption = FieldCheck FunctionName
                 | FieldDefault FieldValue
                 deriving (Show, Eq, Data, Typeable)

data FieldValue = StringValue String
                | IntValue Int
                | FloatValue Double
                | BoolValue Bool
                | NothingValue
                deriving (Show, Eq, Ord, Data, Typeable)
fieldValueToSql :: FieldValue -> String    
fieldValueToSql fv = case fv of
    (StringValue s) -> "'" ++ s ++ "'"
    (IntValue i) -> show i
    (FloatValue d) -> show d
    (BoolValue b) -> show b
    NothingValue -> "NULL"
   
fieldValueToEsqueleto :: FieldValue -> String    
fieldValueToEsqueleto fv = case fv of
    (StringValue s) -> "\"" ++ s ++ "\""
    (IntValue i) -> show i
    (FloatValue d) -> show d
    (BoolValue b) -> show b
    NothingValue -> "nothing"

fieldValueToHs :: FieldValue -> String
fieldValueToHs fv = case fv of
    StringValue s -> "\"" ++ s ++ "\""
    IntValue i -> show i
    FloatValue d -> show d
    BoolValue b -> show b
    NothingValue -> "Nothing"
fieldOptions :: Field -> [FieldOption]
fieldOptions f = fieldContentOptions (fieldContent f)
    where fieldContentOptions (NormalField  _ options) = options
          fieldContentOptions _ = []
    
fieldDefault :: Field -> Maybe FieldValue
fieldDefault f = case find isDefault (fieldOptions f) of
    Just (FieldDefault fv) -> Just fv
    _ -> Nothing
    where isDefault (FieldDefault _) = True
          isDefault _  = False

fieldChecks :: Field -> [FunctionName]
fieldChecks f = map (\(FieldCheck func) -> func) $ filter isCheck (fieldOptions f)
    where isCheck (FieldCheck _) = True
          isCheck _ = False

lookupEntity :: Module -> EntityName -> Maybe Entity
lookupEntity m en = listToMaybe [ e | e <- modEntities m, entityName e == en ]
         
lookupField :: Module -> EntityName -> FieldName -> Maybe Field
lookupField m en fn = listToMaybe [ f | e <- modEntities m,
                                    f <- entityFields e,
                                    entityName e == en,
                                    fieldName f == fn ] 

lowerFirst :: String -> String
lowerFirst (a:b) = (toLower a):b
lowerFirst a = a

upperFirst :: String -> String
upperFirst (a:b) = (toUpper a):b
upperFirst a = a
