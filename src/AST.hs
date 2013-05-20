module AST where
import Lexer
import Data.Maybe
import Data.List
import Data.Char
type ImportPath = FilePath

data Module = Module {
    modName :: Maybe String,
    modImports   :: [ImportPath],
    modEntities  :: [Entity],
    modClasses :: [Class],
    modEnums :: [EnumType],
    modResources :: [Resource]
}
    deriving (Show)
moduleName :: Module -> String
moduleName = fromJust . modName

emptyModule = Module {
    modName = Nothing,
    modImports = [],
    modEntities = [],
    modClasses = [],
    modEnums = [],
    modResources = []
}
data ModDef = EntityDef Entity
           | ClassDef Class
           | EnumDef EnumType
           | ResourceDef Resource
           deriving (Show)


 
isClass (ClassDef _) = True
isClass _ = False
isEntity (EntityDef _) = True
isEntity _ = False
isEnum (EnumDef _) = True
isEnum _ = False

isResource (ResourceDef _) = True
isResource _ = False

getEntities :: [ModDef] -> [Entity]
getEntities defs = map (\(EntityDef e) -> e) $ filter isEntity defs
getClasses :: [ModDef] -> [Class]
getClasses defs = map (\(ClassDef e) -> e) $ filter isClass defs

getEnums :: [ModDef] -> [EnumType]
getEnums defs = map (\(EnumDef e) -> e) $ filter isEnum defs

getResources :: [ModDef] -> [Resource]
getResources defs = map (\(ResourceDef e) -> e) $ filter isResource defs
   


type ClassName = String

type UniqueFlag = Bool
type ParamName = String
type EntityName = String
data FieldType = FTWord32 | FTWord64 | FTInt32 | FTInt64 | FTText 
               | FTBool | FTDouble | FTTime | FTDate | FTDateTime 
                              | FTZonedTime
instance Show FieldType where
    show FTWord32 = "Word32"
    show FTWord64 = "Word64"
    show FTInt32 = "Int32"
    show FTInt64 = "Int64"
    show FTText = "Text"
    show FTBool = "Bool"
    show FTDouble = "Double"
    show FTTime = "TimeOfDay"
    show FTDate = "Day"
    show FTDateTime = "UTCTime"
    show FTZonedTime = "ZonedTime"

type FieldName = String 
type PathName = String
type OptionalFlag = Bool
type UniqueName = String

data Location = Loc FilePath Int Int 

instance Show Location where
    show (Loc path row col) = path ++ " line " ++ show row ++ " col " ++ show col
mkLoc t = Loc "" (tokenLineNum t) (tokenColNum t)

data Unique = Unique {
    uniqueName :: UniqueName,
    uniqueFields :: [FieldName]
} deriving (Show, Eq)

data HandlerType = GetHandler 
                 | PutHandler 
                 | PostHandler 
                 | DeleteHandler 
                 deriving (Eq) 
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
              deriving (Show, Eq)

data BinOp = Eq | Ne | Lt | Gt | Le | Ge | Like deriving (Show,Eq)     
data Expr = AndExpr Expr Expr
          | OrExpr Expr Expr
          | BinOpExpr ValExpr BinOp ValExpr deriving (Show, Eq)
data ValExpr = FieldExpr FieldRef
           | ConstExpr FieldValue deriving (Show, Eq)
data HandlerParam = Public 
                  | DefaultFilterSort
                  | TextSearchFilter TextSearchParams
                  | Select SelectQuery 
                  | IfFilter IfFilterParams
                  | DeleteFrom EntityName VariableName (Maybe Expr)
                  | Replace EntityName InputFieldRef (Maybe [InputField])
                  | Insert EntityName (Maybe [InputField])
                  deriving (Show, Eq) 
type TextSearchParams = (ParamName,[FieldRef])
type IfFilterParams = (ParamName,[Join],Expr)

data SelectQuery = SelectQuery {
    sqFields       :: [SelectField],
    sqFrom         :: (EntityName, VariableName),
    sqJoins        :: [Join],
    sqWhere        :: Maybe Expr,
    sqOrderBy        :: [(FieldRef, SortDir)],
    sqLimitOffset  :: (Int, Int)
} deriving (Show, Eq)    

sqAliases :: SelectQuery -> [(EntityName, VariableName)]
sqAliases sq = sqFrom sq : [ (joinEntity j, joinAlias j) | j <- sqJoins sq]

data SelectField = SelectAllFields EntityName
                 | SelectField EntityName FieldName (Maybe VariableName)
                 | SelectIdField EntityName (Maybe VariableName)
                 deriving (Show, Eq)

    
data Join = Join {
    joinType   :: JoinType,
    joinEntity :: EntityName,
    joinAlias  :: VariableName,
    joinExpr   :: Maybe (FieldRef, BinOp, FieldRef)
} deriving (Show, Eq)

type InputField = (ParamName, InputFieldRef)

data InputFieldRef = InputFieldNormal FieldName
                   | InputFieldAuthId
                   | InputFieldPathParam Int
                   | InputFieldConst FieldValue
                    deriving (Show, Eq)
                    
data SortDir = SortAsc | SortDesc deriving (Show, Eq)                   

data Handler = Handler {
    handlerType   :: HandlerType,
    handlerParams :: [HandlerParam] 
} deriving (Show, Eq)
data Entity = Entity {
    entityLoc        :: Location,
    entityName       :: String,
    entityInstances :: [ClassName],
    entityFields     :: [Field],
    entityUniques    :: [Unique],
    entityDeriving   :: [ClassName],
    entityChecks     :: [FunctionName]
} deriving (Show)


data Resource = Resource {
    resLoc :: Location,
    resRoute :: [PathPiece],
    resHandlers :: [Handler]
} deriving (Show)
resPathParams :: Resource -> [PathPiece]
resPathParams = (filter isPathParam) . resRoute
    where isPathParam (PathId _) = True
          isPathParam _ = False

        

handlerName :: Resource -> HandlerType -> String
handlerName r ht = show (resRoute r) ++ " " ++ show ht

routeName :: [PathPiece] -> String
routeName ps = "/" ++ intercalate "/" (map show ps)

handlerSelectFrom :: [HandlerParam] -> Maybe SelectQuery
handlerSelectFrom ps = case find isSelect ps of
    Just (Select sq) -> Just sq
    Nothing -> Nothing
    where isSelect (Select _) = True
          isSelect _ = False
data PathPiece = PathText String
               | PathId EntityName
instance Show PathPiece where
    show (PathText s) = s
    show (PathId en) = "#" ++ en ++ "Id"
    
data FieldRef = FieldRefId VariableName
              | FieldRefNormal VariableName FieldName
              | FieldRefAuthId
              | FieldRefLocalParam
              | FieldRefPathParam Int deriving (Show, Eq) 

entityFieldByName :: Entity -> FieldName -> Field
entityFieldByName e fn = maybe (error $ "No field " ++ fn ++ " in " ++ entityName e) id
                               (find (\f -> fieldName f == fn) (entityFields e))

data EnumType = EnumType {
    enumLoc :: Location,
    enumName :: String,
    enumValues :: [String]
} deriving (Show)

data Class = Class {
    classLoc     :: Location,
    className    :: String,
    classFields  :: [Field],
    classUniques :: [Unique]
} deriving (Show)



modLookup :: Module -> String -> ModDef
modLookup mod name 
        | isJust entityMatch = EntityDef $ fromJust entityMatch
        | isJust classMatch  = ClassDef $ fromJust classMatch
        | otherwise = error $ "modLookup failed : " ++ name
    where
        entityMatch = find (\e -> name == entityName e) (modEntities mod)
        classMatch  = find (\i -> name == className i) (modClasses mod)

moddefFields :: ModDef -> [Field]
moddefFields moddef = case moddef of
    (EntityDef entity)     -> entityFields entity
    (ClassDef classDef) -> classFields classDef
    
type DefaultValue = String
type IsListFlag = Bool
data FieldContent = NormalField FieldType [FieldOption]
                    | EntityField EntityName 
                deriving (Show)
   

data Field = Field {
    fieldOptional :: OptionalFlag,
    fieldName     :: FieldName,
    fieldContent  :: FieldContent
} deriving (Show)

baseFieldType :: Field -> String
baseFieldType f = case fieldContent f of
    (NormalField ft _) -> show ft
    (EntityField en) -> en ++ "Id"

boolToMaybe :: Bool -> String
boolToMaybe True = "Maybe "
boolToMaybe False = ""

hsFieldType :: Field -> String
hsFieldType f = (boolToMaybe . fieldOptional) f
              ++ baseFieldType f


type FunctionName = String

data FieldOption = FieldCheck FunctionName
                 | FieldDefault FieldValue
                 deriving (Show, Eq)

data FieldValue = StringValue String
                | IntValue Int
                | FloatValue Double
                deriving (Eq)
instance Show FieldValue where
    show (StringValue s) = "\"" ++ s ++ "\""
    show (IntValue i) = show i
    show (FloatValue d) = show d

fieldOptions :: Field -> [FieldOption]
fieldOptions f = fieldContentOptions (fieldContent f)
    where fieldContentOptions (NormalField  _ options) = options
          fieldContentOptions _ = []
    
fieldDefault :: Field -> Maybe FieldValue
fieldDefault f = case find isDefault (fieldOptions f) of
    Just (FieldDefault fv) -> Just fv
    Nothing -> Nothing
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
