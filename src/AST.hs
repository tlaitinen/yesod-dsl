module AST where
import Lexer
import Data.Maybe
import Data.List
type ImportPath = FilePath

data Module = Module {
    dbImports   :: [ImportPath],
    dbEntities  :: [Entity],
    dbClasses :: [Class],
    dbEnums :: [DbEnum],
    dbResources :: [Resource]
}
    deriving (Show)
emptyModule = Module {
    dbImports = [],
    dbEntities = [],
    dbClasses = [],
    dbEnums = [],
    dbResources = []
}
data DbDef = EntityDef Entity
           | ClassDef Class
           | EnumDef DbEnum
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

getEntities :: [DbDef] -> [Entity]
getEntities defs = map (\(EntityDef e) -> e) $ filter isEntity defs
getClasses :: [DbDef] -> [Class]
getClasses defs = map (\(ClassDef e) -> e) $ filter isClass defs

getEnums :: [DbDef] -> [DbEnum]
getEnums defs = map (\(EnumDef e) -> e) $ filter isEnum defs

getResources :: [DbDef] -> [Resource]
getResources defs = map (\(ResourceDef e) -> e) $ filter isResource defs
   


type ClassName = String

type UniqueFlag = Bool
type ParamName = String
type EntityName = String
type FieldType = TokenType
type FieldName = String
type PathName = String
type OptionalFlag = Bool
type UniqueName = String

data Location = Loc FilePath Int Int 

instance Show Location where
    show (Loc path row col) = path ++ " line " ++ show row ++ " col " ++ show col
mkLoc t = Loc "" (tokenLineNum t) (tokenColNum t)

data Unique = Unique UniqueName [FieldName]
           deriving (Show)

data HandlerType = GetHandler 
                 | PutHandler 
                 | PostHandler 
                 | DeleteHandler 
                 | ValidateHandler  deriving (Show, Eq) 
type QueryAlias = String
data JoinType = InnerJoin 
              | CrossJoin
              | LeftOuterJoin
              | RightOuterJoin
              | FullOuterJoin
              deriving (Show, Eq)

data BinOp = Eq | Ne | Lt | Gt | Le | Ge | Like deriving (Show,Eq)     
data Expr = AndExpr Expr Expr
          | OrExpr Expr Expr
          | BinOpExpr ValExpr ValExpr deriving (Show,Eq)
data ValExpr = FieldExpr FieldPath
           | ConstExpr FieldValue deriving (Show,Eq)
data HandlerParam = Public 
                  | DefaultFilterSort
                  | TextSearchFilter QueryAlias [FieldPath]
                  | SelectFrom EntityName QueryAlias
                  | Join JoinType EntityName QueryAlias 
                         (Maybe (FieldPath, BinOp, FieldPath))
                  | Where Expr
                  | PreTransform FunctionName
                  | PostTransform FunctionName
                  | SortBy [(FieldPath,SortDir)]
                  | PreHook FunctionName 
                  | PostHook FunctionName  deriving (Show, Eq) 
data SortDir = SortAsc | SortDesc deriving (Show, Eq)                   

data Handler = Handler HandlerType [HandlerParam]  deriving (Show)

data Entity = Entity {
    entityLoc        :: Location,
    entityName       :: String,
    entityImplements :: [ClassName],
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

data PathPiece = PathText String
               | PathId EntityName deriving (Show)


data FieldPath = FieldPathId EntityName 
               | FieldPathNormal EntityName FieldName deriving (Show, Eq)

entityFieldByName :: Entity -> FieldName -> Field
entityFieldByName e fn = maybe (error $ "No field " ++ fn ++ " in " ++ entityName e) id
                               (find (\f -> fieldName f == fn) (entityFields e))

data DbEnum = DbEnum {
    enumLoc :: Location,
    enumName :: String,
    enumValues :: [String]
} deriving (Show)

entityPath :: Entity -> String
entityPath e = entityName e ++ " in " ++ show (entityLoc e)


data Class = Class {
    classLoc     :: Location,
    className    :: String,
    classFields  :: [Field],
    classUniques :: [Unique]
} deriving (Show)

dbLookup :: Module -> String -> DbDef
dbLookup db name 
        | isJust entityMatch = EntityDef $ fromJust entityMatch
        | isJust classMatch  = ClassDef $ fromJust classMatch
        | otherwise = error $ "dbLookup failed : " ++ name
    where
        entityMatch = find (\e -> name == entityName e) (dbEntities db)
        classMatch  = find (\i -> name == className i) (dbClasses db)

dbdefFields :: DbDef -> [Field]
dbdefFields dbdef = case dbdef of
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
type FunctionName = String

data FieldOption = FieldCheck FunctionName
                 | FieldDefault String
                 deriving (Show)

data FieldValue = StringValue String
                | IntValue Int
                | FloatValue Double
                deriving (Show,Eq)

fieldOptions :: Field -> [FieldOption]
fieldOptions f = fieldContentOptions (fieldContent f)
    where fieldContentOptions (NormalField  _ options) = options
          fieldContentOptions _ = []
    
fieldDefault :: Field -> Maybe String
fieldDefault f = maybe Nothing (\(FieldDefault d) -> Just d) 
                      (find isFieldDefault (fieldOptions f))
    where
        isFieldDefault (FieldDefault _) = True
        isFieldDefault _ = False


