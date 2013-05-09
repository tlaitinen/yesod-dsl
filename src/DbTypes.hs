module DbTypes where
import DbLexer
import Data.Maybe
import Data.List
type ImportPath = FilePath

data DbModule = DbModule {
    dbImports   :: [ImportPath],
    dbEntities  :: [Entity],
    dbClasses :: [Class],
    dbEnums :: [DbEnum]
}
    deriving (Show)
emptyDbModule = DbModule {
    dbImports = [],
    dbEntities = [],
    dbClasses = [],
    dbEnums = []
}
data DbDef = EntityDef Entity
           | ClassDef Class
           | EnumDef DbEnum
           deriving (Show)


 
isClass (ClassDef _) = True
isClass _ = False
isEntity (EntityDef _) = True
isEntity _ = False
isEnum (EnumDef _) = True
isEnum _ = False

getEntities :: [DbDef] -> [Entity]
getEntities defs = map (\(EntityDef e) -> e) $ filter isEntity defs
getClasses :: [DbDef] -> [Class]
getClasses defs = map (\(ClassDef e) -> e) $ filter isClass defs

getEnums :: [DbDef] -> [DbEnum]
getEnums defs = map (\(EnumDef e) -> e) $ filter isEnum defs

   


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

data ServiceType = GetService 
                 | GetServiceNested PathName [Join]
                 | PutService 
                 | PostService 
                 | DeleteService 
                 | ValidateService  deriving (Show, Eq) 
data ServiceParam = PublicService 
                  | ServiceDefaultFilterSort
                  | ServiceTextSearchFilter ParamName [FieldName]
                  | ServiceFilter FunctionName
                  | ServiceSelectOpts FunctionName
                  | ServicePreTransform FunctionName
                  | ServicePostTransform FunctionName
                  | ServiceSortBy [(FieldName,SortDir)]
                  | ServicePreHook FunctionName 
                  | ServicePostHook FunctionName  deriving (Show, Eq) 
data SortDir = SortAsc | SortDesc deriving (Show, Eq)                   

data Service = Service ServiceType [ServiceParam]  deriving (Show)

data Entity = Entity {
    entityLoc        :: Location,
    entityName       :: String,
    entityImplements :: [ClassName],
    entityFields     :: [Field],
    entityUniques    :: [Unique],
    entityDeriving   :: [ClassName],
    entityChecks     :: [FunctionName],
    entityServices   :: [Service]
} deriving (Show)

data Join = Join EntityName FieldPath FieldPath deriving (Show, Eq)

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

dbLookup :: DbModule -> String -> DbDef
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
                deriving (Show)

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


