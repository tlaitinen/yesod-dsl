module DbTypes where
import DbLexer
import Data.Maybe
import Data.List
type ImportPath = FilePath

data DbModule = DbModule {
    dbImports   :: [ImportPath],
    dbEntities  :: [Entity],
    dbIfaces :: [Iface]
}
    deriving (Show)
emptyDbModule = DbModule {
    dbImports = [],
    dbEntities = [],
    dbIfaces = []
}
data DbDef = EntityDef Entity
           | IfaceDef Iface
           deriving (Show)


 
isIface (IfaceDef _) = True
isIface _ = False
isEntity (EntityDef _) = True
isEntity _ = False

getEntities :: [DbDef] -> [Entity]
getEntities defs = map (\(EntityDef e) -> e) $ filter isEntity defs
getIfaces :: [DbDef] -> [Iface]
getIfaces defs = map (\(IfaceDef e) -> e) $ filter isIface defs

   


type IfaceName = String

type UniqueFlag = Bool

type EntityName = String
type FieldType = TokenType
type FieldName = String
type OptionalFlag = Bool
type UniqueName = String

data Location = Loc FilePath Int Int 

instance Show Location where
    show (Loc path row col) = path ++ " line " ++ show row ++ " col " ++ show col
mkLoc t = Loc "" (tokenLineNum t) (tokenColNum t)

data Unique = Unique UniqueName [FieldName]
           deriving (Show)

data ServiceType = GetService 
                 | PutService 
                 | PostService 
                 | DeleteService 
                 | ValidateService  deriving (Show, Eq) 
data ServiceParam = PublicService 
                  | ServiceDefaultFilterSort
                  | ServiceFilter FunctionName
                  | ServiceSort FunctionName
                  | ServiceCond FunctionName 
                  | ServicePostHook FunctionName  deriving (Show, Eq) 
                   
data Service = Service ServiceType [ServiceParam]  deriving (Show)

data Entity = Entity {
    entityLoc        :: Location,
    entityName       :: String,
    entityImplements :: [IfaceName],
    entityFields     :: [Field],
    entityUniques    :: [Unique],
    entityChecks     :: [FunctionName],
    entityServices   :: [Service]
} deriving (Show)
            

entityPath :: Entity -> String
entityPath e = entityName e ++ " in " ++ show (entityLoc e)


data Iface = Iface {
    ifaceLoc     :: Location,
    ifaceName    :: String,
    ifaceFields  :: [Field]
} deriving (Show)

dbLookup :: DbModule -> String -> DbDef
dbLookup db name 
        | isJust entityMatch = EntityDef $ fromJust entityMatch
        | isJust ifaceMatch  = IfaceDef $ fromJust ifaceMatch
        | otherwise = error $ "dbLookup failed : " ++ name
    where
        entityMatch = find (\e -> name == entityName e) (dbEntities db)
        ifaceMatch  = find (\i -> name == ifaceName i) (dbIfaces db)

dbdefFields :: DbDef -> [Field]
dbdefFields dbdef = case dbdef of
    (EntityDef entity)     -> entityFields entity
    (IfaceDef iface) -> ifaceFields iface
    
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
                 deriving (Show)

data FieldValue = StringValue String
                | IntValue Int
                | FloatValue Double
                deriving (Show)



