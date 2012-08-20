module DbTypes where
import DbLexer
import Data.Maybe
import Data.List
type ImportPath = FilePath

data DbModule = DbModule {
    dbImports   :: [ImportPath],
    dbEntities  :: [Entity],
    dbRelations :: [Relation],
    dbIfaces :: [Iface]
}
    deriving (Show)
emptyDbModule = DbModule {
    dbImports = [],
    dbEntities = [],
    dbRelations = [],
    dbIfaces = []

}
data DbDef = EntityDef Entity
           | IfaceDef Iface
           | RelDef Relation
           deriving (Show)


 
isIface (IfaceDef _) = True
isIface _ = False
isEntity (EntityDef _) = True
isEntity _ = False
isRelation (RelDef _) = True
isRelation _ = False

getEntities :: [DbDef] -> [Entity]
getEntities defs = map (\(EntityDef e) -> e) $ filter isEntity defs
getRelations :: [DbDef] -> [Relation]
getRelations defs = map (\(RelDef e) -> e) $ filter isRelation defs
getIfaces :: [DbDef] -> [Iface]
getIfaces defs = map (\(IfaceDef e) -> e) $ filter isIface defs

   


type IfaceName = String

type UniqueFlag = Bool

type EntityName = String
type FieldType = TokenType
type FieldName = String
type OptionalFlag = Bool

data Location = Loc FilePath Int Int 

instance Show Location where
    show (Loc path row col) = path ++ " line " ++ show row ++ " col " ++ show col
mkLoc t = Loc "" (tokenLineNum t) (tokenColNum t)

data IndexDir = AscIndex | DescIndex deriving (Show)
data Index = Index UniqueFlag IndexDir [FieldName] 
           deriving (Show)
data Entity = Entity {
    entityLoc        :: Location,
    entityName       :: String,
    entityImplements :: [IfaceName],
    entityFields     :: [Field],
    entityIndices    :: [Index]
} deriving (Show)


entityPath :: Entity -> String
entityPath e = entityName e ++ " in " ++ show (entityLoc e)


data Iface = Iface {
    ifaceLoc     :: Location,
    ifaceName    :: String,
    ifaceFields  :: [Field],
    ifaceIndices :: [Index]
} deriving (Show)

data Relation = Relation {
    relLoc     :: Location,
    relName    :: String,
    relFields  :: [Field],
    relIndices :: [Index]
} deriving (Show)

dbLookup :: DbModule -> String -> DbDef
dbLookup db name 
        | isJust entityMatch = EntityDef $ fromJust entityMatch
        | isJust ifaceMatch  = IfaceDef $ fromJust ifaceMatch
        | isJust relMatch    = RelDef $ fromJust relMatch
        | otherwise = error $ "dbLookup failed : " ++ name
    where
        entityMatch = find (\e -> name == entityName e) (dbEntities db)
        ifaceMatch  = find (\i -> name == ifaceName i) (dbIfaces db)
        relMatch    = find (\r -> name == relName r) (dbRelations db)
 
type DefaultValue = String
type IsListFlag = Bool
data EmbedFieldType = EmbedSingle | EmbedList deriving (Show)
data FieldContent = NormalField FieldType [FieldOption]
                    | EmbedField EmbedFieldType EntityName 
                    | RelField EntityName (Maybe BackRefField)
                deriving (Show)
   

data Field = Field {
    fieldOptional :: OptionalFlag,
    fieldName     :: FieldName,
    fieldContent  :: FieldContent
} deriving (Show)
type FunctionName = String

data FieldOption = FieldDefaultValue FieldValue
                 | FieldUnique
                 | FieldCheck FunctionName
                 deriving (Show)

data FieldValue = StringValue String
                | IntValue Int
                | FloatValue Double
                deriving (Show)
data Multiplicity = One | Many
                  deriving (Show)
data BackRefField = BackRefField Multiplicity FieldName
                  deriving (Show)



