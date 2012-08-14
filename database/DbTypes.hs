module DbTypes where
import DbLexer
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

data Index = Index UniqueFlag [FieldName] 
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

type DefaultValue = String

data Field = Field OptionalFlag FieldType FieldName [FieldOption]
                 | RefField OptionalFlag EntityName FieldName (Maybe BackRefField)
                 deriving (Show)

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



