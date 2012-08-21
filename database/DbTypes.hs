module DbTypes where
import DbLexer
import Data.Maybe
import Data.List
type ImportPath = FilePath

data DbModule = DbModule {
    dbImports   :: [ImportPath],
    dbDocs  :: [Doc],
    dbRecs   :: [Record],
    dbIfaces :: [Iface]
}
    deriving (Show)
emptyDbModule = DbModule {
    dbImports = [],
    dbDocs = [],
    dbRecs = [],
    dbIfaces = []
}
data DbDef = DocDef Doc
           | RecDef Record
           | IfaceDef Iface
           deriving (Show)


 
isIface (IfaceDef _) = True
isIface _ = False
isRecord (RecDef _) = True
isRecord _ = False
isDoc (DocDef _) = True
isDoc _ = False

getDocs :: [DbDef] -> [Doc]
getDocs defs = map (\(DocDef e) -> e) $ filter isDoc defs
getRecords :: [DbDef] -> [Record]
getRecords defs = map (\(RecDef e) -> e) $ filter isRecord defs
getIfaces :: [DbDef] -> [Iface]
getIfaces defs = map (\(IfaceDef e) -> e) $ filter isIface defs

   


type IfaceName = String

type UniqueFlag = Bool

type DocName = String
type FieldType = TokenType
type FieldName = String
type OptionalFlag = Bool

data Location = Loc FilePath Int Int 

instance Show Location where
    show (Loc path row col) = path ++ " line " ++ show row ++ " col " ++ show col
mkLoc t = Loc "" (tokenLineNum t) (tokenColNum t)

data IndexDir = AscIndex | DescIndex deriving (Show)
type IndexId = [FieldName]
data Index = Index UniqueFlag IndexDir [IndexId] 
           deriving (Show)
data Doc = Doc {
    docLoc        :: Location,
    docName       :: String,
    docImplements :: [IfaceName],
    docFields     :: [Field],
    docIndices    :: [Index]
} deriving (Show)

data Record = Record {
    recLoc  :: Location,
    recName :: String,
    recFields :: [Field]
} deriving (Show)


docPath :: Doc -> String
docPath e = docName e ++ " in " ++ show (docLoc e)


data Iface = Iface {
    ifaceLoc     :: Location,
    ifaceName    :: String,
    ifaceFields  :: [Field],
    ifaceIndices :: [Index]
} deriving (Show)

dbLookup :: DbModule -> String -> DbDef
dbLookup db name 
        | isJust docMatch = DocDef $ fromJust docMatch
        | isJust ifaceMatch  = IfaceDef $ fromJust ifaceMatch
        | isJust recMatch = RecDef $ fromJust recMatch
        | otherwise = error $ "dbLookup failed : " ++ name
    where
        docMatch = find (\e -> name == docName e) (dbDocs db)
        ifaceMatch  = find (\i -> name == ifaceName i) (dbIfaces db)
        recMatch = find (\r -> name == recName r) (dbRecs db)
 
type DefaultValue = String
type IsListFlag = Bool
data EmbedFieldType = EmbedSingle | EmbedList deriving (Show)
data FieldContent = NormalField FieldType [FieldOption]
                    | EmbedField EmbedFieldType DocName 
                    | RelField DocName
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



