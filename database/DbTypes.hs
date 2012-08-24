module DbTypes where
import DbLexer
import Data.Maybe
import Data.List
type ImportPath = FilePath

data DbModule = DbModule {
    dbImports   :: [ImportPath],
    dbDocs  :: [Doc],
    dbIfaces :: [Iface]
}
    deriving (Show)
emptyDbModule = DbModule {
    dbImports = [],
    dbDocs = [],
    dbIfaces = []
}
data DbDef = DocDef Doc
           | IfaceDef Iface
           deriving (Show)


 
isIface (IfaceDef _) = True
isIface _ = False
isDoc (DocDef _) = True
isDoc _ = False

getDocs :: [DbDef] -> [Doc]
getDocs defs = map (\(DocDef e) -> e) $ filter isDoc defs
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
        | otherwise = error $ "dbLookup failed : " ++ name
    where
        docMatch = find (\e -> name == docName e) (dbDocs db)
        ifaceMatch  = find (\i -> name == ifaceName i) (dbIfaces db)

dbdefFields :: DbDef -> [Field]
dbdefFields dbdef = case dbdef of
    (DocDef doc)     -> docFields doc
    (IfaceDef iface) -> ifaceFields iface
    
type DefaultValue = String
type IsListFlag = Bool
data DocFieldType = SingleField | ListField deriving (Show,Eq)
data DocFieldEmbedding = EmbedField | RefField deriving (Show,Eq)
data FieldContent = NormalField FieldType [FieldOption]
                    | DocField DocFieldEmbedding DocFieldType DocName 
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



