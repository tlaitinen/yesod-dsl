module AST where
import Lexer
import Data.Maybe
import Data.List
type ImportPath = FilePath

data Module = Module {
    modImports   :: [ImportPath],
    modEntities  :: [Entity],
    modClasses :: [Class],
    modEnums :: [EnumType],
    modResources :: [Resource]
}
    deriving (Show)
emptyModule = Module {
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
                              | FTZonedTime deriving (Show)
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
                 deriving (Show, Eq) 
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
          | BinOpExpr ValExpr ValExpr deriving (Show,Eq)
data ValExpr = FieldExpr FieldRef
           | ConstExpr FieldValue deriving (Show,Eq)
data HandlerParam = Public 
                  | HandlerEntity EntityName
                  | DefaultFilterSort
                  | TextSearchFilter ParamName [FieldRef]
                  | SelectFrom EntityName VariableName
                  | Join JoinType EntityName VariableName
                         (Maybe (FieldRef, BinOp, FieldRef))
                  | Where Expr
                  | MapBy FunctionName
                  | OrderBy [(FieldRef,SortDir)]
                  | ReturnEntity VariableName
                  | ReturnFields [(ParamName, FieldRef)]
                  | BeforeHandler FunctionName 
                  | AfterHandler FunctionName  deriving (Show, Eq) 
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

handlerName :: Resource -> HandlerType -> String
handlerName r ht = show (resRoute r) ++ " " ++ show ht


data PathPiece = PathText String
               | PathId EntityName deriving (Show)


data FieldRef = FieldRefId VariableName
              | FieldRefNormal VariableName FieldName
              | FieldRefAuthId
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
type FunctionName = String

data FieldOption = FieldCheck FunctionName
                 | FieldDefault FieldValue
                 deriving (Show)

data FieldValue = StringValue String
                | IntValue Int
                | FloatValue Double
                deriving (Show,Eq)

fieldOptions :: Field -> [FieldOption]
fieldOptions f = fieldContentOptions (fieldContent f)
    where fieldContentOptions (NormalField  _ options) = options
          fieldContentOptions _ = []
    

