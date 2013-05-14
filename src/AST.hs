module AST where
import Lexer
import Data.Maybe
import Data.List
import Data.Char
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
                  | TextSearchFilter ParamName [FieldRef]
                  | SelectFrom EntityName VariableName
                  | DeleteFrom EntityName VariableName Expr
                  | Replace EntityName FieldRef InputObject
                  | Insert EntityName InputObject
                  | ReadJson VariableName
                  | Join JoinType EntityName VariableName
                         (Maybe (FieldRef, BinOp, FieldRef))
                  | Where Expr
                  | OrderBy [(FieldRef,SortDir)]
                  | ReturnEntity VariableName
                  | ReturnFields [(ParamName, FieldRef)]
                  deriving (Show, Eq) 
data InputObject = InputJsonVariable VariableName 
                 | InputJsonFields [(ParamName, InputFieldRef)] 
                  deriving (Show, Eq)

data InputFieldRef = InputFieldNormal VariableName FieldName
                   | InputFieldAuthId
                   | InputFieldPathParam Int
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
    entityChecks     :: [Check]
} deriving (Show)

data Check = Check FunctionName [FieldName] deriving (Show)

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

handlerSelectFrom :: [HandlerParam] -> Maybe (EntityName, VariableName)
handlerSelectFrom ps = case find isSelectFrom ps of
    Just (SelectFrom en vn) -> Just (en, vn)
    Nothing -> Nothing
    where isSelectFrom (SelectFrom _ _) = True
          isSelectFrom _ = False

handlerJoins :: [HandlerParam] -> [(JoinType, EntityName, VariableName,
                                   (Maybe (FieldRef, BinOp, FieldRef)))]
handlerJoins = (map (\(Join jt en vn je) -> (jt,en,vn,je))) . (filter isJoin)
    where isJoin (Join _ _ _ _) = True
          isJoin _ = False

handlerEntities :: [HandlerParam] -> [(EntityName, VariableName)]
handlerEntities = mapMaybe match
    where match (SelectFrom en vn) = Just (en, vn)
          match (Join _ en vn _) = Just (en ,vn)
          match _ = Nothing

handlerFields :: Module -> [HandlerParam] -> [(Entity, VariableName, Field)]
handlerFields m ps = [ (e,vn,f) | e <- modEntities m,
                                  (en,vn) <- handlerEntities ps,
                                  entityName e == en,
                                  f <- entityFields e ]


handlerVariableEntity :: [HandlerParam] -> VariableName -> Maybe EntityName
handlerVariableEntity ps vn = case filter match ps of
    ((SelectFrom en _):_) -> Just en
    ((Join _ en _ _):_) -> Just en
    _ -> Nothing
   where match (SelectFrom _ vn') = vn == vn'
         match (Join _ _ vn' _) = vn == vn'

data PathPiece = PathText String
               | PathId EntityName
instance Show PathPiece where
    show (PathText s) = s
    show (PathId en) = "#" ++ en ++ "Id"
    
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
    classUniques :: [Unique],
    classChecks  :: [Check]
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

data FieldOption = FieldDefault FieldValue
                 deriving (Show, Eq)

data FieldValue = StringValue String
                | IntValue Int
                | FloatValue Double
                deriving (Show, Eq)

fieldOptions :: Field -> [FieldOption]
fieldOptions f = fieldContentOptions (fieldContent f)
    where fieldContentOptions (NormalField  _ options) = options
          fieldContentOptions _ = []
    
fieldDefault :: Field -> Maybe FieldValue
fieldDefault f = case find isDefault (fieldOptions f) of
    Just (FieldDefault fv) -> Just fv
    Nothing -> Nothing
    where isDefault (FieldDefault _) = True

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
