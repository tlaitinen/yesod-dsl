module Generator (generateModels) where
import System.IO (FilePath)
import DbTypes
import DbLexer
import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils
-- from Database.Persist.TH
recName :: String -> String -> String
recName dt f = lowerFirst dt ++ upperFirst f

lowerFirst :: String -> String
lowerFirst (a:b) = (toLower a):b
lowerFirst a = a

upperFirst :: String -> String
upperFirst (a:b) = (toUpper a):b
upperFirst a = a
-- ^^^^ Database.Persist.TH        
entityFieldDeps :: DbModule -> String -> [String]
entityFieldDeps db name 
    | name `elem` [ entityName entity | entity <- dbEntities db ] = [name]
    | otherwise = [name ++ "Inst", name ++ "InstRef"]

getFieldDeps :: DbModule -> Field -> [String]
getFieldDeps db field = case (fieldContent field) of
    (NormalField _ _) -> []
    (EntityField entityName) -> entityFieldDeps db entityName

lookupDeps :: DbModule -> String -> [String]
lookupDeps db name = concatMap (getFieldDeps db) $ (dbdefFields . (dbLookup db)) name

genUnique :: Unique -> String
genUnique (Unique name fields) = "Unique" ++ name ++ " " ++ intercalate " " fields

genFieldType :: DbModule -> Field -> String
genFieldType db field = case (fieldContent field) of
    (NormalField ftype _)   -> fromTkType ftype
    (EntityField entityName) -> entityName ++ "Id"
    where 
        fromTkType TWord32 = "Word32"
        fromTkType TWord64 = "Word64"
        fromTkType TInt32  = "Int32"
        fromTkType TInt64  = "Int64"
        fromTkType TText   = "Text"
        fromTkType TBool   = "Bool"
        fromTkType TDouble = "Double"
        fromTkType TTime   = "TimeOfDay"
        fromTkType TDate   = "Day"
        fromTkType TDateTime = "UTCTime"
        fromTkType TZonedTime = "ZonedTime"
        fromTkType ft = error $ "Unknown field type: " ++ show ft 

persistFieldType :: DbModule -> Field -> String
persistFieldType db field = genFieldType db field ++ (maybeMaybe (fieldOptional field))
        where
            maybeMaybe True = " Maybe "
            maybeMaybe False = " "

genField :: DbModule -> Field -> String
genField db field = fieldName field ++ " " ++ persistFieldType db field

genModel :: DbModule -> Entity -> String
genModel db entity = unlines $ [ entityName entity ] 
                            ++ (indent $ (map (genField db) (entityFields entity))
                                      ++ (map genUnique (entityUniques entity)))

generateModels :: DbModule -> [(FilePath,String)]
generateModels db =  [("config/models", unlines $ map (genModel db) (dbEntities db)),
                      ("Model/Validation.hs", genValidation db )]

genEntityValidate :: DbModule -> Entity -> [String]
genEntityValidate db e = ["instance Validatable " ++ (entityName e) ++ " where "]
                       ++ (indent (["validate d = catMaybes ["]
                           ++ [join " " fieldChecks]
                           ++ ["]",""]))
              where fieldChecks = map (genFieldChecker db (entityName e)) (entityFields e)

genFieldChecker :: DbModule -> String -> Field -> String
genFieldChecker db name (Field _ fname (NormalField _ opts)) = join ",\n" $ catMaybes (map maybeCheck opts)
    where
        maybeCheck (FieldCheck func) = Just $ "if V." ++ func ++ " $ " ++ fname ++ " d == False then Just \"" ++ name ++ "." ++ fname ++ " " ++ func ++ "\" else Nothing"
        maybeCheck _ = Nothing
        
genFieldChecker db name _ = []        


genValidation :: DbModule -> String
genValidation db = unlines $ [
    "{-# LANGUAGE OverloadedStrings #-}",
    "module Model.Validation (",
    "    Validatable(..) where",
    "import Data.Text",
    "import Model.ValidationFunctions",
    "class Validatable a where",
    "    validate :: a -> [Text]"
    ] ++ concatMap (genEntityValidate db) (dbEntities db)
                   

indent :: [String] -> [String]
indent = map (\l -> "    " ++ l)
        



