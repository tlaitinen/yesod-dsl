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

genModel :: DbModule -> Entity -> String
genModel db entity = unlines $ [ entityName entity ] 
                            ++ (indent $ (map (genField db) (entityFields entity))
                                      ++ (map genUnique (entityUniques entity)))

generateModels :: DbModule -> [(FilePath,String)]
generateModels db = genCommon db 
--                  ++ map (genEntity db) (dbEntities db) 
--                  ++ concatMap (genIface db) (dbIfaces db)
                  ++ [("config/models", unlines $ map (genModel db) (dbEntities db))]

genCommon :: DbModule -> [(FilePath, String)]
genCommon db = [("Model/Common.hs", unlines $ [
    "{-# LANGUAGE DeriveDataTypeable #-}",
    "module Model.Common (",
    "    Text(..),", 
    "    Int32(..),",
    "    Int64(..),",
    "    Word32(..),",
    "    Word64(..),",
    "    UTCTime(..),",
    "    Day(..),",
    "    TimeOfDay(..),",
    "    Validatable(..),",
    "    Either(..), Maybe(..), catMaybes, isJust, fromJust, EnterException(..), throw) where",
    "import Data.Text",
    "import Data.Int",
    "import Data.Word",
    "import Data.Time",
    "import Data.Either",
    "import Data.Maybe",
    "import Data.Typeable",
    "import Control.Exception",
    "data EnterException = NoInstance deriving (Show,Typeable)",
    "instance Exception EnterException",
    "class Validatable a where",
    "    validate :: a -> [Text]"
    ])]
--    ("Model.hs", unlines $ ["module Model where"] ++
--     map (\n -> "import qualified Model." ++ n ++ " as " ++ n) allNames
 --    )] 
    where
        ifaces = dbIfaces db
        ifaceNames = map ifaceName ifaces
        ifaceInstNames = map (++"Inst") ifaceNames
        ifaceInstRefNames = map (++"InstRef")  ifaceNames
        allNames = map ifaceName (dbIfaces db) ++ map entityName (dbEntities db) 
                 ++ ifaceInstNames ++ ifaceInstRefNames
                   

indent :: [String] -> [String]
indent = map (\l -> "    " ++ l)

imports = ["import Database.Persist",
           "import Database.Persist.MongoDB",
           "import Database.Persist.TH",
           "import Language.Haskell.TH.Syntax",
           "import qualified Model.Validation as V",
           "import Model.Common"
           ]




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

maybeMaybe True = " Maybe "
maybeMaybe False = " "

persistFieldType :: DbModule -> Field -> String
persistFieldType db field = genFieldType db field ++ (maybeMaybe (fieldOptional field))
haskellFieldType :: DbModule -> Field -> String
haskellFieldType db field = maybeMaybe (fieldOptional field) ++ genFieldType db field

genField :: DbModule -> Field -> String
genField db field = fieldName field ++ " " ++ persistFieldType db field
    
importDeps :: DbModule -> String -> [String]
importDeps db name = nub $ map (\n -> "import Model." ++ n ++ " (" ++ n ++ ", " ++ n ++ "Generic, " ++ n ++ "Id)") $ lookupDeps db name
persistHeader = "share [mkPersist MkPersistSettings { mpsBackend = ConT ''Action }] [persist|"
persistFooter = "|]"

mkIfaceFieldImpl :: [String] -> String -> [String]
mkIfaceFieldImpl  entityNames fname = 
         [ 
          fname ++ " d"] ++ indent ([ "| isJust (" ++ lowerFirst name ++ " d)"
                               ++ " = Model." ++ name ++ "." ++ fname 
                               ++ "$ fromJust (" ++ lowerFirst name ++ " d)"
                               | name <- entityNames ] ++ [
                               "| otherwise = throw NoInstance"])
         ++ ["s_" ++ fname ++ " v d"] 
         ++ indent (["| isJust (" ++ lowerFirst name ++ " d)"
                        ++ " = s_" ++ (lowerFirst name) ++ 
                          " (Just $ Model." ++ name ++ ".s_" ++ fname
                           ++ " v $ fromJust (" ++ lowerFirst name ++ " d)) d"
                             | name <- entityNames]
                          ++ ["| otherwise = throw NoInstance"])
            

 

    
implIfaceInst :: DbModule -> Iface -> [String] -> String
implIfaceInst db  iface entityNames = let
            name = ifaceName iface
            instName = name ++ "Inst"
            fields = ifaceFields iface
            fieldNames = map fieldName fields
            fieldImpls = concatMap (mkIfaceFieldImpl entityNames) fieldNames
        in unlines $ ["instance " ++ name ++ "." ++ name ++ " " ++ instName ++ " where"]
                   ++ indent fieldImpls

genIfaceValidate :: DbModule -> Iface -> [String] -> String
genIfaceValidate db iface entityNames = let
        instName = ifaceName iface ++ "Inst"
        in unlines $ ["instance Validatable " ++ instName ++ " where"]
                     ++ (indent $ ["validate d"] 
                         ++(indent (["| isJust (" ++ lowerFirst name ++ " d) = "
                                 ++ " validate $ fromJust (" 
                                 ++ lowerFirst name ++ " d)" 
                                 | name <- entityNames] 
                                 ++ ["| otherwise = throw NoInstance"])))
                      


genFieldChecker :: DbModule -> String -> Field -> String
genFieldChecker db name (Field _ fname (NormalField _ opts)) = join ",\n" $ catMaybes (map maybeCheck opts)
    where
        maybeCheck (FieldCheck func) = Just $ "if not $ V." ++ func ++ " $ " ++ fname ++ " d then Just \"" ++ name ++ "." ++ fname ++ " " ++ func ++ "\" else Nothing"
        
genFieldChecker db name _ = []        

genFieldSetter :: DbModule -> String -> Field -> String
genFieldSetter db name field = unlines $ [ 
        funName ++ " :: " ++ ftype ++ " -> "  ++ name ++ " -> " ++ name,
        funName ++ " v d = d { " ++ recName name fname ++ " = v }"]
        where
            fname = fieldName field 
            funName = "s_" ++ fname
            ftype = haskellFieldType db field

genPersist :: DbModule -> [String] -> String -> [Field] -> (FilePath, String)
genPersist db extraImports name fields = 
        ("Model/" ++ name ++ ".hs",unlines $ [ 
        "{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}",
        "{-# LANGUAGE GADTs, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}",
        "module Model." ++ name ++ " where "] ++ imports 
         ++ extraImports ++ [
         persistHeader,
         name] ++ indent (map (genField db) fields) ++ [
            persistFooter
         ] ++ map genShortFieldName fields 
          ++ map (genFieldSetter db name) fields)
    where 
        genShortFieldName field = fieldName field ++ " = "
                                 ++ recName name (fieldName field) 
implEntityIfaces :: DbModule -> Entity -> String -> [String]
implEntityIfaces db entity implName = 
    let
        name = entityName entity
        (IfaceDef iface) = dbLookup db implName
        fields = ifaceFields iface
        fieldNames = map fieldName fields
        fieldImpls = [ fname ++ " = Model." ++ name ++ "." ++ fname 
                       | fname <- fieldNames ] 
                    ++ [ "s_" ++ fname ++ " = Model." ++ name ++ ".s_" ++ fname 
                       | fname <- fieldNames ] 

    in
        ["instance " ++ implName ++ "." ++ implName ++ " " ++ name ++ " where"]
        ++ indent fieldImpls

    

genEntity :: DbModule -> Entity -> (FilePath,String)
genEntity db entity = let
        extraImports = importDeps db (entityName entity) ++ ["import qualified Model." ++ iName ++ " as " ++ iName |
                         iName <- entityImplements entity ]
        (name, content) = genPersist db extraImports (entityName entity) (entityFields entity)
       
     in 
        (name, content ++ unlines (concatMap (implEntityIfaces db entity) 
                                     (entityImplements entity)
               ++ ["instance Validatable " ++ (entityName entity) ++ " where "])
               ++ (join "" (indent (["validate d = catMaybes ["]
                    ++ map (genFieldChecker db (entityName entity)) 
                                 (entityFields entity)
                    ++ ["]"]))))


