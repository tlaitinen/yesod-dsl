module Generator (generateModels) where
import System.IO (FilePath)
import DbTypes
import DbLexer
import Dependencies
import Data.Char
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


generateModels :: DbModule -> [(FilePath,String)]
generateModels db = genCommon db 
                  ++ map (genDoc deps) (dbDocs db) 
                  ++ map (genIface deps) (dbIfaces db)
        where deps = makeDependencies db

genCommon :: DbModule -> [(FilePath, String)]
genCommon db = [("Model/Common.hs", unlines $ [
    "module Model.Common (",
    "    Text(..),", 
    "    Int32(..),",
    "    Int64(..),",
    "    Word32(..),",
    "    Word64(..),",
    "    UTCTime(..),",
    "    Day(..),",
    "    TimeOfDay(..)) where",
    "import Data.Text",
    "import Data.Int",
    "import Data.Word",
    "import Data.Time"
    ]),
    ("Model.hs", unlines $ ["module Model where"] ++
     map (\n -> "import qualified Model." ++ n ++ " as " ++ n) allNames
     )] 
    where
        allNames = map ifaceName (dbIfaces db) ++ map docName (dbDocs db)

indent :: [String] -> [String]
indent = map (\l -> "    " ++ l)

imports = ["import Database.Persist",
           "import Database.Persist.MongoDB",
           "import Database.Persist.TH",
           "import Language.Haskell.TH.Syntax",
           "import Model.Common"]




genFieldType :: Field -> String
genFieldType field = case (fieldContent field) of
    (NormalField ftype _)   -> fromTkType ftype
    (DocField embed list docName) -> fromDocField embed list docName
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
        fromDocField embed list docName = lbrack ++ docName ++ maybeId ++ rbrack
            where
                (lbrack,rbrack) 
                    | list == ListField = ("[","]")
                    | otherwise = ("","")
                maybeId
                    | embed == EmbedField = ""
                    | otherwise = "Id"


maybeMaybe True = " Maybe "
maybeMaybe False = " "

genField :: Field -> String
genField field = fieldName field ++ " " ++ genFieldType field ++ (maybeMaybe (fieldOptional field))
    
importDeps :: Deps -> String -> [String]
importDeps deps name = map (\n -> "import Model." ++ n) $ lookupDeps deps name

genIface :: Deps -> Iface -> (FilePath,String)
genIface deps iface = ("Model/" ++ name ++ ".hs",unlines $[
        "module Model." ++ name ++ " where ",
        "import Model.Common"] ++ importDeps deps name ++ [
        "class " ++ name ++ " a where "]
        ++ indent (map genIfaceField (ifaceFields iface))
        )
    where name = ifaceName iface
          genIfaceField field = (fieldName field)
                                ++ " :: a ->" ++ maybeMaybe (fieldOptional field) ++ genFieldType field

genDoc :: Deps -> Doc -> (FilePath,String)
genDoc deps doc = ("Model/" ++ name ++ ".hs",unlines $ [ 
        "{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}",
        "{-# LANGUAGE GADTs, FlexibleContexts #-}",
        "module Model." ++ name ++ " where "] ++ imports 
        ++ importDeps deps name ++ [
        "share [mkPersist MkPersistSettings { mpsBackend = ConT ''Action }] [persist|",
        name] ++ indent (map genField (docFields doc)) ++ [
        "|]"
        ] ++ map genShortFieldName (docFields doc)) 
    where name = docName doc
          genShortFieldName field = fieldName field ++ " = "
                                    ++ recName name (fieldName field) 
