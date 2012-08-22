module Generator (generateModels) where
import System.IO (FilePath)
import DbTypes
import DbLexer
generateModels :: DbModule -> [(FilePath,String)]
generateModels db = genCommon db ++ map genDoc (dbDocs db) ++ map genIface (dbIfaces db)


genCommon :: DbModule -> [(FilePath, String)]
genCommon db = [("Model/Common.hs", unlines $ [
    "module Model.Common (",
    "    Text(..),", 
    "    Int32(..),",
    "    Int64(..),",
    "    Word32(..),",
    "    Word64(..),",
    "    Double(..),",
    "    UTCTime(..),",
    "    Day(..),",
    "    TimeOfDay(..)) where",
    "import Data.Text",
    "import Data.Int",
    "import Data.Word",
    "import Data.Double",
    "import Data.Time"
    ])]

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
    


genIface :: Iface -> (FilePath,String)
genIface iface = ("Model/" ++ name ++ ".hs",unlines $[
        "module Model." ++ name ++ " where ",
        "import Model.Common",
        "class " ++ name ++ " a where "]
        ++ indent (map genIfaceField (ifaceFields iface))
        )
    where name = ifaceName iface
          genIfaceField field = (fieldName field)
                                ++ " :: a ->" ++ maybeMaybe (fieldOptional field) ++ genFieldType field

genDoc :: Doc -> (FilePath,String)
genDoc doc = ("Model/" ++ name ++ ".hs",unlines $ [ 
        "{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}",
        "{-# LANGUAGE GADTs, FlexibleContexts #-}",
        "module Model." ++ name ++ " where "] ++ imports ++ [
        "share [mkPersist MkPersistSettings { mpsBackend = ConT ''Action }] [persist|",
        name] ++ indent (map genField (docFields doc)) ++ [
        "|]"
        ])
    where name = docName doc
