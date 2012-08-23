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
                  ++ map (genDoc db deps) (dbDocs db) 
                  ++ concatMap (genIface db deps) (dbIfaces db)
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
        ifaces = dbIfaces db
        ifaceNames = map ifaceName ifaces
        ifaceInstNames = map (++"Inst") ifaceNames
        ifaceInstRefNames = map (++"InstRef")  ifaceNames
        allNames = map ifaceName (dbIfaces db) ++ map docName (dbDocs db) 
                 ++ ifaceInstNames ++ ifaceInstRefNames
                   

indent :: [String] -> [String]
indent = map (\l -> "    " ++ l)

imports = ["import Database.Persist",
           "import Database.Persist.MongoDB",
           "import Database.Persist.TH",
           "import Language.Haskell.TH.Syntax",
           "import Model.Common",
           "import Model.Validation"
           ]

mkDocFieldName :: DbModule -> DocFieldEmbedding -> DocFieldType -> DocName -> String
mkDocFieldName db embed list dName = lbrack ++ dName ++ suffix ++ rbrack
    where
        (lbrack,rbrack) 
            | list == ListField = ("[","]")
            | otherwise = ("","")
        maybeId
            | embed == EmbedField = ""
            | otherwise = "Id"
        maybeInst
            | embed == EmbedField = "Inst"
            | otherwise = "InstRef"
        suffix
            | isDocument dName = maybeId
            | otherwise = maybeInst
        isDocument dn = dn `elem` [ docName doc | doc <- dbDocs db ]


genFieldType :: DbModule -> Field -> String
genFieldType db field = case (fieldContent field) of
    (NormalField ftype _)   -> fromTkType ftype
    (DocField embed list docName) -> mkDocFieldName db embed list docName
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

genField :: DbModule -> Field -> String
genField db field = fieldName field ++ " " ++ genFieldType db field ++ (maybeMaybe (fieldOptional field))
    
importDeps :: Deps -> String -> [String]
importDeps deps name = map (\n -> "import Model." ++ n) $ lookupDeps deps name
persistHeader = "share [mkPersist MkPersistSettings { mpsBackend = ConT ''Action }] [persist|"
persistFooter = "|]"
mkIfaceInstance :: DbModule -> Iface -> [(FilePath, String)]
mkIfaceInstance db iface = let
            instName = ifaceName iface ++ "Inst"
            instRefName = ifaceName iface ++ "InstRef"
            docs = dbDocs db
            implDocs = [ doc | doc <- docs,
                               (ifaceName iface) `elem` docImplements doc ]
            docNames = [ docName doc | doc <- implDocs ]
            instFields = [ Field True (lowerFirst name) 
                                 (DocField EmbedField SingleField name)
                           | name <- docNames ]
            instRefFields = [ Field True ((lowerFirst name) ++ "Id")
                                 (DocField RefField SingleField name)
                           | name <- docNames ]
            instDeps = [(instName, [ifaceName iface] ++ docNames)]
            instRefDeps = [(instRefName, [ifaceName iface] ++ docNames)]
        in [genPersist db instDeps instName instFields,
            genPersist db instRefDeps instRefName instRefFields]


        

genIface :: DbModule -> Deps -> Iface -> [(FilePath,String)]
genIface db deps iface = [("Model/" ++ name ++ ".hs",unlines $[
        "{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}",
        "{-# LANGUAGE GADTs, FlexibleContexts #-}",
        "module Model." ++ name ++ " where ",
        "import Model.Common"] ++ imports ++ importDeps deps name ++ [
        "class " ++ name ++ " a where "]
        ++ indent (map genIfaceField (ifaceFields iface)))]
        ++ mkIfaceInstance db iface
    where name = ifaceName iface
          genIfaceField field = (fieldName field)
                                ++ " :: a ->" ++ maybeMaybe (fieldOptional field) ++ genFieldType db field
genPersist :: DbModule -> Deps -> String -> [Field] -> (FilePath, String)
genPersist db deps name fields = 
        ("Model/" ++ name ++ ".hs",unlines $ [ 
        "{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}",
        "{-# LANGUAGE GADTs, FlexibleContexts #-}",
        "module Model." ++ name ++ " where "] ++ imports 
        ++ importDeps deps name ++ [
        persistHeader,
        name] ++ indent (map (genField db) fields) ++ [
        persistFooter
        ] ++ map genShortFieldName fields)
    where 
        genShortFieldName field = fieldName field ++ " = "
                                 ++ recName name (fieldName field) 

genDoc :: DbModule -> Deps -> Doc -> (FilePath,String)
genDoc db deps doc = let
        (name, content) = genPersist db deps (docName doc) (docFields doc)
     in 
        (name, content ++ [
            ])
