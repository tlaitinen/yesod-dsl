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
docFieldDeps :: DbModule -> String -> [String]
docFieldDeps db name 
    | name `elem` [ docName doc | doc <- dbDocs db ] = [name]
    | otherwise = [name ++ "Inst", name ++ "InstRef"]

getFieldDeps :: DbModule -> Field -> [String]
getFieldDeps db field = case (fieldContent field) of
    (NormalField _ _) -> []
    (DocField _ _ docName) -> docFieldDeps db docName

lookupDeps :: DbModule -> String -> [String]
lookupDeps db name = concatMap (getFieldDeps db) $ (dbdefFields . (dbLookup db)) name

generateModels :: DbModule -> [(FilePath,String)]
generateModels db = genCommon db 
                  ++ map (genDoc db) (dbDocs db) 
                  ++ concatMap (genIface db) (dbIfaces db)

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
        allNames = map ifaceName (dbIfaces db) ++ map docName (dbDocs db) 
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
mkIfaceFieldImpl  docNames fname = 
         [ 
          fname ++ " d"] ++ indent ([ "| isJust (" ++ lowerFirst name ++ " d)"
                               ++ " = Model." ++ name ++ "." ++ fname 
                               ++ "$ fromJust (" ++ lowerFirst name ++ " d)"
                               | name <- docNames ] ++ [
                               "| otherwise = throw NoInstance"])
         ++ ["s_" ++ fname ++ " v d"] 
         ++ indent (["| isJust (" ++ lowerFirst name ++ " d)"
                        ++ " = s_" ++ (lowerFirst name) ++ 
                          " (Just $ Model." ++ name ++ ".s_" ++ fname
                           ++ " v $ fromJust (" ++ lowerFirst name ++ " d)) d"
                             | name <- docNames]
                          ++ ["| otherwise = throw NoInstance"])
            

 

    
implIfaceInst :: DbModule -> Iface -> [String] -> String
implIfaceInst db  iface docNames = let
            name = ifaceName iface
            instName = name ++ "Inst"
            fields = ifaceFields iface
            fieldNames = map fieldName fields
            fieldImpls = concatMap (mkIfaceFieldImpl docNames) fieldNames
        in unlines $ ["instance " ++ name ++ "." ++ name ++ " " ++ instName ++ " where"]
                   ++ indent fieldImpls

genIfaceValidate :: DbModule -> Iface -> [String] -> String
genIfaceValidate db iface docNames = let
        instName = ifaceName iface ++ "Inst"
        in unlines $ ["instance Validatable " ++ instName ++ " where"]
                     ++ (indent $ ["validate d"] 
                         ++(indent (["| isJust (" ++ lowerFirst name ++ " d) = "
                                 ++ " validate $ fromJust (" 
                                 ++ lowerFirst name ++ " d)" 
                                 | name <- docNames] 
                                 ++ ["| otherwise = throw NoInstance"])))
                      

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
            instDeps = [(instName, docNames)]
            instRefDeps = [(instRefName,  docNames)]
            extraImports = ["import qualified Model." ++ifaceName iface ++ " as " ++ ifaceName iface ]
            instImports = extraImports ++ ["import Model." ++ d ++ " (" ++ d ++ ")" | d <- docNames ] 
                     ++ ["import qualified Model." ++ d | d <- docNames ]
            instRefImports = extraImports ++ ["import Model." ++ d ++ " (" ++ d ++ "Id, " ++ d ++ "Generic)" | d <- docNames ]

            (instFileName, instContent) = genPersist db instImports instName instFields
            (instRefFileName, instRefContent) = genPersist db instRefImports instRefName instRefFields
           in
              [ (instFileName, instContent   
                              ++ implIfaceInst db iface docNames
                              ++ genIfaceValidate db iface docNames),
                (instRefFileName, instRefContent) ]


        

genIface :: DbModule -> Iface -> [(FilePath,String)]
genIface db iface = [("Model/" ++ name ++ ".hs",unlines $[
        "{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}",
        "{-# LANGUAGE GADTs, FlexibleContexts #-}",
        "module Model." ++ name ++ "(" ++ exportList ++ ") where ",
        "import Model.Common"] ++ importDeps db name ++ [
        "class " ++ name ++ " a where "]
        ++ indent (map genIfaceFieldGetter fields)
        ++ indent (map genIfaceFieldSetter fields))]
        ++ mkIfaceInstance db iface
    where name = ifaceName iface
          fields = ifaceFields iface
          exportList = name ++ "(..)"
          genIfaceFieldGetter field = (fieldName field)
                                ++ " :: a ->" ++ haskellFieldType db field
          genIfaceFieldSetter field = "s_" ++ (fieldName field)
                                ++ " :: " ++ haskellFieldType db field
                                ++ " -> a -> a"
genFieldChecker :: DbModule -> String -> Field -> String
genFieldChecker db name (Field _ fname (NormalField _ opts)) = join ",\n" $ catMaybes (map maybeCheck opts)
    where
        maybeCheck (FieldCheck func) = Just $ "if not $ V." ++ func ++ " $ " ++ fname ++ " d then Just \"" ++ name ++ "." ++ fname ++ " " ++ func ++ "\" else Nothing"
        maybeCheck _ = Nothing
        
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
implDocIfaces :: DbModule -> Doc -> String -> [String]
implDocIfaces db doc implName = 
    let
        name = docName doc
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

    

genDoc :: DbModule -> Doc -> (FilePath,String)
genDoc db doc = let
        extraImports = importDeps db (docName doc) ++ ["import qualified Model." ++ iName ++ " as " ++ iName |
                         iName <- docImplements doc ]
        (name, content) = genPersist db extraImports (docName doc) (docFields doc)
       
     in 
        (name, content ++ unlines (concatMap (implDocIfaces db doc) 
                                     (docImplements doc)
               ++ ["instance Validatable " ++ (docName doc) ++ " where "])
               ++ (join "" (indent (["validate d = catMaybes ["]
                    ++ map (genFieldChecker db (docName doc)) 
                                 (docFields doc)
                    ++ ["]"]))))


