module Generator (generateModels) where
import System.IO (FilePath)
import DbTypes
import DbLexer
generateModels :: DbModule -> [(FilePath,String)]
generateModels db = map genDoc (dbDocs db) ++ map genIface (dbIfaces db)

imports = ["import Database.Persist",
           "import Database.Persist.MongoDB",
           "import Database.Persist.TH",
           "import Language.Haskell.TH.Syntax"]

persistHeader = "share [mkPersist MkPersistSettings { mpsBackend = ConT ''Action }]"



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


genFields :: [Field] -> String
genFields fields = ""


genIface :: Iface -> (FilePath,String)
genIface iface = ("Model/" ++ name ++ ".hs",unlines $[
        "module " ++ name ++ " where ",
        "class " ++ name ++ " a where "]
        ++ (map genIfaceField (ifaceFields iface))
        )
    where name = ifaceName iface
          genIfaceField field = "    " 
                              ++ (fieldName field)
                              ++ " :: a -> " ++ genFieldType field

genDoc :: Doc -> (FilePath,String)
genDoc doc = ("Model/" ++ name ++ ".hs",unlines $ [ 
        "module " ++ name ++ " where "] ++ imports ++ [
        ])
    where name = docName doc
