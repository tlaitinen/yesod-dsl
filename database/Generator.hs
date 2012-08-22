module Generator (generateModels) where
import System.IO (FilePath)
import DbTypes
generateModels :: DbModule -> [(FilePath,String)]
generateModels db = map genDoc (dbDocs db) ++ map genIface (dbIfaces db)

imports = ["import Database.Persist",
           "import Database.Persist.MongoDB",
           "import Database.Persist.TH",
           "import Language.Haskell.TH.Syntax"]

persistHeader = "share [mkPersist MkPersistSettings { mpsBackend = ConT ''Action }]"

genFields :: [Field] -> String
genFields fields = ""

genIface :: Iface -> (FilePath,String)
genIface iface = ("Model/" ++ name ++ ".hs",unlines $[
        "module " ++ name ++ " where "] ++ imports ++ [

        ])
    where name = ifaceName iface

genDoc :: Doc -> (FilePath,String)
genDoc doc = ("Model/" ++ name ++ ".hs",unlines $ [ 
        "module " ++ name ++ " where "] ++ imports ++ [
        ])
    where name = docName doc
