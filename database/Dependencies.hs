module Dependencies (makeDependencies, Deps, lookupDeps) where
import DbTypes
import Data.List
import Data.Maybe

type Name = String
data Deps = Deps [(Name, [Name])]

lookupDeps :: Deps -> Name -> [Name]
lookupDeps (Deps deps) name = maybe [] id (lookup name deps)
getFieldDeps :: Field -> [Name]
getFieldDeps field = case (fieldContent field) of
    (NormalField _ _) -> []
    (DocField _ _ docName) -> [docName]

makeDependencies :: DbModule -> Deps
makeDependencies db = Deps ([ (docName doc, 
                              concatMap getFieldDeps (docFields doc)) |
                        doc <- dbDocs db ]
                      ++ [ (ifaceName i, 
                            concatMap getFieldDeps (ifaceFields i)) 
                            | i <- dbIfaces db ])
    
    

