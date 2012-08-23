module Dependencies (makeDependencies, Deps, lookupDeps) where
import DbTypes
import Data.List
import Data.Maybe

type Name = String
type Deps = [(Name, [Name])]

lookupDeps :: Deps -> Name -> [Name]
lookupDeps deps name = maybe [] id (lookup name deps)

docFieldDeps :: DbModule -> Name -> [Name]
docFieldDeps db name 
    | name `elem` [ docName doc | doc <- dbDocs db ] = [name]
    | otherwise = [name ++ "Inst", name ++ "InstRef"]

getFieldDeps :: DbModule -> Field -> [Name]
getFieldDeps db field = case (fieldContent field) of
    (NormalField _ _) -> []
    (DocField _ _ docName) -> docFieldDeps db docName

makeDependencies :: DbModule -> Deps
makeDependencies db = ([ (docName doc, 
                              concatMap (getFieldDeps db) (docFields doc)) |
                        doc <- dbDocs db ]
                      ++ [ (ifaceName i, 
                            concatMap (getFieldDeps db) (ifaceFields i)) 
                            | i <- dbIfaces db ])
    
    

