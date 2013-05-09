module ModuleMerger (mergeModules) where
import AST
import Data.List

    
mergeModules :: [(FilePath,Module)] -> Module
mergeModules dbs = removeDuplicates $ foldl merge emptyModule dbs'
    where dbs' = map updateLocation dbs

removeDuplicates :: Module -> Module
removeDuplicates db = db {
        dbImports = nub $ dbImports db
    }

merge :: Module -> Module -> Module
merge db1 db2 = Module {
        dbImports = dbImports db1 ++ dbImports db2,
        dbEntities = dbEntities db1 ++ dbEntities db2,
        dbClasses = dbClasses db1 ++ dbClasses db2,
        dbEnums = dbEnums db1 ++ dbEnums db2,
        dbResources = dbResources db1 ++ dbResources db2
    }

updateLocation :: (FilePath,Module) -> Module
updateLocation (path,db) = db {
        dbEntities = map (updateDocLoc path) (dbEntities db),
        dbClasses    = map (updateClassLoc path) (dbClasses db)
    } 
    where 
        updateDocLoc path e = e { entityLoc = updateLoc path (entityLoc e) }
        updateClassLoc path i = i { classLoc = updateLoc path (classLoc i) }
 
updateLoc :: FilePath -> Location -> Location
updateLoc path (Loc _ l c) = Loc path l c


