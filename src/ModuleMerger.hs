module ModuleMerger (mergeModules) where
import DbTypes
import Data.List

    
mergeModules :: [(FilePath,DbModule)] -> DbModule
mergeModules dbs = removeDuplicates $ foldl merge emptyDbModule dbs'
    where dbs' = map updateLocation dbs

removeDuplicates :: DbModule -> DbModule
removeDuplicates db = db {
        dbImports = nub $ dbImports db
    }

merge :: DbModule -> DbModule -> DbModule
merge db1 db2 = DbModule {
        dbImports = dbImports db1 ++ dbImports db2,
        dbEntities = dbEntities db1 ++ dbEntities db2,
        dbClasses = dbClasses db1 ++ dbClasses db2,
        dbEnums = dbEnums db1 ++ dbEnums db2
    }

updateLocation :: (FilePath,DbModule) -> DbModule
updateLocation (path,db) = db {
        dbEntities = map (updateDocLoc path) (dbEntities db),
        dbClasses    = map (updateClassLoc path) (dbClasses db)
    } 
    where 
        updateDocLoc path e = e { entityLoc = updateLoc path (entityLoc e) }
        updateClassLoc path i = i { classLoc = updateLoc path (classLoc i) }
 
updateLoc :: FilePath -> Location -> Location
updateLoc path (Loc _ l c) = Loc path l c


