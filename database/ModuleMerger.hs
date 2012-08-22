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
        dbDocs = dbDocs db1 ++ dbDocs db2,
        dbIfaces = dbIfaces db1 ++ dbIfaces db2
    }

updateLocation :: (FilePath,DbModule) -> DbModule
updateLocation (path,db) = db {
        dbDocs = map (updateDocLoc path) (dbDocs db),
        dbIfaces    = map (updateIfaceLoc path) (dbIfaces db)
    } 
    where 
        updateDocLoc path e = e { docLoc = updateLoc path (docLoc e) }
        updateIfaceLoc path i = i { ifaceLoc = updateLoc path (ifaceLoc i) }
 
updateLoc :: FilePath -> Location -> Location
updateLoc path (Loc _ l c) = Loc path l c


