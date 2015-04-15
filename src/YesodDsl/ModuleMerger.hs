module YesodDsl.ModuleMerger (mergeModules) where
import YesodDsl.AST
import Data.List
import Data.Maybe
    
mergeModules :: [(FilePath,Module)] -> Module
mergeModules ms = foldl merge emptyModule ms'
    where ms' = map updateLocation ms

merge :: Module -> Module -> Module
merge m1 m2 = Module {
        modName = listToMaybe $ mapMaybe modName [m1, m2],
        modEntities = modEntities m1 ++ modEntities m2,
        modClasses = modClasses m1 ++ modClasses m2,
        modEnums = modEnums m1 ++ modEnums m2,
        modRoutes = modRoutes m1 ++ modRoutes m2,
        modDefines = modDefines m1 ++ modDefines m2,
        modImports = modImports m1 ++ modImports m2
    }

updateLocation :: (FilePath,Module) -> Module
updateLocation (path,m) = m {
        modEntities = map updateEntityLoc (modEntities m),
        modClasses  = map updateClassLoc  (modClasses m),
        modEnums    = map updateEnumLoc  (modEnums m),
        modRoutes   = map updateRouteLoc  (modRoutes m),
        modDefines  = map updateDefineLoc (modDefines m)
    } 
    where 
        updateEntityLoc e = e { entityLoc = updateLoc path (entityLoc e) }
        updateClassLoc i = i { classLoc = updateLoc path (classLoc i) }
        updateEnumLoc e = e { enumLoc = updateLoc path (enumLoc e) }
        updateRouteLoc r = r {
            routeLoc = updateLoc path (routeLoc r),
            routeHandlers = map updateHandlerLoc (routeHandlers r)
        }
        updateHandlerLoc h = h {
            handlerLoc = updateLoc path (handlerLoc h)
        }
        updateDefineLoc d = d {
            defineLoc = updateLoc path (defineLoc d)
        }
 
updateLoc :: FilePath -> Location -> Location
updateLoc path (Loc _ l c) = Loc path l c


