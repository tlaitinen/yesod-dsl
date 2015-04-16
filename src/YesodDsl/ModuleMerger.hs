module YesodDsl.ModuleMerger (mergeModules) where
import YesodDsl.AST
import Data.List
import Data.Maybe
import Data.Generics
    
mergeModules :: [(FilePath,Module)] -> Module
mergeModules ms = foldl merge emptyModule $ map updateLocation ms

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
updateLocation (path,m) = everywhere (mkT (updateLoc path)) m
    where
        updateLoc path (Loc _ l c) = Loc path l c


