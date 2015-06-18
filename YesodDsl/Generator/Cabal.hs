module YesodDsl.Generator.Cabal (syncCabal) where
import YesodDsl.AST
import Distribution.PackageDescription
import Distribution.ModuleName
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.PrettyPrint
import Distribution.Package
import Distribution.Version
import Data.Maybe (fromMaybe)
import System.Directory
import Distribution.Verbosity
import System.FilePath.Posix
import Data.List
import YesodDsl.Generator.Routes
import YesodDsl.SyncFile
knownMods :: Module -> [ModuleName]
knownMods m = map fromString $ [pfx, pfx ++ ".Internal", pfx ++ ".Enums", pfx ++ ".Routes", pfx ++ ".Esqueleto", pfx ++ ".PathPieces", pfx ++ ".Validation", pfx ++ ".FilterSort" ]
                ++ [pfx ++ "." ++ (routeModuleName r) | r <- modRoutes m ] ++ (map importModule $ modImports m)
    where pfx = handlerPrefix m

handlerPrefix :: Module -> String
handlerPrefix m = "Handler." ++ (fromMaybe "" $ modName m)

ensureDeps :: [Dependency] -> [Dependency]
ensureDeps deps = nubBy samePackage ([Dependency (PackageName name) anyVersion 
                       | name <- ["unordered-containers",
                                  "transformers",
                                  "tagged",
                                  "blaze-builder",
                                  "http-types",
                                  "wai",
                                  "resourcet",
                                  "attoparsec",
                                  "time",
                                  "vector",
                                  "esqueleto",
                                  "yesod-persistent",
                                  "old-locale",
                                  "filepath",
                                  "unix",
                                  "path-pieces",
                                  "conduit-extra",
                                  "exceptions"
                                  ] ] ++ deps)
    where samePackage (Dependency pn1 _) (Dependency pn2 _) = pn1 == pn2
          

modifyDesc :: Module -> GenericPackageDescription -> GenericPackageDescription
modifyDesc m d = d {
        condLibrary = (condLibrary d) >>= modifyCtree
    }
    where 
        modifyCtree ctree = Just $ ctree {
            condTreeData = modifyLib (condTreeData ctree),
            condTreeConstraints = nub $ ensureDeps (condTreeConstraints ctree)
        }
        modifyLib l = l {
            exposedModules = modifyExposed (exposedModules l)            
        }
        modifyExposed mods = nub $ filter notGeneratedMod mods ++ knownMods m
        notGeneratedMod = not . ((handlerPrefix m) `isPrefixOf`) . (intercalate ".") . components
syncCabal :: FilePath -> Module -> IO ()
syncCabal path' m = do
    let path = addExtension (dropExtension path') ".cabal"
    e <- doesFileExist path
    if e 
        then do
            desc <- readPackageDescription verbose path
            
            let content = showGenericPackageDescription (modifyDesc m desc)
            syncFile path content

        else return ()


    
    
