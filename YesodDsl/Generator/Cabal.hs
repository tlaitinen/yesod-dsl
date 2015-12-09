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
import Control.Monad (when)
         

syncCabal :: FilePath -> [String] -> String -> [String]-> IO ()
syncCabal path extraDeps generatedModule moduleNames = do
    e <- doesFileExist path
    when e $ do
        desc <- readPackageDescription verbose path
        syncFile path $ showGenericPackageDescription $ modifyDesc desc
    where
        modifyDesc d = d {
                condLibrary = (condLibrary d) >>= modifyCtree
            }
        modifyCtree ctree = Just $ ctree {
            condTreeData = modifyLib (condTreeData ctree),
            condTreeConstraints = nub $ ensureDeps $ condTreeConstraints ctree
        }
        modifyLib l = l {
            exposedModules = modifyExposed (exposedModules l)            
        }

        samePackage (Dependency pn1 _) (Dependency pn2 _) = pn1 == pn2

        ensureDeps deps = nubBy samePackage $ [ Dependency (PackageName name) anyVersion 
                           | name <- extraDeps ] ++ deps
        modifyExposed mods = nub $ filter notGeneratedMod mods ++ map fromString moduleNames
        notGeneratedMod = not . (generatedModule `isPrefixOf`) . (intercalate ".") . components   
