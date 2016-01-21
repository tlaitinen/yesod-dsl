
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator (generate, hsRouteName, genJson, genPureScript, genHsClient) where
import Prelude hiding (readFile)
import System.FilePath (joinPath)    
import System.Directory (createDirectoryIfMissing)
import YesodDsl.AST
import Text.Shakespeare.Text hiding (toText)
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Control.Monad
import System.FilePath (replaceExtension, dropExtension)
import Data.String.Utils (replace)

import YesodDsl.Generator.Models
import YesodDsl.Generator.EntityFactories
import YesodDsl.Generator.Classes
import YesodDsl.Generator.Routes
import YesodDsl.Generator.Validation
import YesodDsl.Generator.Handlers
import YesodDsl.Generator.EsqueletoInstances
import YesodDsl.Generator.Cabal
import YesodDsl.Generator.Json
import YesodDsl.Generator.PureScript
import YesodDsl.Generator.HsClient
import YesodDsl.SyncFile
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map

allImports :: Module -> String
allImports m = concatMap fmtImport $ modImports m

fmtImport :: Import -> String
fmtImport i = T.unpack $(codegenFile "codegen/import.cg")    

routeFile :: Module -> Route -> (FilePath, String)
routeFile m r = (joinPath ["Handler", moduleName m, 
                                      routeModuleName r ++ ".hs"],
                 T.unpack $(codegenFile "codegen/route-header.cg") 
                    ++ content)
    where
        content = concatMap (handler m r) (routeHandlers r)
        imports = concatMap fmtImport $ filter ((`elem` modules) . importModule) $ modImports m
        modules = nub $ catMaybes $ [ 
                            Map.lookup fn importedFunctions 
                            | fn <- usedFunctions 
                ] 
        importedFunctions = Map.fromList [ (fn, importModule i) |
                                            i <- modImports m,
                                            fn <- importFunctions i ]
                                            
        usedFunctions = [ fn | Call fn _ <- universeBi r ] ++ [ fn | ExternExpr fn _ <- universeBi r ] 
                        ++ concat [ catMaybes [ mm | (_,_,mm) <- ifs ] 
                                    | Update _ _ (Just ifs) <- universeBi r ] 
                        ++ concat [ catMaybes [ mm | (_,_,mm) <- ifs ] | Insert _ (Just (Just _, ifs)) _ <- universeBi r ]
pathToModuleName :: String -> String
pathToModuleName = (replace "/" ".") . dropExtension                        

generate :: FilePath -> Module -> IO ()
generate path m = do
    createDirectoryIfMissing True (joinPath ["Handler", moduleName m])

    forM_ files $ uncurry syncFile
    syncCabal (replaceExtension path ".cabal") cabalDeps (moduleName m) moduleNames
    where
        moduleNames = map (pathToModuleName . fst) files
        files = [(joinPath ["Handler", moduleName m, "Enums.hs"],
                  T.unpack $(codegenFile "codegen/enums-header.cg")
                    ++ (concatMap enum $ modEnums m)),
                 (joinPath ["Handler", moduleName m, "Esqueleto.hs"],
                  T.unpack $(codegenFile "codegen/esqueleto-header.cg")
                    ++ (esqueletoInstances m)),
                 (joinPath ["Handler", moduleName m, "Internal.hs"],
                  T.unpack $(codegenFile "codegen/header.cg")
                        ++ models m
                        ++ entityFactories m
                        ++ classes m
                        ++ (T.unpack $(codegenFile "codegen/json-wrapper.cg"))),
                 (joinPath ["Handler", moduleName m, "Validation.hs"],
                  T.unpack $(codegenFile "codegen/validation-header.cg")
                    ++ (concatMap validationEntity (modEntities m))),
                 (joinPath ["Handler", moduleName m, "Routes.hs"],
                  routes m),
                 (joinPath ["Handler", moduleName m ++ ".hs"],
                  T.unpack $(codegenFile "codegen/dispatch.cg")),
                 (joinPath ["Handler", moduleName m, "PathPieces.hs"],
                  T.unpack $(codegenFile "codegen/path-pieces.cg")),
                 (joinPath ["Handler", moduleName m, "FilterSort.hs"],
                  T.unpack $(codegenFile "codegen/filter-sort.cg"))]
                    ++ [ routeFile m r | r <- modRoutes m ]
        routeImport r = T.unpack $(codegenFile "codegen/route-import.cg")

        cabalDeps = ["unordered-containers",
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
                    ] 
                            

genJson :: FilePath -> Module -> IO ()
genJson path m = syncFile path $ moduleToJson m

genPureScript :: FilePath -> Module -> IO ()
genPureScript path m = do
    syncFile path $ moduleToPureScript m

genHsClient :: FilePath -> Module -> IO ()
genHsClient path m = do
    createDirectoryIfMissing True $ joinPath [path, clientModuleName ]
    forM_ (moduleToHsClient m) $ \(name, src) -> do
        syncFile (joinPath [path, name]) src 
    syncCabal (joinPath [ path, path ++ ".cabal"]) cabalDeps (clientModuleName) moduleNames
    where
        moduleNames = map (pathToModuleName . fst) $ moduleToHsClient m
        clientModuleName = moduleName m ++ "Client"
        cabalDeps = ["aeson", "wreq", "time", "text"]    
