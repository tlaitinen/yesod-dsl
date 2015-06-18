
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator (generate, hsRouteName, genJson) where
import Prelude hiding (readFile)
import System.FilePath (joinPath)    
import System.Directory (createDirectoryIfMissing)
import YesodDsl.AST
import Text.Shakespeare.Text hiding (toText)
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Control.Monad

import YesodDsl.Generator.Models
import YesodDsl.Generator.EntityFactories
import YesodDsl.Generator.Classes
import YesodDsl.Generator.Routes
import YesodDsl.Generator.Validation
import YesodDsl.Generator.Handlers
import YesodDsl.Generator.EsqueletoInstances
import YesodDsl.Generator.Cabal
import YesodDsl.Generator.Json
import YesodDsl.SyncFile
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map

allImports :: Module -> String
allImports m = concatMap fmtImport $ modImports m

fmtImport :: Import -> String
fmtImport i = T.unpack $(codegenFile "codegen/import.cg")    

writeRoute :: Module -> Route -> IO ()
writeRoute m r = do
    let content = concatMap (handler m r) (routeHandlers r)
    syncFile (joinPath ["Handler", moduleName m, 
                                      routeModuleName r ++ ".hs"]) $
        T.unpack $(codegenFile "codegen/route-header.cg") ++ content
    where
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
generate :: FilePath -> Module -> IO ()
generate path m = do
    syncCabal path m
    createDirectoryIfMissing True (joinPath ["Handler", moduleName m])
    syncFile (joinPath ["Handler", moduleName m, "Enums.hs"]) $
        T.unpack $(codegenFile "codegen/enums-header.cg")
            ++ (concatMap enum $ modEnums m)
    syncFile (joinPath ["Handler", moduleName m, "Esqueleto.hs"]) $
        T.unpack $(codegenFile "codegen/esqueleto-header.cg")
        ++ (esqueletoInstances m)        

    forM_ (modRoutes m) (writeRoute m)
    syncFile (joinPath ["Handler", moduleName m, "Internal.hs"]) $
        T.unpack $(codegenFile "codegen/header.cg")
            ++ models m
            ++ entityFactories m
            ++ classes m
            ++ (T.unpack $(codegenFile "codegen/json-wrapper.cg"))      
    syncFile (joinPath ["Handler", moduleName m, "Validation.hs"]) $
        T.unpack $(codegenFile "codegen/validation-header.cg")
            ++ (concatMap validationEntity (modEntities m))
    syncFile (joinPath ["Handler", moduleName m, "Routes.hs"]) $
           routes m
    syncFile (joinPath ["Handler", moduleName m ++ ".hs"]) $ 
        T.unpack $(codegenFile "codegen/dispatch.cg")
    syncFile (joinPath ["Handler", moduleName m, "PathPieces.hs"]) $
        T.unpack $(codegenFile "codegen/path-pieces.cg")
    syncFile (joinPath ["Handler", moduleName m, "FilterSort.hs"]) $
        T.unpack $(codegenFile "codegen/filter-sort.cg")    
    where
           routeImport r = T.unpack $(codegenFile "codegen/route-import.cg")

                            

genJson :: FilePath -> Module -> IO ()
genJson path m = syncFile path $ moduleToJson m

