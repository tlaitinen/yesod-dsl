
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator (generate, hsRouteName, genFay) where
import Prelude hiding (readFile)
import System.IO (FilePath, writeFile)
import System.IO.Strict (readFile)
import System.FilePath (joinPath)    
import System.Directory (createDirectoryIfMissing)
import Data.String.Utils (rstrip)    
import YesodDsl.AST
import Text.Shakespeare.Text hiding (toText)
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Control.Monad
import Data.Char

import YesodDsl.Generator.Models
import YesodDsl.Generator.EntityFactories
import YesodDsl.Generator.Classes
import YesodDsl.Generator.Routes
import YesodDsl.Generator.Interface
import YesodDsl.Generator.Validation
import YesodDsl.Generator.Handlers
import YesodDsl.Generator.EsqueletoInstances
import YesodDsl.Generator.Cabal
import YesodDsl.Generator.Fay
import YesodDsl.SyncFile
import Control.Monad.State
import YesodDsl.Generator.Esqueleto
imports :: Module -> String
imports m = concatMap fmtImport $ modImports m
    where
        fmtImport i = T.unpack $(codegenFile "codegen/import.cg")    
writeRoute :: Module -> Route -> IO Context
writeRoute m r = do
    let (content, ctx) = runState (liftM concat $ mapM handler (routeHandlers r)) ((emptyContext m) { ctxRoute = Just r})
    syncFile (joinPath ["Handler", moduleName m, 
                                      routeModuleName r ++ ".hs"]) $
        T.unpack $(codegenFile "codegen/route-header.cg") ++ content
    return ctx
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

    ctxs <- forM (modRoutes m) (writeRoute m)
    syncFile (joinPath ["Handler", moduleName m, "Internal.hs"]) $
        T.unpack $(codegenFile "codegen/header.cg")
            ++ models m
            ++ entityFactories m
            ++ classes m
            ++ interface m ctxs
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
    where
           routeImport r = T.unpack $(codegenFile "codegen/route-import.cg")

                            

genFay :: FilePath -> Module -> IO ()
genFay path m = do
    syncFile path $ fay m

