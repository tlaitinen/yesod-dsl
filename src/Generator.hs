
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Generator (generate, hsRouteName) where
import Prelude hiding (readFile)
import System.IO (FilePath, writeFile)
import System.IO.Strict (readFile)
import System.FilePath (joinPath)    
import System.Directory (createDirectoryIfMissing)
import Data.String.Utils (rstrip)    
import AST
import Text.Shakespeare.Text hiding (toText)
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Control.Monad
import Data.Char

import Generator.Models
import Generator.Classes
import Generator.Routes
import Generator.Validation
import Generator.Handlers
import Generator.EsqueletoInstances

syncFile :: FilePath -> String -> IO ()
syncFile path content = do
    oldContent <- catch (readFile path) (\_ -> return "")
    if content /= oldContent
        then do
            putStrLn $ "Updating " ++ path
            writeFile path content
        else return ()

writeRoute :: Module -> Route -> IO ()
writeRoute m r = syncFile (joinPath ["Handler", moduleName m, 
                                      routeModuleName r ++ ".hs"]) $
    T.unpack $(codegenFile "codegen/route-header.cg")
        ++ (concatMap (handler m r) (routeHandlers r))
generate :: Module -> IO ()
generate m = do
    createDirectoryIfMissing True (joinPath ["Handler", moduleName m])
    syncFile (joinPath ["Handler", moduleName m, "Enums.hs"]) $
        T.unpack $(codegenFile "codegen/enums-header.cg")
            ++ (concatMap enum $ modEnums m)
    syncFile (joinPath ["Handler", moduleName m, "Esqueleto.hs"]) $
        T.unpack $(codegenFile "codegen/esqueleto-header.cg")
        ++ (esqueletoInstances m)        
    syncFile (joinPath ["Handler", moduleName m, "Internal.hs"]) $
        T.unpack $(codegenFile "codegen/header.cg")
            ++ models m
            ++ classes m
            ++ validation m
            ++ (T.unpack $(codegenFile "codegen/json-wrapper.cg"))      
    forM_ (modRoutes m) (writeRoute m)
    syncFile (joinPath ["Handler", moduleName m, "Routes.hs"]) $
           routes m
    syncFile (joinPath ["Handler", moduleName m ++ ".hs"]) $ 
        T.unpack $(codegenFile "codegen/dispatch.cg")
    where
           routeImport r = T.unpack $(codegenFile "codegen/route-import.cg")

                            



