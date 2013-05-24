
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Generator (generate, hsRouteName) where

import System.IO (FilePath, writeFile)
import System.FilePath (joinPath)    
import System.Directory (createDirectoryIfMissing)
import Data.String.Utils (rstrip)    
import AST
import Text.Shakespeare.Text hiding (toText)
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Data.Char

import Generator.Models
import Generator.Classes
import Generator.Routes
import Generator.Validation
import Generator.Handlers

generate :: Module -> IO ()
generate m = do
    createDirectoryIfMissing True (joinPath ["Handler", moduleName m])
    writeFile (joinPath ["Handler", moduleName m, "Enums.hs"]) $
        T.unpack $(codegenFile "codegen/enums-header.cg")
            ++ (concatMap enum $ modEnums m)
    writeFile (joinPath ["Handler", moduleName m, "Internal.hs"]) $
        T.unpack $(codegenFile "codegen/header.cg")
            ++ models m
            ++ classes m
            ++ validation m
            ++ (T.unpack $(codegenFile "codegen/json-wrapper.cg"))
            ++ (concat [ handler m r h | r <- modRoutes m, h <- routeHandlers r ])
            ++ routes m
    writeFile (joinPath ["Handler", moduleName m ++ ".hs"]) $ 
        T.unpack $(codegenFile "codegen/dispatch.cg")
                            



