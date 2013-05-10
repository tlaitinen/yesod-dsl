import Lexer
import Parser
import System.Environment
import ModuleMerger
import Validation
import CheckServices
import ClassImplementer
import AST
import Data.List
import Generator
import SyncFiles
import System.Directory
import Control.Monad
import System.IO

createFiles :: [(FilePath, String)] -> IO ()
createFiles files = mapM_ createFile files
    where createFile (path,contents) = do
            exists <- doesFileExist path
            if exists == False
                then do
                    putStrLn $ "Created " ++ path 
                    writeFile path contents
                else return ()

main = do
    [ path ] <- getArgs
    dbs <- parse path
    let merged  = implementClasses . mergeModules dbs
    let errors = validate merged
    if null errors 
        then do
            syncFiles (generateModels impl)
            createFiles [("Model/ValidationFunctions.hs",
                          "module Model.ValidationFunctions where\nimport Import"),
                         ("Handler/Hooks.hs",
                          "module Handler.Hooks where\nimport Import")]
        else do
            hPutStrLn stderr errors
