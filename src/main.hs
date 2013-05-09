import Lexer
import Parser
import System.Environment
import ModuleMerger
import NameFinder
import CheckServices
import ClassImplementer
import AST
import Data.List
import Generator
import SyncFiles
import System.Directory
import Control.Monad

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
    let merged    = mergeModules dbs
    let impl      = (checkServices . implementInterfaces) merged
    let generated = generateModels impl
    syncFiles generated
    createFiles [("Model/ValidationFunctions.hs",
                  "module Model.ValidationFunctions where\nimport Import"),
                 ("Handler/Hooks.hs",
                  "module Handler.Hooks where\nimport Import")]
