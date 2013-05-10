import Lexer
import Parser
import System.Environment
import ModuleMerger
import Validation
import ClassImplementer
import AST
import ASTToInt
import qualified Intermediate as I 
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
    let ast  = implementClasses . mergeModules dbs
    let errors = validate ast
    if null errors 
        then handle (astToIntermediate ast)
       else do
            hPutStrLn stderr errors
    where
        handle (Left err) = hPutStrLn stderr err
        handle (Right int) = do
            syncFiles (generateModels int)
            createFiles [("Model/ValidationFunctions.hs",
                          "module Model.ValidationFunctions where\nimport Import"),
                         ("Handler/Hooks.hs",
                          "module Handler.Hooks where\nimport Import")]

    
