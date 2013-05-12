import Lexer
import Parser
import System.Environment
import ModuleMerger
import Validation
import ClassImplementer
import AST
import Data.List
import Generator
import SyncFiles
import System.Directory
import Control.Monad
import System.IO

main = do
    [ path ] <- getArgs
    dbs <- parse path
    let ast  = implementClasses . mergeModules $ dbs
    let errors = validate ast
    if null errors 
        then do
            putStr $ generate ast
        else do
            hPutStrLn stderr errors
