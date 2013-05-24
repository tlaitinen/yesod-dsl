import Lexer
import Parser
import System.Environment
import System.Console.GetOpt
import ModuleMerger
import Validation
import ClassImplementer
import AST
import Data.List
import Data.Maybe
import Generator
import System.Directory
import Control.Monad
import System.IO

data Flag = Input FilePath 

options :: [OptDescr Flag]
options = [
  ]

header = "Usage: yesod-dsl FILE"
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (_,     (path:_), [])     -> main' path
        (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options


    where
                         
        main' path = do
            dbs <- parse path
            let ast  = implementClasses . mergeModules $ dbs
            let errors = validate ast
            if null errors 
                then generate $ ast 
                else hPutStrLn stderr errors
