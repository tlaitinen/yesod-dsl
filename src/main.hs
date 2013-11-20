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
import Obfuscate

data Flag = Obfuscate deriving Eq
options :: [OptDescr Flag]
options = [
    Option ['o']     ["obfuscate"] (NoArg Obfuscate)       "obfuscated output"
  ]

header = "Usage: yesod-dsl FILE"
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (o,     (path:_), [])     -> main' o path
        (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options


    where
                         
        main' o path = do
            dbs' <- parse path
            let dbs = if Obfuscate `elem` o then obfuscate dbs' else dbs'
            let ast  = implementClasses . mergeModules $ dbs
            let errors = validate ast
            if null errors 
                then generate path $ ast 
                else hPutStrLn stderr errors
