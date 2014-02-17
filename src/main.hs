import Lexer
import Parser
import System.Environment
import System.Console.GetOpt
import ModuleMerger
import Validation
import ClassImplementer
import Generator
import Control.Monad
import System.IO
import Obfuscate

data Flag = Obfuscate 
          | FayPath String deriving Eq
options :: [OptDescr Flag]
options = [
    Option ['o']     ["obfuscate"] (NoArg Obfuscate)        "obfuscated output",
    Option ['f']     ["fay"] (ReqArg FayPath "FILE") "translate DSL definition to Fay compatible code and store it in FILE"
  ]

header :: String

header = "Usage: yesod-dsl FILE"
main :: IO ()
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
                then do
                    generate path $ ast 
                    forM_ o (processFlag ast)
                else hPutStrLn stderr errors
        processFlag ast (FayPath path) = genFay path ast
            
        processFlag _ _ = return ()
