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
    Option ['i'] ["input"]  (ReqArg Input "FILE")      "read Yesod DSL definitions from FILE"
  ]

data Opts = Opts {
    optInput      :: Maybe FilePath
}

foldOptions :: [Flag] -> Opts
foldOptions fs = foldl f (Opts Nothing) fs
    where f o (Input i) = o { optInput = Just i }

checkOptions :: Opts -> Opts
checkOptions o
    | isNothing $ optInput o = error $ "missing -i parameter\n" ++ usageInfo header options
    | otherwise = o

header = "Usage: yesod-dsl [OPTION...]"
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (flags, [],      [])     -> main' $ checkOptions $ foldOptions flags
        (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
        (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

    where
                         
        main' o = do
            dbs <- parse (fromJust $ optInput o)
            let ast  = implementClasses . mergeModules $ dbs
            let errors = validate ast
            if null errors 
                then generate $ ast 
                else hPutStrLn stderr errors
