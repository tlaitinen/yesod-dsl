import YesodDsl.Lexer
import YesodDsl.Parser
import System.Environment
import System.Console.GetOpt
import YesodDsl.ModuleMerger
import YesodDsl.ClassImplementer
import YesodDsl.Generator
import Control.Monad
import System.IO
import YesodDsl.ExpandMacros

data Flag = 
          FayPath String deriving Eq
options :: [OptDescr Flag]
options = [
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
            mast <- parse path
            case mast of
                Just ast -> do
                    generate path $ ast 
                    forM_ o (processFlag ast)
                Nothing -> return ()
        processFlag ast (FayPath path) = genFay path ast
            
        processFlag _ _ = return ()
