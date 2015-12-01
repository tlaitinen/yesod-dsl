import Paths_yesod_dsl (version)
import Data.Version (showVersion)
import YesodDsl.Parser
import System.Environment
import System.Console.GetOpt
import YesodDsl.Generator
import Control.Monad

data Flag = Version 
          | JsonPath String 
          | HsClient String
          | PureScriptPath String deriving Eq
          
options :: [OptDescr Flag]
options = [
    Option [] ["json"] (ReqArg JsonPath "FILE") "translate DSL definition to JSON object",
    Option [] ["purescript"] (ReqArg PureScriptPath "FILE") "generate client-side PureScript module",
    Option [] ["hs-client"] (ReqArg HsClient "PATH") "generate client-side Haskell modules",
    Option ['v'] ["version"] (NoArg Version) "print version number"
  ]

header :: String

header = "Usage: yesod-dsl FILE"
main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (o,     (path:_), [])     -> main' o path
        (o,     _,       msgs)   -> if Version `elem` o
            then versionInfo 
            else error $ concat msgs ++ usageInfo header options


    where
        versionInfo = putStrLn $ "yesod-dsl " ++ showVersion version
        main' o path = do
            if Version `elem` o
                then versionInfo
                else do
                    mast <- parse path
                    case mast of
                        Just ast -> do
                            generate path $ ast 
                            forM_ o (processFlag ast)
                        Nothing -> return ()
        processFlag ast (JsonPath path) = genJson path ast
        processFlag ast (PureScriptPath path) = genPureScript path ast
        processFlag ast (HsClient path) = genHsClient path ast
        processFlag _ _ = return ()
