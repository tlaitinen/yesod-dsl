{-# LANGUAGE DeriveDataTypeable #-}
import Paths_yesod_dsl (version)
import Data.Version (showVersion)
import YesodDsl.Parser
import System.Environment
import System.Console.GetOpt
import YesodDsl.Generator
import Control.Monad
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Maybe (listToMaybe)
import Data.Generics.Uniplate.Data


data Flag = Version 
          | JsonPath String 
          | HsClient String
          | PureScriptPath String
          | PureScriptModulePrefix String
    deriving (Eq, Data, Typeable)
          
options :: [OptDescr Flag]
options = [
    Option [] ["json"] (ReqArg JsonPath "FILE") "translate DSL definition to JSON object",
    Option [] ["purescript"] (ReqArg PureScriptPath "FILE") "generate client-side PureScript module",
    Option [] ["purescript-module-prefix"] (ReqArg PureScriptModulePrefix "PREFIX") "add PREFIX to PureScript module name",
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
                            forM_ o (processFlag ast o)
                        Nothing -> return ()
        processFlag ast _ (JsonPath path)  = genJson path ast
        processFlag ast o (PureScriptPath path)  = genPureScript path ast (listToMaybe [ pfx | PureScriptModulePrefix pfx <- universeBi o ])
        processFlag ast _ (HsClient path)  = genHsClient path ast
        processFlag _ _ _ = return ()
