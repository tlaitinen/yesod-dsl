module YesodDsl.SyncFile where
import Prelude hiding (readFile, catch)
import System.IO (FilePath, writeFile)
import System.IO.Strict (readFile)
import Control.Exception (catch, SomeException)

syncFile :: FilePath -> String -> IO ()
syncFile path content = do
    oldContent <- catch (readFile path) ((\_ -> return "") :: SomeException -> IO String)
    if content /= oldContent
        then do
            putStrLn $ "Updating " ++ path
            writeFile path content
        else return ()

