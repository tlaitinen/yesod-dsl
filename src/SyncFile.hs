module SyncFile where
import Prelude hiding (readFile)
import System.IO (FilePath, writeFile)
import System.IO.Strict (readFile)

syncFile :: FilePath -> String -> IO ()
syncFile path content = do
    oldContent <- catch (readFile path) (\_ -> return "")
    if content /= oldContent
        then do
            putStrLn $ "Updating " ++ path
            writeFile path content
        else return ()

