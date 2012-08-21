module SyncFiles (syncFiles) where
import Prelude hiding (readFile)
import System.IO.Strict
import Control.Monad

    
syncFiles :: [(FilePath,String)] -> IO ()
syncFiles files = forM_ files (\(path,content) -> do
    oldContent <- catch (readFile path) (\_ -> return "")
    if content /= oldContent 
        then do
            putStrLn $ "Updating" ++ path
            writeFile path content
        else
            return ())
    
