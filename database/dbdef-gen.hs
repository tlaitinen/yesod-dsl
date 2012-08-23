import DbLexer
import DbParser
import System.Environment
import ModuleMerger
import NameFinder
import IfaceImplementer
import DbTypes
import Data.List
import Generator
import SyncFiles
import System.Directory

main = do
    [ path ] <- getArgs
    dbs <- parse path
    let merged    = mergeModules dbs
    let names     = findNames merged
    let impl      = implementInterfaces merged
    let generated = generateModels impl
    syncFiles generated
    exists <- doesFileExist "Model/Validation.hs"
    if not exists 
        then do writeFile "Model/Validation.hs" "module Model.Validation where"
        else return ()
