import DbLexer
import DbParser
import System.Environment
import ModuleMerger
import NameFinder
import CheckServices
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
    let impl      = (checkServices . implementInterfaces) merged
    let generated = generateModels impl
    syncFiles generated
    exists <- doesFileExist "Model/ValidationFunctions.hs"
    if not exists 
        then do writeFile "Model/ValidationFunctions.hs" "module Model.ValidationFunctions where\nimport Import"
        else return ()
