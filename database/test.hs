import DbLexer
import DbParser
import System.Environment
import ModuleMerger
import NameFinder
import IfaceImplementer
import DbTypes
import Data.List
main = do
    [ path ] <- getArgs
    dbs <- parse path
    print dbs
    let merged = mergeModules dbs
    print merged
    let names = findNames merged
    let impl = implementInterfaces merged
    print impl
    print names
    
