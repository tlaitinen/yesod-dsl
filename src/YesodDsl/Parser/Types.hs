module YesodDsl.Parser.Types where

import YesodDsl.AST
import YesodDsl.Parser.Indent

type SymName = String




data SymContent = SymEntity EntityName
data SymScope = SymScope
data Sym = Sym {
    symName    :: String,
    symScope   :: SymScope,
    symContent :: SymContent
}


data Env = Env {
    envSymbols :: [Sym],
    envDefs    :: [ModDef],
    envFiles   :: [FilePath]
}


type YdslParser a = IndentParserT String Env IO a

initEnv :: FilePath -> Env
initEnv path = Env {
    envSymbols = [],
    envDefs    = [],
    envFiles   = [path]
}

