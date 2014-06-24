module YesodDsl.Parser.Test where

import YesodDsl.AST
import YesodDsl.Parser.Types
import YesodDsl.Parser.Tokens
import YesodDsl.Parser.Indent
import YesodDsl.Parser.Field
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec
import Text.Parsec.Pos (newPos)
import Control.Monad.Trans.Class (lift)
import Control.Monad (forM_)

moduleNameParser :: YdslParser String
moduleNameParser = do
    stringAndSpace "module"
    x <- upperCaseId
    spaces'
    return x
    
importParser :: YdslParser String
importParser = do
    stringAndSpace "import"
    x <- quotedString
    spaces'
    return x


classDef :: YdslParser Class
classDef = withPos $ do
    pos <- getPosition
    stringAndSpace "class"
    n <- upperCaseId
    spaces'
    defs <- option [] $ indented >> (withPos $ many $ 
            (fmap ClassField fieldDef)
            <|> (fmap ClassUnique uniqueDef))
    return $ Class pos n defs 

parseClassName :: YdslParser ClassName
parseClassName = upperCaseId -- TODO: validate

instanceDef :: YdslParser [ClassName]
instanceDef = withPos $ do
    stringAndSpace "instance"
    stringAndSpace "of"
    spaces' 
    c <- parseClassName
    cs <- many $ try $ same >> spaces' >> comma >> spaces' >> parseClassName
    return $ c:cs
   
derivingDef :: YdslParser [ClassName]
derivingDef = withPos $ do
    stringAndSpace "deriving"
    spaces'
    c <- upperCaseId
    cs <- many $ try $ same >> spaces' >> comma >> spaces' >> upperCaseId
    return $ c:cs
 
entityDef :: YdslParser Entity
entityDef = withPos $ do
    pos <- getPosition
    try $ stringAndSpace "entity"
    n <- upperCaseId
    spaces'
    indented
    withPos $ do
        instances <- option [] instanceDef
        defs <- many $
                (fmap EntityDefField fieldDef)
                <|> (fmap EntityDefUnique uniqueDef)
                <|> (fmap EntityDefCheck checkDef)
        derivs <- option [] derivingDef
        return $ Entity pos n instances defs derivs

enumDef :: YdslParser EnumType
enumDef = withPos $ do
    pos <- getPosition
    try $ stringAndSpace "enum"
    n <- upperCaseId
    spaces'
    same
    char '='
    spaces'
    indented
    withPos $ do
        v <- upperCaseId
        vs <- many $ try $ indented >> spaces' >> char '|' >> spaces'
            >> indented >> upperCaseId
        return $ EnumType pos n (v:vs)

modDef :: YdslParser ModDef
modDef = (fmap ModClass classDef)
       <|> (fmap ModEntity entityDef)
       <|> (fmap ModEnum enumDef)

parseAndAddModDef :: YdslParser ()
parseAndAddModDef = do
    md <- modDef
    modifyState $ \env -> env {
        envDefs = envDefs env ++ [md]
    }
subModule :: YdslParser ()
subModule = do
    path <- importParser
    env <- getState
    if path `elem` envFiles env
        then return ()
        else do 
            input <- lift $ lift $ readFile path
            oldInput <- getInput
            oldPos <- getPosition
            setInput input
            setPosition $ newPos path 1 1
            many parseAndAddModDef
            modifyState $ \env' -> env' {
                    envFiles = path : envFiles env'
                }
            setInput oldInput
            setPosition oldPos
moduleParser :: YdslParser Module
moduleParser = do
    mName <- moduleNameParser 
    many subModule
    many parseAndAddModDef
    env <- getState 
    eof
    return $ Module mName (envDefs env)

parseFile :: FilePath -> IO (Either ParseError Module)
parseFile path = do
    input <- readFile path
    runIndent path $ runParserT moduleParser (initEnv path) path input

main = do
    m <- parseFile "test.ydsl"
    print m
