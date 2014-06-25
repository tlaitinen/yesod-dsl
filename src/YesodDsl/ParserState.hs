module YesodDsl.ParserState (ParserMonad, initParserState, getParserState,
getPath, getParsed,
setParserState, runParser, pushScope, popScope, declare, SymType(..),
ParserState, mkLoc) where 
import qualified Data.Map as Map
import Control.Monad.State.Lazy
import Control.Monad.Trans.Class
import YesodDsl.AST 
import YesodDsl.Lexer 
import System.IO (FilePath)

data SymType = SEnum EnumType
             | SClass Class
             | SEntity Entity
             | SDefine Define
             | SField Field
             | SUnique Unique
             | SRoute Route
             | SHandler Handler
             | SParam
             | SForParam InputFieldRef
             | SReserved

data Sym = Sym Int Location SymType

type ParserMonad = StateT ParserState IO 

data ParserState = ParserState {
    psSyms    :: Map.Map String [Sym],
    psScopeId :: Int,
    psPath    :: FilePath,
    psParsed  :: [FilePath]
}

initParserState :: ParserState
initParserState = ParserState {
    psSyms = Map.empty,
    psScopeId = 0,
    psParsed = []
}
getParserState :: ParserMonad ParserState
getParserState = get

getPath :: ParserMonad FilePath
getPath = gets psPath

getParsed :: ParserMonad [FilePath]
getParsed = gets psParsed


setParserState :: ParserState -> ParserMonad ()
setParserState = put

runParser :: FilePath -> ParserState -> ParserMonad a -> IO (a,ParserState)
runParser path ps m = do
    (r,ps') <- runStateT m $ ps { psPath = path, psParsed = path:psParsed ps }
    return (r, ps' { psPath = psPath ps} )

pushScope :: ParserMonad ()
pushScope = modify $ \ps -> ps {
    psScopeId = psScopeId ps + 1
}

popScope :: ParserMonad ()
popScope = modify $ \ps -> ps {
    psSyms = Map.filter (not . null) $ 
        Map.map (filter (\(Sym s _ _) -> s < psScopeId ps)) $ psSyms ps,
    psScopeId = psScopeId ps - 1
}

pError :: Location -> String -> ParserMonad ()
pError l e = lift $ putStrLn $ show l ++ ": " ++ e

declare :: Location -> String -> SymType -> ParserMonad ()
declare l n t = do
    ps <- get
    let sym = [Sym (psScopeId ps) l t] 
    case Map.lookup n (psSyms ps) of
        Just ((Sym s l' _):_) -> if s == psScopeId ps
            then pError l $ "'" ++ n 
                    ++ "' already declared in " ++ show l'
            else put $ ps { 
                psSyms = Map.adjust (sym++) n (psSyms ps)
            }
        Nothing -> put $ ps {
                psSyms = Map.insert n sym $ psSyms ps
            }

mkLoc :: Token -> ParserMonad Location
mkLoc t = do
    path <- gets psPath
    return $ Loc path (tokenLineNum t) (tokenColNum t) 
