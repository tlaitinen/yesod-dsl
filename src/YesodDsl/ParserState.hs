module YesodDsl.ParserState (ParserMonad, runParser, pushScope, popScope, declare, SymType(..)) where
import qualified Data.Map as Map
import Control.Monad.State.Lazy
import Control.Monad.Trans.Class
import YesodDsl.AST 
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
    psScopeId :: Int
}

initParserState :: ParserState
initParserState = ParserState {
    psSyms = Map.empty,
    psScopeId = 0
}
runParser :: ParserMonad a -> IO a
runParser m = evalStateT m initParserState

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
pError l e = lift $ putStrLn $ show l ++ ":" ++ e

declare :: Location -> String -> SymType -> ParserMonad ()
declare l n t = do
    ps <- get
    let sym = [Sym (psScopeId ps) l t] 
    case Map.lookup n (psSyms ps) of
        Just ((Sym s l' _):_) -> if s == psScopeId ps
            then pError l $ "Identifier '" ++ n 
                    ++ "' already declared in " ++ show l'
            else put $ ps { 
                psSyms = Map.adjust (sym++) n (psSyms ps)
            }
        Nothing -> put $ ps {
                psSyms = Map.insert n sym $ psSyms ps
            }
