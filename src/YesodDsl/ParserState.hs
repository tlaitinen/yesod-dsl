module YesodDsl.ParserState (ParserMonad, initParserState, getParserState,
getPath, getParsed,
setParserState, runParser, pushScope, popScope, declare, SymType(..),
ParserState, mkLoc, parseErrorCount, withSymbol, requireClass, requireField) where 
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

instance Show SymType where
    show st = case st of
        SEnum _  -> "enum"
        SClass _   -> "class"
        SEntity _  -> "entity"
        SDefine _  -> "define"
        SField _   -> "field"
        SUnique _  -> "unique"
        SRoute _  -> "route"
        SHandler _ -> "handler"
        SParam   -> "param"
        SForParam _ -> "for-param"
        SReserved -> "reserved"

data Sym = Sym Int Location SymType

type ParserMonad = StateT ParserState IO 

data ParserState = ParserState {
    psSyms    :: Map.Map String [Sym],
    psScopeId :: Int,
    psPath    :: FilePath,
    psParsed  :: [FilePath],
    psErrors  :: Int
}

initParserState :: ParserState
initParserState = ParserState {
    psSyms = Map.empty,
    psScopeId = 0,
    psParsed = [],
    psErrors = 0
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
pError l e = do
    lift $ putStrLn $ show l ++ ": " ++ e
    modify $ \ps -> ps { psErrors = psErrors ps + 1 }

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
        _ -> put $ ps {
                psSyms = Map.insert n sym $ psSyms ps
            }

mkLoc :: Token -> ParserMonad Location
mkLoc t = do
    path <- gets psPath
    return $ Loc path (tokenLineNum t) (tokenColNum t) 
 
parseErrorCount :: ParserState -> Int
parseErrorCount = psErrors
  
withSymbol :: Location -> String -> (Location -> Location -> SymType -> ParserMonad ()) -> ParserMonad ()
withSymbol l n f = do
    syms <- gets psSyms
    case Map.lookup n syms >>= return . (filter notReserved) of
        Just ((Sym _ l' st):_) -> f l l' st
        _ -> pError l $ "Reference to an undeclared identifier '"
            ++ n ++ "'"
    where
        notReserved (Sym _ _ SReserved) = False
        notReserved _ = True        

requireClass :: (Class -> ParserMonad ()) -> (Location -> Location -> SymType -> ParserMonad ())
requireClass f = f'
    where f' _ _ (SClass c) = f c
          f' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected class)"


requireField:: (Field -> ParserMonad ()) -> (Location -> Location -> SymType -> ParserMonad ())
requireField f = f'
    where f' _ _ (SField sf) = f sf
          f' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected field)"
