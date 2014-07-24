module YesodDsl.ParserState (ParserMonad, initParserState, getParserState,
    getPath, getParsed,
    setParserState, runParser, pushScope, popScope, declare, SymType(..),
    ParserState, mkLoc, parseErrorCount, withSymbol, 
    requireClass, 
    requireEntity,
    requireEntityField,
    requireField,
    setCurrentHandlerType,
    unsetCurrentHandlerType,
    postValidation) where 
import qualified Data.Map as Map
import Control.Monad.State.Lazy
import Control.Monad.Trans.Class
import YesodDsl.AST 
import YesodDsl.Lexer 
import YesodDsl.ExpandMacros
import YesodDsl.ClassImplementer
import System.IO (FilePath)
import qualified Data.List as L
data SymType = SEnum EnumType
             | SClass Class
             | SEntity EntityName
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

type Syms = Map.Map String [Sym]

type EntityValidation = (EntityName, Entity -> ParserMonad ())

data ParserState = ParserState {
    psSyms        :: Syms,
    psScopeId     :: Int,
    psPath        :: FilePath,
    psParsed      :: [FilePath],
    psErrors      :: Int,
    psHandlerType :: Maybe HandlerType,
    psEntityValidations :: [EntityValidation]
}

initParserState :: ParserState
initParserState = ParserState {
    psSyms = Map.empty,
    psScopeId = 0,
    psPath = "",
    psParsed = [],
    psErrors = 0,
    psHandlerType = Nothing,
    psEntityValidations = []
}
getParserState :: ParserMonad ParserState
getParserState = get

getPath :: ParserMonad FilePath
getPath = gets psPath

getParsed :: ParserMonad [FilePath]
getParsed = gets psParsed

setCurrentHandlerType :: HandlerType -> ParserMonad ()
setCurrentHandlerType ht = modify $ \ps -> ps {
        psHandlerType = Just ht
    }
unsetCurrentHandlerType :: ParserMonad ()
unsetCurrentHandlerType = modify $ \ps -> ps {
        psHandlerType = Nothing
    }

setParserState :: ParserState -> ParserMonad ()
setParserState = put

runEntityValidation :: Module -> EntityValidation -> ParserMonad ()
runEntityValidation m (en,f) = 
    case L.find (\e -> entityName e == en) $ modEntities m of
        Just e -> f e
        Nothing -> return ()

postValidation :: Module -> ParserState -> IO Int
postValidation m ps = do
    ps' <- execStateT (mapM (runEntityValidation m) $
                         psEntityValidations ps) $ ps
    return $ psErrors ps'  


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
    return ()
    -- lift $ putStrLn $ show l ++ ": " ++ e
    -- modify $ \ps -> ps { psErrors = psErrors ps + 1 }

declare :: Location -> String -> SymType -> ParserMonad ()
declare l n t = do
    ps <- get
    let sym = [Sym (psScopeId ps) l t] 
    case Map.lookup n (psSyms ps) of
        Just ((Sym s l' _):_) -> if s == psScopeId ps
            then do
                pError l $ "'" ++ n 
                    ++ "' already declared in " ++ show l'
                return ()
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
  
addEntityValidation :: EntityValidation -> ParserMonad ()
addEntityValidation ev = do
    modify $ \ps -> ps {
        psEntityValidations = psEntityValidations ps ++ [ev]
    }


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

requireEntity :: (Entity -> ParserMonad ()) -> (Location -> Location -> SymType -> ParserMonad ())
requireEntity f = f'
    where f' _ _ (SEntity en) = addEntityValidation (en, f)
          f' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected entity)"

requireEntityField :: Location -> FieldName -> ((Entity,Field) -> ParserMonad ()) -> (Location -> Location -> SymType -> ParserMonad ())
requireEntityField l fn fun = fun'
    where fun' _ _ (SEntity en) = addEntityValidation (en, \e -> 
            case L.find (\f -> fieldName f == fn) $ entityFields e of
              Just f -> fun (e,f)
              Nothing -> pError l $ "Reference to undeclared field '" 
                                ++ fn ++ "' of entity '" ++ entityName e ++ "'")
          fun' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected entity)"



requireField:: (Field -> ParserMonad ()) -> (Location -> Location -> SymType -> ParserMonad ())
requireField f = f'
    where f' _ _ (SField sf) = f sf
          f' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected field)"
