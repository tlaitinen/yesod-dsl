module YesodDsl.ParserState (ParserMonad, initParserState, getParserState,
    getPath, getParsed,
    setParserState, runParser, pushScope, popScope, declare, SymType(..),
    ParserState, mkLoc, parseErrorCount, withSymbol, withSymbolNow,
    pError,
    hasReserved,
    requireClass, 
    requireEntity,
    requireEntityId,
    requireEntityField,
    requireField,
    requireFieldType,
    requireEnumValue,
    requireParam,
    setCurrentHandlerType,
    getCurrentHandlerType,
    requireHandlerType,
    validateExtractField,
    validateInsert,
    beginHandler,
    statement,
    lastStatement,
    addCheck,
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
import Data.Maybe
data SymType = SEnum EnumType
             | SClass Class
             | SEntity EntityName
             | SEntityId EntityName
             | SDefine Define
             | SField Field
             | SFieldType FieldType
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
        SEntityId _ -> "entity id"
        SDefine _  -> "define"
        SField _   -> "field"
        SFieldType _ -> "field type"
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

type PendingValidation = Syms -> ParserMonad ()

data ParserState = ParserState {
    psSyms        :: Syms,
    psScopeId     :: Int,
    psPath        :: FilePath,
    psParsed      :: [FilePath],
    psErrors      :: Int,
    psHandlerType :: Maybe HandlerType,
    psPendingValidations :: [[PendingValidation]],
    psEntityValidations :: [EntityValidation],
    psLastStatement :: Maybe (Location, String),
    psChecks      :: [(Location,String,FieldType)]
}

initParserState :: ParserState
initParserState = ParserState {
    psSyms = Map.empty,
    psScopeId = 0,
    psPath = "",
    psParsed = [],
    psErrors = 0,
    psHandlerType = Nothing,
    psPendingValidations = [],
    psEntityValidations = [],
    psLastStatement = Nothing,
    psChecks = []
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
    
requireHandlerType :: Location -> String -> (HandlerType -> Bool) -> ParserMonad ()
requireHandlerType l n f = do
    mht <- gets psHandlerType
    when (fmap f mht /= Just True) $ 
        pError l $ n ++ " not allowed " 
            ++ (fromMaybe "outside handler" $ 
                mht >>= \ht' -> return $ "in " ++ show ht' ++ " handler")
getCurrentHandlerType :: ParserMonad (Maybe HandlerType)
getCurrentHandlerType = gets psHandlerType    
setParserState :: ParserState -> ParserMonad ()
setParserState = put

runEntityValidation :: Module -> EntityValidation -> ParserMonad ()
runEntityValidation m (en,f) = 
    case L.find (\e -> entityName e == en) $ modEntities m of
        Just e -> f e
        Nothing -> return ()

validateExtractField :: Location -> String -> ParserMonad ()
validateExtractField l s = if s `elem` validFields
    then return ()
    else pError l $ "Unknown subfield '" ++ s ++ "' to extract"
    where 
        validFields = [
                "century", "day", "decade", "dow", "doy", "epoch", "hour",
                "isodow", "microseconds", "millennium", "milliseconds",
                "minute", "month", "quarter", "second", "timezone",
                "timezone_hour", "timezone_minute", "week", "year" 
            ]

validateInsert :: Location -> Entity -> Maybe [InputField] -> ParserMonad ()
validateInsert  l e (Just ifs) = do
    case [ fieldName f | f <- entityFields e, 
                         (not . fieldOptional) f, 
                         isNothing (fieldDefault f) ] 
                         L.\\ [ fn | (fn,_) <- ifs ] of
        fs@(_:_) -> pError l $ "Missing required fields without default value: " ++ (show fs)
        _ -> return ()
validateInsert _ _ _ = return ()

beginHandler :: ParserMonad ()
beginHandler = modify $ \ps -> ps {
        psLastStatement = Nothing
    }

statement :: Location -> String -> ParserMonad ()
statement l s= do
    mlast <- gets psLastStatement
    fromMaybe (return ()) (mlast >>= \(l',s') -> do
        return $ pError l $ "'" ++ s ++ "' not allowed after '" ++ s' ++ "' in "
            ++ show l')
lastStatement :: Location -> String -> ParserMonad ()
lastStatement l s = do
    statement l s
    modify $ \ps -> ps {
       psLastStatement = listToMaybe $ catMaybes [ 
           psLastStatement ps, Just (l,s) 
       ]
    }

addCheck :: Location -> String -> FieldType -> ParserMonad ()
addCheck l n ft = do
    checks <- gets psChecks
    forM_ [ c | c@(l',n',ft') <- checks, ft' /= ft, n' == n ] $ \(l',n',ft') ->
        pError l $ "'" ++ n ++ "' is for " ++ show ft ++ " but '" 
            ++ n' ++ "' in " ++ show l' ++ " is for " ++ show ft' 
    modify $ \ps -> ps { psChecks = psChecks ps ++ [(l,n,ft)] }
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
    psScopeId = psScopeId ps + 1,
    psPendingValidations = []:psPendingValidations ps 
}

popScope :: ParserMonad ()
popScope = do
    (vs:_) <- gets psPendingValidations 
    syms <- gets psSyms
    forM_ vs $ \v -> v syms
    modify $ \ps -> ps {
        psSyms = Map.filter (not . null) $ 
            Map.map (filter (\(Sym s _ _) -> s < psScopeId ps)) $ psSyms ps,
        psScopeId = psScopeId ps - 1,
        psPendingValidations = tail $ psPendingValidations ps
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
addEntityValidation ev = modify $ \ps -> ps {
        psEntityValidations = psEntityValidations ps ++ [ev]
    }

addPendingValidation :: (Syms -> ParserMonad ()) -> ParserMonad ()
addPendingValidation v = modify $ \ps -> let vs = psPendingValidations ps in ps {
        psPendingValidations = (v:(head vs)):tail vs
    }

hasReserved :: String -> ParserMonad Bool
hasReserved n = do
    syms <- gets psSyms
    return $ fromMaybe False $ 
        Map.lookup n syms >>= return . (not . null . (filter match))
    where
        match (Sym _ _ SReserved) = True
        match _ = False 


withSymbol' :: Syms -> Location -> String -> (Location -> Location -> SymType -> ParserMonad ()) -> ParserMonad ()
withSymbol' syms l n f = do
    case Map.lookup n syms >>= return . (filter notReserved) of
        Just ((Sym _ l' st):_) -> f l l' st
        _ -> pError l $ "Reference to an undeclared identifier '"
                ++ n ++ "'"
    where
        notReserved (Sym _ _ SReserved) = False
        notReserved _ = True        

withSymbolNow :: Location -> String -> (Location -> Location -> SymType -> ParserMonad()) -> ParserMonad ()
withSymbolNow l n f = do
    syms <- gets psSyms
    withSymbol' syms l n f

withSymbol :: Location -> String -> (Location -> Location -> SymType -> ParserMonad ()) -> ParserMonad ()
withSymbol l n f = addPendingValidation $ \syms -> withSymbol' syms l n f        

requireClass :: (Class -> ParserMonad ()) -> (Location -> Location -> SymType -> ParserMonad ())
requireClass f = f'
    where f' _ _ (SClass c) = f c
          f' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected class)"

requireEntity :: (Entity -> ParserMonad ()) -> (Location -> Location -> SymType -> ParserMonad ())
requireEntity f = f'
    where f' _ _ (SEntity en) = addEntityValidation (en, f)
          f' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected entity)"

requireEntityId :: (EntityName -> ParserMonad ()) -> (Location -> Location -> SymType -> ParserMonad ())
requireEntityId f = f'
    where f' _ _ (SEntityId en) = f en
          f' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected entity id)"
 

requireEntityField :: Location -> FieldName -> ((Entity,Field) -> ParserMonad ()) -> (Location -> Location -> SymType -> ParserMonad ())
requireEntityField l fn fun = fun'
    where fun' _ _ (SEntity en) = addEntityValidation (en, \e -> 
            case L.find (\f -> fieldName f == fn) $ entityFields e of
              Just f -> fun (e,f)
              Nothing -> pError l $ "Reference to undeclared field '" 
                                ++ fn ++ "' of entity '" ++ entityName e ++ "'")
          fun' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected entity)"
requireFieldType :: (FieldType -> ParserMonad()) -> (Location -> Location -> SymType -> ParserMonad ())
requireFieldType f = fun'
    where 
        fun' _ _ (SFieldType ft) = f ft
        fun' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected field type)"  
        

requireEnumValue :: Location -> FieldName -> (Location -> Location -> SymType -> ParserMonad ())
requireEnumValue l fn = fun'
    where 
        fun' _ _ (SEnum e) = 
            case L.find (\ev' -> ev' == fn) $ enumValues e of
                Just ev -> return ()
                Nothing -> pError l $ "Reference to undeclared enum value '"
                    ++ fn ++ "' of enum '" ++ enumName e ++ "'"
        fun' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected enum)"
          
requireParam :: (Location -> Location -> SymType -> ParserMonad ())
requireParam = fun'
    where
        fun' _ _ SParam = return ()
        fun' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected param)"
        


requireField:: (Field -> ParserMonad ()) -> (Location -> Location -> SymType -> ParserMonad ())
requireField f = f'
    where f' _ _ (SField sf) = f sf
          f' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected field)"
