{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module YesodDsl.ParserState (ParserMonad, initParserState, getParserState,
    getPath, getParsed,
    setParserState, runParser, pushScope, popScope, 
    declare, declareClassInstances, declareGlobal, SymType(..),
    addClassInstances,
    ParserState, mkLoc, parseErrorCount, withSymbol, withGlobalSymbol, withSymbolNow,
    pError,
    hasReserved,
    getEntitySymbol,
    symbolMatches,
    requireClass, 
    requireEnumName,
    requireEntity,
    requireEntityResult,
    requireEntityOrClass,
    requireEntityId,
    requireEntityField,
    requireEntityFieldSelectedOrResult,
    requireField,
    requireEnum,
    requireEnumValue,
    requireParam,
    requireFunction,
    setCurrentHandlerType,
    getCurrentHandlerType,
    requireHandlerType,
    validateExtractField,
    validateInsert,
    beginHandler,
    statement,
    lastStatement,
    postValidation) where
import qualified Data.Map as Map
import Control.Monad.State.Lazy
import YesodDsl.AST 
import YesodDsl.Lexer 
import qualified Data.List as L
import Data.Maybe
import System.IO

data SymType = SEnum EnumType
             | SEnumName EnumName
             | SClass Class
             | SEntity EntityName
             | SEntityResult EntityName
             | SEntityId EntityName
             | SField Field
             | SFieldType FieldType
             | SUnique Unique
             | SRoute Route
             | SHandler Handler
             | SParam
             | SForParam FieldRef
             | SReserved
             | SFunction

instance Show SymType where
    show st = case st of
        SEnum _  -> "enum"
        SEnumName _ -> "enum name"
        SClass _   -> "class"
        SEntity _  -> "entity"
        SEntityResult _ -> "get-entity"
        SEntityId _ -> "entity id"
        SField _   -> "field"
        SFieldType _ -> "field type"
        SUnique _  -> "unique"
        SRoute _  -> "route"
        SHandler _ -> "handler"
        SParam   -> "param"
        SForParam _ -> "for-param"
        SReserved -> "reserved"
        SFunction -> "function"

data Sym = Sym Int Location SymType deriving (Show)

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
    psChecks      :: [(Location,String,FieldType)],
    psClassInstances :: Map.Map ClassName [EntityName]
} deriving (Show)

instance Show (Syms -> ParserMonad ()) where
    show _ = "<pendingvalidation>"

instance Show (Entity -> ParserMonad ()) where
    show _ = "<entityvalidation>"
initParserState :: ParserState
initParserState = ParserState {
    psSyms = Map.fromList [
            ("ClassInstance", [ Sym 0 (Loc "<builtin>" 0 0) (SEntity "ClassInstance") ])
        ],
    psScopeId = 0,
    psPath = "",
    psParsed = [],
    psErrors = 0,
    psHandlerType = Nothing,
    psPendingValidations = [],
    psEntityValidations = [],
    psLastStatement = Nothing,
    psChecks = [],
    psClassInstances = Map.empty
}
getParserState :: ParserMonad ParserState
getParserState = get

getPath :: ParserMonad FilePath
getPath = gets psPath

getParsed :: ParserMonad [FilePath]
getParsed = gets psParsed

addClassInstances :: EntityName -> [ClassName] -> ParserMonad ()
addClassInstances en cns = modify $ \ps -> ps {
        psClassInstances = Map.unionWith (++) (psClassInstances ps) m
    }
    where 
        m = Map.fromListWith (++) [ (cn,[en]) | cn <- cns ]

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
runEntityValidation m (en,f) = do
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

validateInsert :: Location -> Entity -> Maybe (Maybe VariableName, [FieldRefMapping]) -> ParserMonad ()
validateInsert  l e (Just (Nothing, ifs)) = do
    case [ fieldName f | f <- entityFields e, 
                         (not . fieldOptional) f, 
                         isNothing (fieldDefault f) ] 
                         L.\\ [ fn | (fn,_,_) <- ifs ] of
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

postValidation :: Module -> ParserState -> IO Int
postValidation m ps = do
    ps' <- execStateT f ps
    return $ psErrors ps'  
    where
        f = do
            forM_ (psEntityValidations ps) (runEntityValidation m)
            when (isNothing $ modName m) $ globalError "Missing top-level module name"

runParser :: FilePath -> ParserState -> ParserMonad a -> IO (a,ParserState)
runParser path ps m = do
    (r,ps') <- runStateT m $ ps { psPath = path, psParsed = path:psParsed ps }
    return (r, ps' { psPath = psPath ps} )

pushScope :: ParserMonad ()
pushScope = do
    modify $ \ps -> ps {
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
    lift $ hPutStrLn stderr $ show l ++ ": " ++ e
    modify $ \ps -> ps { psErrors = psErrors ps + 1 }
globalError :: String -> ParserMonad ()
globalError e = do
    lift $ hPutStrLn stderr e
    modify $ \ps -> ps { psErrors = psErrors ps + 1 }

declare :: Location -> String -> SymType -> ParserMonad ()
declare l n t = declare' False l n t

declareClassInstances :: ClassName -> Location -> String -> ParserMonad ()
declareClassInstances cn l n = do
    ps <- get
    forM_ (Map.findWithDefault [] cn $ psClassInstances ps) $ \en -> do
        declare l (n ++ "_" ++ en) (SEntity en)

declareGlobal :: Location -> String -> SymType -> ParserMonad ()
declareGlobal l n t = declare' True l n t

declare' :: Bool -> Location -> String -> SymType -> ParserMonad ()
declare' global l n t = do
    ps <- get
    let scopeId = if global then 0 else psScopeId ps
        sym = [Sym scopeId l t ]
    case Map.lookup n (psSyms ps) of
        Just ((Sym s l' _):_) -> if s == scopeId
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
addGlobalPendingValidation :: (Syms -> ParserMonad ()) -> ParserMonad ()
addGlobalPendingValidation v = modify $ \ps -> let vs = psPendingValidations ps in ps {
        psPendingValidations = init vs ++ [v : last vs]
    }


hasReserved :: String -> ParserMonad Bool
hasReserved n = do
    syms <- gets psSyms
    return $ fromMaybe False $ 
        Map.lookup n syms >>= return . (not . null . (filter match))
    where
        match (Sym _ _ SReserved) = True
        match _ = False 


withSymbol' :: Syms -> a -> Location -> String -> (Location -> Location -> SymType -> ParserMonad a) -> ParserMonad a
withSymbol' syms d l n f = do
    case Map.lookup n syms >>= return . (filter notReserved) of
        Just ((Sym _ l' st):_) -> f l l' st
        _ -> pError l ("Reference to an undeclared identifier '"
                ++ n ++ "'") >> return d
notReserved :: Sym -> Bool                
notReserved (Sym _ _ SReserved) = False
notReserved _ = True        

withSymbolNow :: a -> Location -> String -> (Location -> Location -> SymType -> ParserMonad a) -> ParserMonad a
withSymbolNow d l n f = do
    syms <- gets psSyms
    withSymbol' syms d l n f

withSymbol :: Location -> String -> (Location -> Location -> SymType -> ParserMonad ()) -> ParserMonad ()
withSymbol l n f = addPendingValidation $ \syms -> void $ withSymbol' syms () l n f    



withGlobalSymbol :: Location -> String -> (Location -> Location -> SymType -> ParserMonad ()) -> ParserMonad ()
withGlobalSymbol l n f = addGlobalPendingValidation $ \syms -> void $ withSymbol' syms () l n f    




getEntitySymbol :: Location -> Location -> SymType -> ParserMonad (Maybe EntityName)
getEntitySymbol _ _ (SEntity en) = return $ Just en
getEntitySymbol _ _ _ = return Nothing


symbolMatches :: String -> (SymType -> Bool) -> ParserMonad Bool
symbolMatches n f = do
    syms <- gets psSyms
    case Map.lookup n syms >>= return . (filter notReserved) of
        Just ((Sym _ _ st):_) -> return $ f st
        _ -> return False
 

requireClass :: (Class -> ParserMonad ()) -> (Location -> Location -> SymType -> ParserMonad ())
requireClass f = f'
    where f' _ _ (SClass c) = f c
          f' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected class)"

requireEntity :: (Entity -> ParserMonad ()) -> (Location -> Location -> SymType -> ParserMonad ())
requireEntity f = f'
    where f' _ _ (SEntity en) = addEntityValidation (en, f)
          f' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected entity)"


requireEntityResult :: (Entity -> ParserMonad ()) -> (Location -> Location -> SymType -> ParserMonad ())
requireEntityResult f = f'
    where f' _ _ (SEntityResult en) = addEntityValidation (en, f)
          f' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected get-entity)"


requireEntityOrClass :: (Location -> Location -> SymType -> ParserMonad ())
requireEntityOrClass = f'
    where f' _ _ (SEntity _) = return ()
          f' _ _ (SClass _) = return ()
          f' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected entity or class)"


requireEntityId :: (EntityName -> ParserMonad (Maybe a)) -> (Location -> Location -> SymType -> ParserMonad (Maybe a))
requireEntityId f = f'
    where f' _ _ (SEntityId en) = f en
          f' l1 l2 st = pError l1 ("Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected entity id)") >> return Nothing

requireEnumName :: (EntityName -> ParserMonad (Maybe a)) -> (Location -> Location -> SymType -> ParserMonad (Maybe a))
requireEnumName f = f'
    where f' _ _ (SEnumName en) = f en
          f' l1 l2 st = pError l1 ("Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected enum name)") >> return Nothing



requireEntityField :: Location -> FieldName -> ((Entity,Field) -> ParserMonad ()) -> (Location -> Location -> SymType -> ParserMonad ())
requireEntityField l fn fun = fun'
    where fun' _ _ (SEntity en) = addEntityValidation (en, \e -> 
            case L.find (\f -> fieldName f == fn) $ entityFields e ++ entityClassFields e of
              Just f -> fun (e,f)
              Nothing -> pError l ("Reference to undeclared field '" 
                            ++ fn ++ "' of entity '" ++ entityName e ++ "'"))
          fun' l1 l2 st = pError l1 ( "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected entity)") 
requireEntityFieldSelectedOrResult :: Location -> FieldName -> (Location -> Location -> SymType -> ParserMonad ())
requireEntityFieldSelectedOrResult l fn = fun'
    where 
        fun' _ _ (SEntity en) = validate en
        fun' _ _ (SEntityResult en) = validate en
        fun' l1 l2 st = pError l1 ( "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected entity or get-entity)")  

        validate en = addEntityValidation (en, \e -> 
            case L.find (\f -> fieldName f == fn) $ entityFields e ++ entityClassFields e of
              Just _ -> return ()
              Nothing -> pError l ("Reference to undeclared field '" 
                            ++ fn ++ "' of entity '" ++ entityName e ++ "'"))
         

       
requireEnum :: Location -> Location -> SymType -> ParserMonad ()
requireEnum = fun'
    where 
        fun' _ _ (SEnum e) = return ()
        fun' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected enum)"
 
requireEnumValue :: Location -> EnumValue -> (Location -> Location -> SymType -> ParserMonad ())
requireEnumValue l ev = fun'
    where 
        fun' _ _ (SEnum e) = 
            case L.find (\ev' -> ev' == ev) $ enumValues e of
                Just _ -> return ()
                Nothing -> pError l $ "Reference to undeclared enum value '"
                    ++ ev ++ "' of enum '" ++ enumName e ++ "'"
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

requireFunction :: (Location -> Location -> SymType -> ParserMonad ())
requireFunction = fun'
    where 
        fun' _ _ SFunction = return ()
        fun' l1 l2 st = pError l1 $ "Reference to " ++ show st ++ " declared in " ++ show l2 ++ " (expected function)"  
 
