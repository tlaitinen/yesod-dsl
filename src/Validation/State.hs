
module Validation.State (validate) where

import AST
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.List as L
type Info = String



data VState = VState {
    stEnv :: Map.Map String [VId],
    stScope :: Int,
    stScopePath :: [String],
    stErrors :: [String]
}

initialState :: VState
initialState = VState Map.empty 0 [] []

data VId = VId Int VIdType deriving(Eq)
data VIdType = VEnum EnumType
         | VClass  Class
         | VEntity Entity
         | VField Field
         | VUnique Unique
         | VRoute Route
         | VHandler Handler
         | VReserved
         deriving (Eq,Show)

instance Ord VId where
    compare (VId s1 _) (VId s2 _) = compare s1 s2

type Validation = State VState ()



vError :: String -> Validation
vError err = modify $ \st -> st { stErrors = stErrors st ++ [err] }
    

pushScope :: String -> Validation
pushScope path = modify $ \st -> st { stScope = stScope st + 1,
                                 stScopePath = path:stScopePath st }

popScope :: Validation
popScope = modify $ \st -> let newScope = stScope st - 1 in st {
            stScope = newScope,
            stScopePath = tail $ stScopePath st,
            stEnv = Map.map (filter (\(VId s _) -> s <= newScope)) $ stEnv st
        }

declare :: Int -> String -> VIdType -> Validation
declare scope name id = do
    st <- get
    let e = stEnv st
    let newId = [VId scope id]
    case Map.lookup name e of
        Just ((VId s t):_) -> do
            if s == scope
                then vError $ "Identifier " ++ name 
                     ++ " already declared : " ++ show t
                else put $ st { stEnv = Map.adjust (L.sort . (newId++)) name e }
        Nothing -> put $ st { stEnv = Map.insert name newId e }
       
declareGlobal :: String -> VIdType -> Validation        
declareGlobal = declare 0

declareLocal :: String -> VIdType -> Validation
declareLocal name id = do
    scope <- gets stScope
    declare scope name id

withLookup :: String -> (VIdType -> Validation) -> Validation
withLookup name f = do
    env <- gets stEnv
    path <- gets stScopePath
    case Map.lookup name env of
        Just ((VId _ t):_) -> f t
        Nothing -> vError $ "Reference to an undeclared identifier '" 
                          ++ name ++ "' in " ++ show path

withLookupField :: String -> (Field -> Validation) -> Validation
withLookupField name f = do
    path <- gets stScopePath
    withLookup name $ \idt -> case idt of
        (VField t) -> f t
        _ -> vError $ "Reference to an incompatible type " ++ show idt 
                     ++ " in " ++ show path ++ " (expected field)"

withLookupEntity :: String -> (Entity -> Validation) -> Validation
withLookupEntity name f =do
    path <- gets stScopePath
    withLookup name $ \idt -> case idt of
        (VEntity t) -> f t
        _ -> vError $ "Reference to an incompatible type " ++ show idt 
                     ++ " by " ++ show path ++ " (expected entity)"

withLookupEnum :: String -> (EnumType -> Validation) -> Validation
withLookupEnum name f = do
    path <- gets stScopePath
    withLookup name $ \idt -> case idt of
        (VEnum t) -> f t
        _ -> vError $ "Reference to an incompatible type " ++ show idt 
                     ++ " by " ++ show path ++ " (expected enum)"


validate :: Module -> [String]
validate m = stErrors $ execState (validate' m) initialState

validate' :: Module -> Validation
validate' m = do
    forM_ (modEnums m) $ \e -> declareGlobal (enumName e) (VEnum e)
    forM_ (modClasses m) $ \c -> declareGlobal (className c) (VClass c)
    forM_ (modEntities m) $ \e -> declareGlobal (entityName e) (VEntity e)
    forM_ (modClasses m) vClass
    forM_ (modEntities m) vEntity
    forM_ (modRoutes m) vRoute
    return ()

vClass :: Class -> Validation
vClass c = do
    pushScope $ "class " ++ (className c) ++ " in " ++ (show (classLoc c))
    forM_ (classFields c) vField
    forM_ (classUniques c) $ vUnique (className c) 
    popScope

vEntity :: Entity -> Validation
vEntity e = do
    pushScope $ "entity " ++ (entityName e) ++ " in " ++ (show $ entityLoc e)
    forM_ (entityFields e) vField
    forM_ (entityUniques e) $ vUnique (entityName e)
    popScope


vField :: Field -> Validation
vField f = do
    declareLocal (fieldName f) (VField f)
    pushScope $ "field " ++ fieldName f
    case fieldContent f of
        EntityField en -> vEntityRef en
        EnumField en -> vEnumRef en
    popScope    

vUnique :: String -> Unique -> Validation
vUnique prefix u = do
    declareGlobal (prefix ++ uniqueName u) (VUnique u)
    pushScope $ "unique " ++ uniqueName u
    forM_ (uniqueFields u) $ \fn -> withLookupField fn $ \f -> return ()
    popScope

vRoute :: Route -> Validation
vRoute r = do
    declareGlobal (show $ routePath r) (VRoute r)
    pushScope $ "route " ++ (show $ routePath r) ++ " in "++ (show $ routeLoc r)
    forM_ (routeHandlers r) vHandler
    popScope
    
vHandler :: Handler -> Validation
vHandler h = do
    declareLocal (show $ handlerType h) (VHandler h)
    pushScope $ "handler " ++ (show $ handlerType h)
    forM_ (handlerParams h) vHandlerParam
    popScope

vHandlerParam :: HandlerParam -> Validation
vHandlerParam Public = declareLocal "public;" VReserved
vHandlerParam DefaultFilterSort = declareLocal "default-filter-sort;" VReserved
vHandlerParam (Select sq) = do
    declareLocal "select;" VReserved
    pushScope $ "select"
    let (en,vn) = sqFrom sq
    withLookupEntity en $ \e -> declareLocal vn (VEntity e)
    forM_ (sqJoins sq) vJoin
    case sqWhere sq of 
        Just e -> do    
            pushScope $ "where expression"
            vExpr e
            popScope
    forM_ (sqOrderBy sq) $ \(fr,_) -> vFieldRef fr
    popScope
vHandlerParam (IfFilter (vn,joins,e)) = do
    pushScope "if param"
    declareLocal vn VReserved
    forM_ joins vJoin
    vExpr e
    popScope
vHandlerParam (DeleteFrom en vn me) = do
    pushScope "delete from"
    withLookupEntity en $ \e -> declareLocal vn (VEntity e)
    case me of
        Just e -> vExpr e
        Nothing -> return ()
    popScope
vHandlerParam (Update en ifr mifs) = do
    pushScope "update"
    path <- gets stScopePath
    withLookupEntity en $ \e -> do
        case mifs of
            Just ifs -> forM_ ifs $ \(fn,_) -> 
                case L.find (\f -> fieldName f == fn) (entityFields e) of
                    Just f' -> return ()
                    Nothing -> vError $ "Reference to undeclared field '"
                                       ++ fn ++ "' in entity " ++ en
                                       ++ " in " ++ (show path)
            Nothing -> return ()
    popScope
vHandlerParam (Insert en mfs) = do
    pushScope "insert"
    path <- gets stScopePath
    withLookupEntity en $ \e -> do
        case mfs of
            Just ifs -> forM_ ifs $ \(fn,_) -> 
                case L.find (\f -> fieldName f == fn) (entityFields e) of
                    Just f' -> return ()
                    Nothing -> vError $ "Reference to undeclared field '"
                                       ++ fn ++ "' in entity " ++ en
                                       ++ " in " ++ (show path)
            Nothing -> return ()
    popScope

vJoin :: Join -> Validation
vJoin j = do
    path <- gets stScopePath
    withLookupEntity (joinEntity j) $ \e -> declareLocal (joinAlias j) 
                                                         (VEntity e)
    case joinExpr j of
        Just e -> do
            pushScope $ "join expression"
            vExpr e
            popScope
        Nothing -> if joinType j /= CrossJoin
            then vError $ "Missing join expression in " ++ show path
            else return () 
     
vEntityRef :: EntityName -> Validation
vEntityRef en = withLookupEntity en $ \e -> return ()

vEnumRef :: EnumName -> Validation
vEnumRef en = withLookupEnum en $ \e -> return ()

vExpr :: Expr -> Validation
vExpr (AndExpr e1 e2) = do
    vExpr e1
    vExpr e2
vExpr (OrExpr e1 e2) = do
    vExpr e1
    vExpr e2
vExpr (NotExpr e) = do
    vExpr e
vExpr loe@(ListOpExpr fr1 op fr2) = do
    vFieldRef fr1
    vFieldRef fr2
    case (fr1) of   
        FieldRefId _ -> return ()
        FieldRefNormal _ _ -> return ()
        _ -> vError $ "Unsupported left hand side operand in list expression : " ++ show  loe
    case (fr2) of
        FieldRefLocalParam -> return ()
        FieldRefSubQuery _ -> return ()
        _ -> vError $ "Unsupported right hand side operand in list expression : " ++ show loe
vExpr (BinOpExpr ve1 op ve2) = do
    vValExpr ve1
    vValExpr ve2
     
    return ()

vFieldRef :: FieldRef -> Validation
vFieldRef (FieldRefId vn) = vEntityRef vn 
vFieldRef (FieldRefNormal vn fn) = do
    path <- gets stScopePath
    withLookupEntity vn $ \e -> 
        case L.find (\f -> fieldName f == fn) $ entityFields e of
            Just f -> return ()
            Nothing -> vError $ "Entity " ++ entityName e ++ " referenced by "
                               ++ vn ++ "." ++ fn ++ " in " ++ (show path)
                               ++ " does not have the field " ++ fn
vFieldRef (FieldRefSubQuery sq) = do
    pushScope "sub-select"
    let (en,vn) = sqFrom sq
    withLookupEntity en $ \e -> declareLocal vn (VEntity e)
    forM_ (sqJoins sq) vJoin 
    case sqWhere sq of 
        Just e -> do    
            pushScope $ "where expression"
            vExpr e
            popScope
        Nothing -> return ()
    popScope

vValExpr :: ValExpr -> Validation
vValExpr ve = case ve of
    FieldExpr fr -> vFieldRef fr
    ConstExpr _ -> return ()
    ConcatExpr ve1 ve2 -> do
        vValExpr ve1
        vValExpr ve2
