
module Validation.State (validate) where

import AST
import Control.Monad.State
import qualified Data.Map as Map
import Data.List (sort)
type Info = String



data VState = VState {
    stEnv :: Map.Map String [VId],
    stScope :: Int,
    stErrors :: [String]
}

initialState :: VState
initialState = VState Map.empty 0 []

data VId = VId Int VIdType deriving(Eq)
data VIdType = VEnum EnumType
         | VClass  Class
         | VEntity Entity
         | VField Field
         | VUnique Unique
         deriving (Eq,Show)

instance Ord VId where
    compare (VId s1 _) (VId s2 _) = compare s1 s2

type Validation = State VState ()


class Show a => HasLoc a where


instance HasLoc Unique where

vError :: String -> Validation
vError err = modify $ \st -> st { stErrors = stErrors st ++ [err] }
    

pushScope :: Validation
pushScope = modify $ \st -> st { stScope = stScope st + 1 }

popScope :: Validation
popScope = modify $ \st -> let newScope = stScope st - 1 in st {
            stScope = newScope,
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
                else put $ st { stEnv = Map.adjust (sort . (newId++)) name e }
        Nothing -> put $ st { stEnv = Map.insert name newId e }
            
declareLocal :: String -> VIdType -> Validation
declareLocal name id = do
    scope <- gets stScope
    declare scope name id

withLookup :: HasLoc a => a -> String -> (VIdType -> Validation) -> Validation
withLookup o name f = do
    env <- gets stEnv
    case Map.lookup name env of
        Just ((VId _ t):_) -> f t
        Nothing -> vError $ "Reference to an undeclared identifier '" 
                          ++ name ++ "' by " ++ show o 

withLookupField :: HasLoc a => a -> String -> (Field -> Validation) -> Validation
withLookupField o name f = withLookup o name $ \idt -> case idt of
    (VField t) -> f t
    _ -> vError $ "Reference to an incompatible type " ++ show idt 
                 ++ " by " ++ show o

validate :: Module -> [String]
validate m = stErrors $ execState (validate' m) initialState

validate' :: Module -> Validation
validate' m = do
    forM_ (modEnums m) vEnum
    forM_ (modClasses m) vClass
    return ()

vEnum :: EnumType -> Validation
vEnum e = declare 0 (enumName e) (VEnum e)

vClass :: Class -> Validation
vClass c = do
    declare 0 (className c) (VClass c)
    pushScope
    forM_ (classFields c) vField
    forM_ (classUniques c) $ vUnique (className c) 
    popScope

vField :: Field -> Validation
vField f = declareLocal (fieldName f) (VField f)

vUnique :: String -> Unique -> Validation
vUnique prefix u = do
    declare 0 (prefix ++ uniqueName u) (VUnique u)
    forM_ (uniqueFields u) $ vFieldRef u

vFieldRef :: HasLoc a => a -> FieldName -> Validation
vFieldRef o fn = withLookupField o fn $ \f -> return ()

    

