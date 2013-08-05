module Validation.State (validate) where

import AST
import Control.Monad.State
import qualified Data.Map as Map
type Info = String

data VState = VState {
    stEnv :: Map.Map String VId,
    stErrors :: [String]
}

initialState :: VState
initialState = VState Map.empty []

data VId = VEnumId EnumType
         | VClassId Class
         | VEntityId Entity
         | VUnique Unique
         deriving (Show)

             

type Validation = State VState ()

vError :: String -> Validation
vError err = modify $ \st -> st { stErrors = stErrors st ++ [err] }
    

declare :: String -> VId -> Validation
declare name id = do
    st <- get
    let e = stEnv st
    case Map.lookup name e of
        Just t -> vError $ "Identifier " ++ name 
                         ++ " already declared : " ++ show t
        Nothing -> put $ st { stEnv = Map.insert name id e }
            

validate :: Module -> [String]
validate m = stErrors $ execState (validate' m) initialState

validate' :: Module -> Validation
validate' m = do
    forM_ (modEnums m) vEnum
    forM_ (modClasses m) vClass
    return ()

vEnum :: EnumType -> Validation
vEnum e = declare (enumName e) (VEnumId e)

vClass :: Class -> Validation
vClass c = do
    declare (className c) (VClassId c)
    forM_ (classFields c) vField
    forM_ (classUniques c) $ vUnique (className c) (classFields c)

vField :: Field -> Validation
vField f = return ()

vUnique :: Unique -> String -> [Field] -> Validation
vUnique u prefix fs = do
    declare (prefix ++ uniqueName u) (VUniqueId u)
    forM_
    


    

