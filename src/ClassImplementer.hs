module ClassImplementer (implementClasses) where
import Data.List
import AST
import Data.Maybe

implementClasses :: Module -> Module
implementClasses m = 
    let
        classes = modClasses m
    in 
        m {
            modEntities  = [ implInEntity m classes e | e <- (modEntities m) ]
        }

classLookup :: [Class] -> ClassName -> Maybe Class
classLookup classes name =  find (\i -> name == className i) classes


expandClassField :: Module -> Entity ->  Field -> [Field]
expandClassField m e f@(Field _ _ (EntityField iName)) 
    | not $ fieldOptional f = error $ show (entityLoc e) ++ ": non-maybe reference to class not allowed"
    | otherwise = [ mkField re | re <- modEntities m,  
                                 iName `elem` (entityInstances re) ]
    where mkField re = Field {
            fieldOptional = True,
            fieldName = lowerFirst (entityName re) ++ upperFirst (fieldName f),
            fieldContent = EntityField (entityName re)
        } 
expandClassField _ _ _ = []

expandClassRefFields :: Module -> Entity -> Field -> [Field]
expandClassRefFields m e f = expand (fieldContent f)
    where       
        expand (EntityField name) = case classLookup (modClasses m) name of
            Just _ -> expandClassField m e f
            Nothing -> [f]
        expand _ = [f]                           
            
entityError :: Entity -> String -> a
entityError e msg = error $ msg ++ " (" ++ entityName e ++ " in " ++ (show $ entityLoc e) ++ ")"

implInEntity :: Module -> [Class] -> Entity -> Entity
implInEntity m classes' e 
    | null invalidClassNames = e {
        entityFields  = concatMap (expandClassRefFields m e) $ entityFields e ++ extraFields,
        entityUniques = entityUniques e ++ (map (addEntityNameToUnique e) $ concatMap classUniques validClasses),
        entityChecks = entityChecks e 
    }
    | otherwise        = entityError e $ "Invalid classes " 
                                        ++ show invalidClassNames
    where
        classes = sortBy (\c1 c2 -> maybeCompare (elemIndex (className c1) instances) 
                                                 (elemIndex (className c2) instances))
                         classes'
        maybeCompare (Just a1) (Just a2) = compare a1 a2
        maybeCompare (Just _) Nothing = LT
        maybeCompare Nothing (Just _) = GT
        instances = entityInstances e
        invalidClassNames = [ name | name <- instances, 
                                 isNothing $ classLookup classes name ]
        instantiatedClasses = map (classLookup classes) instances
        validClasses = catMaybes instantiatedClasses
                                     
        extraFields = concatMap classFields validClasses
        addEntityNameToUnique e (Unique name fields) = Unique (entityName e ++ name) fields
        
    
