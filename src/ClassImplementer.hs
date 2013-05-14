module ClassImplementer (implementClasses) where
import Data.List
import AST
import Data.Maybe

entityPath :: Entity -> String
entityPath e = entityName e ++ " in " ++ show (entityLoc e)

implementClasses :: Module -> Module
implementClasses mod = 
    let
        classes = modClasses mod
    in 
        mod {
            modEntities  = [ implInEntity mod classes e | e <- (modEntities mod) ]
        }

classLookup :: [Class] -> ClassName -> Maybe Class
classLookup classes name =  find (\i -> name == className i) classes


expandClassField :: Module -> Entity ->  Field -> [Field]
expandClassField mod e f@(Field _ _ (EntityField iName)) 
    | not $ fieldOptional f = error $ show (entityLoc e) ++ ": non-maybe reference to class not allowed"
    | otherwise = [ Field {
                        fieldOptional = True,
                        fieldName = fieldName f ++ entityName re,
                        fieldContent = EntityField (entityName re)

                    } | re <- modEntities mod, iName `elem` (entityInstances re) ]


expandClassRefFields :: Module -> Entity -> Field -> [Field]
expandClassRefFields mod e f = expand (fieldContent f)
    where       
        expand (EntityField name) = if isJust (classLookup (modClasses mod) name) 
                                        then expandClassField mod e f 
                                        else [f]
        expand _ = [f]                           
            

entityError :: Entity -> String -> a
entityError e msg = error $ msg ++ " (" ++ entityPath e++ ")"
implInEntity :: Module -> [Class] -> Entity -> Entity
implInEntity mod classes e 
    | null invalidClassNames = e {
        entityFields  = concatMap (expandClassRefFields mod e) $ entityFields e ++ extraFields,
        entityUniques = entityUniques e ++ (map (addEntityNameToUnique e) $ concatMap classUniques validClasses),
        entityChecks = entityChecks e ++ extraChecks e
    }
    | otherwise        = entityError e $ "Invalid classes " 
                                        ++ show invalidClassNames
    where
        instances = entityInstances e
        invalidClassNames = [ name | name <- instances, 
                                 isNothing $ classLookup classes name ]
        instantiatedClasses = map (classLookup classes) instances
        validClasses = catMaybes instantiatedClasses
                                     
        extraFields = concatMap classFields validClasses
        addEntityNameToUnique e (Unique name fields) = Unique (entityName e ++ name) fields
        addEntityNameToCheck e (Check func fields) = Check ((lowerFirst . entityName) e ++ upperFirst func) fields
        extraChecks e = map (addEntityNameToCheck e) $ concatMap classChecks validClasses
        
    
