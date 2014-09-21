module YesodDsl.ClassImplementer (implementClasses) where
import Data.List
import YesodDsl.AST
import Data.Maybe
implementClasses :: Module -> Module
implementClasses m = m {
        modEntities  = [ implInEntity m (modClasses m) e | e <- modEntities m ]
    }

classLookup :: [Class] -> ClassName -> Maybe Class
classLookup classes name =  find (\i -> name == className i) classes


expandClassField :: Module -> Entity ->  Field -> [Field]
expandClassField m e f@(Field _ _ internal _ (EntityField iName)) 
    | not $ fieldOptional f = error $ show (entityLoc e) ++ ": non-maybe reference to class not allowed"
    | otherwise = [ mkField re | re <- modEntities m,  
                                 iName `elem` (entityInstances re) ]
    where mkField re = Field {
            fieldLoc = fieldLoc f,
            fieldOptional = True,
            fieldInternal = internal,
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
            
implInEntity :: Module -> [Class] -> Entity -> Entity
implInEntity m classes' e = e { 
        entityFields  = concatMap (expandClassRefFields m e) $ 
                            entityFields e ++ extraFields,
        entityClassFields = filter isClassField $ entityFields e,
        entityUniques = entityUniques e ++ (map (addEntityNameToUnique e) $ concatMap classUniques validClasses)
    }
    where
        instances = entityInstances e
        classes = sortBy (\c1 c2 -> maybeCompare (elemIndex (className c1) instances) 
                                                 (elemIndex (className c2) instances))
                         classes'
        maybeCompare (Just a1) (Just a2) = compare a1 a2
        maybeCompare (Just _) Nothing = LT
        maybeCompare Nothing (Just _) = GT
        maybeCompare Nothing Nothing = EQ
        validClasses = mapMaybe (classLookup classes) $ entityInstances e
        extraFields = concatMap classFields validClasses
        isClassField (Field _ _ _ _ (EntityField iName)) = iName `elem` (map className $ modClasses m)
        isClassField _ = False
        addEntityNameToUnique e (Unique name fields) = Unique (entityName e ++ name) fields
        
    
