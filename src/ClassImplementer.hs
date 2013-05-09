module ClassImplementer (implementInterfaces) where
import Data.List
import AST
import Data.Maybe
import CheckFieldNames

implementInterfaces :: Module -> Module
implementInterfaces db' = 
    let
        db = checkFieldNames db'
        classes = dbClasses db
    in 
        db {
            dbEntities  = [ implInEntity db classes e | e <- (dbEntities db) ]
        }

classLookup :: [Class] -> ClassName -> Maybe Class
classLookup classes name =  find (\i -> name == className i) classes


expandClassField :: Module -> Entity ->  Field -> [Field]
expandClassField db e f@(Field _ _ (EntityField iName)) 
    | not $ fieldOptional f = error $ show (entityLoc e) ++ ": non-maybe reference to interface not allowed"
    | otherwise = [ Field {
                        fieldOptional = True,
                        fieldName = fieldName f ++ entityName re,
                        fieldContent = EntityField (entityName re)

                    } | re <- dbEntities db, iName `elem` (entityImplements re) ]


expandClassRefFields :: Module -> Entity -> Field -> [Field]
expandClassRefFields db e f = expand (fieldContent f)
    where       
        expand (EntityField name) = if isJust (classLookup (dbClasses db) name) 
                                        then expandClassField db e f 
                                        else [f]
        expand _ = [f]                           
            

entityError :: Entity -> String -> a
entityError e msg = error $ msg ++ " (" ++ entityPath e++ ")"
implInEntity :: Module -> [Class] -> Entity -> Entity
implInEntity db classes e 
    | null invalidClassNames = e {
        entityFields  = concatMap (expandClassRefFields db e) $ entityFields e ++ extraFields,
        entityUniques = entityUniques e ++ (map (addEntityName e) $ concatMap classUniques validClasses)
    }
    | otherwise        = entityError e $ "Invalid interfaces " 
                                        ++ show invalidClassNames
    where
        implements = entityImplements e
        invalidClassNames = [ name | name <- implements, 
                                 isNothing $ classLookup classes name ]
        implementedClasses = map (classLookup classes) implements
        validClasses = catMaybes implementedClasses
                                     
        extraFields = concat $ map classFields validClasses
        addEntityName e (Unique name fields) = Unique (entityName e ++ name) fields
        
    
