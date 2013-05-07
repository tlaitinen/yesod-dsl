module ClassImplementer (implementInterfaces) where
import Data.List
import DbTypes
import Data.Maybe
import CheckFieldNames

implementInterfaces :: DbModule -> DbModule
implementInterfaces db' = 
    let
        db = checkFieldNames db'
        ifaces = dbClasses db
    in 
        db {
            dbEntities  = [ implInEntity db ifaces e | e <- (dbEntities db) ]
        }

ifaceLookup :: [Class] -> ClassName -> Maybe Class
ifaceLookup ifaces name =  find (\i -> name == ifaceName i) ifaces


expandClassField :: DbModule -> Entity ->  Field -> [Field]
expandClassField db e f@(Field _ _ (EntityField iName)) 
    | not $ fieldOptional f = error $ show (entityLoc e) ++ ": non-maybe reference to interface not allowed"
    | otherwise = [ Field {
                        fieldOptional = True,
                        fieldName = fieldName f ++ entityName re,
                        fieldContent = EntityField (entityName re)

                    } | re <- dbEntities db, iName `elem` (entityImplements re) ]


expandClassRefFields :: DbModule -> Entity -> Field -> [Field]
expandClassRefFields db e f = expand (fieldContent f)
    where       
        expand (EntityField name) = if isJust (ifaceLookup (dbClasses db) name) 
                                        then expandClassField db e f 
                                        else [f]
        expand _ = [f]                           
            

entityError :: Entity -> String -> a
entityError e msg = error $ msg ++ " (" ++ entityPath e++ ")"
implInEntity :: DbModule -> [Class] -> Entity -> Entity
implInEntity db ifaces e 
    | null invalidClassNames = e {
        entityFields  = concatMap (expandClassRefFields db e) $ entityFields e ++ extraFields,
        entityUniques = entityUniques e ++ concatMap ifaceUniques validClasses
    }
    | otherwise        = entityError e $ "Invalid interfaces " 
                                        ++ show invalidClassNames
    where
        implements = entityImplements e
        invalidClassNames = [ name | name <- implements, 
                                 isNothing $ ifaceLookup ifaces name ]
        implementedClasses = map (ifaceLookup ifaces) implements
        validClasses = catMaybes implementedClasses
                                     
        extraFields = concat $ map ifaceFields validClasses
        
    
