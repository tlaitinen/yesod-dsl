module IfaceImplementer (implementInterfaces) where
import Data.List
import DbTypes
import Data.Maybe
import CheckFieldNames

implementInterfaces :: DbModule -> DbModule
implementInterfaces db' = 
    let
        db = checkFieldNames db'
        ifaces = dbIfaces db
    in 
        db {
            dbEntities  = [ implInEntity db ifaces e | e <- (dbEntities db) ]
        }

ifaceLookup :: [Iface] -> IfaceName -> Maybe Iface
ifaceLookup ifaces name =  find (\i -> name == ifaceName i) ifaces


expandIfaceField :: DbModule -> Entity ->  Field -> [Field]
expandIfaceField db e f@(Field _ _ (EntityField iName)) 
    | not $ fieldOptional f = error $ show (entityLoc e) ++ ": non-maybe reference to interface not allowed"
    | otherwise = [ Field {
                        fieldOptional = True,
                        fieldName = fieldName f ++ entityName re,
                        fieldContent = EntityField (entityName re)

                    } | re <- dbEntities db, iName `elem` (entityImplements re) ]


expandIfaceRefFields :: DbModule -> Entity -> Field -> [Field]
expandIfaceRefFields db e f = expand (fieldContent f)
    where       
        expand (EntityField name) = if isJust (ifaceLookup (dbIfaces db) name) 
                                        then expandIfaceField db e f 
                                        else [f]
        expand _ = [f]                           
            

entityError :: Entity -> String -> a
entityError e msg = error $ msg ++ " (" ++ entityPath e++ ")"
implInEntity :: DbModule -> [Iface] -> Entity -> Entity
implInEntity db ifaces e 
    | null invalidIfaceNames = e {
        entityFields  = concatMap (expandIfaceRefFields db e) $ entityFields e ++ extraFields
    }
    | otherwise        = entityError e $ "Invalid interfaces " 
                                        ++ show invalidIfaceNames
    where
        implements = entityImplements e
        invalidIfaceNames = [ name | name <- implements, 
                                 isNothing $ ifaceLookup ifaces name ]
        implementedIfaces = map (ifaceLookup ifaces) implements
        validIfaces = catMaybes implementedIfaces
                                     
        extraFields = concat $ map ifaceFields validIfaces
        
    
