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
            dbEntities  = implInEntitys ifaces (dbEntities db)
        }

ifaceLookup :: [Iface] -> IfaceName -> Maybe Iface
ifaceLookup ifaces name =  find (\i -> name == ifaceName i) ifaces

implInEntitys :: [Iface] -> [Entity] -> [Entity]
implInEntitys ifaces entities = map (implInEntity ifaces) entities


entityError :: Entity -> String -> a
entityError e msg = error $ msg ++ " (" ++ entityPath e++ ")"
implInEntity :: [Iface] -> Entity -> Entity
implInEntity ifaces e 
    | null invalidIfaceNames = e {
        entityFields  = entityFields e ++ extraFields,
        entityUniques = entityUniques e ++ extraUniques
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
        extraUniques = concat $ map ifaceUniques validIfaces
        
    
