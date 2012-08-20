module IfaceImplementer (implementInterfaces) where
import Data.List
import DbTypes
import Data.Maybe

implementInterfaces :: DbModule -> DbModule
implementInterfaces db = 
    let
        ifaces = dbIfaces db
    in 
        db {
            dbEntities  = implInEntities ifaces (dbEntities db),
            dbRelations = implInRelations ifaces (dbRelations db)
        }

ifaceLookup :: [Iface] -> IfaceName -> Maybe Iface
ifaceLookup ifaces name =  find (\i -> name == ifaceName i) ifaces

implInEntities :: [Iface] -> [Entity] -> [Entity]
implInEntities ifaces entities = map (implInEntity ifaces) entities


implInRelations :: [Iface] -> [Relation] -> [Relation]
implInRelations ifaces rels = concatMap (implInRelation ifaces) rels

entityError :: Entity -> String -> a
entityError e msg = error $ msg ++ " (" ++ entityPath e++ ")"
implInEntity :: [Iface] -> Entity -> Entity
implInEntity ifaces e 
    | null invalidIfaceNames = e {
        entityFields  = entityFields e ++ extraFields,
        entityIndices = entityIndices e ++ extraIndices
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
        extraIndices = concat $ map ifaceIndices validIfaces
        
    
implInRelation :: [Iface] -> Relation -> [Relation]
implInRelation ifaces rel = rels
    where 
        getRef (RelField refName _) = Just refName
        getRef _ = Nothing
        refs = catMaybes $ map (getRef . fieldContent) (relFields rel)
        rels = []


