module CheckFieldNames (checkFieldNames) where
import DbTypes

checkFieldNames :: DbModule -> DbModule
checkFieldNames db
    | null unknownFields = db
    | otherwise = error $ "Unknown fields:\n" 
                        ++ (unlines $ map formatField unknownFields)
    where
        ifaces = dbIfaces db
        entitys   = dbEntities db

        allFields :: [(String, Location, String)]
        allFields = concatMap getIfaceFields ifaces ++ concatMap getEntityFields entitys
        allNames = map ifaceName ifaces ++ map entityName entitys

        getIfaceFields iface = [ (ifaceName iface, ifaceLoc iface,
                                  getRefName f) | f <- ifaceFields iface,
                                  isRefField f]
        getEntityFields entity = [ (entityName entity, entityLoc entity, getRefName f) | 
                             f <- entityFields entity, isRefField f ]
        isRefField (Field _ _ (EntityField _)) = True
        isRefField _ = False
        getRefName (Field _ _ (EntityField name)) = name
        getRefName _ = ""
        unknownFields = [ (n,l,f) | (n,l,f) <- allFields,
                              not (f `elem` allNames)]
        formatField (n,l,f) = "    "++ n ++ "." ++ f ++ " " ++ show l


