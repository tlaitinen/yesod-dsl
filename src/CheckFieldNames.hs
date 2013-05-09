module CheckFieldNames (checkFieldNames) where
import DbTypes

checkFieldNames :: DbModule -> DbModule
checkFieldNames db
    | null unknownFields = db
    | otherwise = error $ "Unknown fields:\n" 
                        ++ (unlines $ map formatField unknownFields)
    where
        classes = dbClasses db
        entitys   = dbEntities db

        allFields :: [(String, Location, String)]
        allFields = concatMap getClassFields classes ++ concatMap getEntityFields entitys
        allNames = map className classes ++ map entityName entitys

        getClassFields classDef = [ (className classDef, classLoc classDef,
                                  getRefName f) | f <- classFields classDef,
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


