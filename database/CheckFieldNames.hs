module CheckFieldNames (checkFieldNames) where
import DbTypes

checkFieldNames :: DbModule -> DbModule
checkFieldNames db
    | null unknownFields = db
    | otherwise = error $ "Unknown fields:\n" 
                        ++ (unlines $ map formatField unknownFields)
    where
        ifaces = dbIfaces db
        docs   = dbDocs db

        allFields :: [(String, Location, String)]
        allFields = concatMap getIfaceFields ifaces ++ concatMap getDocFields docs
        allNames = map ifaceName ifaces ++ map docName docs

        getIfaceFields iface = [ (ifaceName iface, ifaceLoc iface,
                                  getRefName f) | f <- ifaceFields iface,
                                  isRefField f]
        getDocFields doc = [ (docName doc, docLoc doc, getRefName f) | 
                             f <- docFields doc, isRefField f ]
        isRefField (Field _ _ (DocField _ _ _)) = True
        isRefField _ = False
        getRefName (Field _ _ (DocField _ _ name)) = name
        getRefName _ = ""
        unknownFields = [ (n,l,f) | (n,l,f) <- allFields,
                              not (f `elem` allNames)]
        formatField (n,l,f) = "    "++ n ++ "." ++ f ++ " " ++ show l


