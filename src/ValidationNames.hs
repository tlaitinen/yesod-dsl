module ValidationNames (findNames, duplicateNameErrors) where
import AST

findNames :: Module -> [(String,[Location])]
findNames m = nameGroups
    where
        entityNames  = [(entityLoc e, entityName e) | e <- modEntities m ]
        entityFieldNames = [(entityLoc e, entityName e ++ "." ++ fieldName f)
                           | e <- modEntities m, f <- entityFields e ]

        classNames   = [(classLoc i, className i) | i <- modClasses m ]
        classFieldNames = [(classLoc i, className i ++ "." ++ fieldName f)
                           | i <- modClasses m, f <- classFields i ]

        enumNames    = [(enumLoc e, enumName e) | e <- modEnums m ]
        enumValueNames = [(enumLoc e, enumName e ++ "." ++ v) 
                         | e <- modEnums m, v <- enumValues e ]

        resNames = [(resLoc r, show $ resRoute r) | r <- modResources m ]
        resHandlerNames = [(resLoc r, show (resRoute r) ++ " " ++ show ht)
                          | r <- modResources m, 
                            (Handler ht _) <- resHandlers r ]
        handlerParams = [(resLoc r, show (resRoute r) ++ " " ++ show ht 
                           ++ " " ++ handlerParamName p)
                         | r <- modResources m,
                           (Handler ht ps) <- resHandlers r,
                           p <- ps ]

        allNames     = entityNames ++ entityFieldNames
                     ++ classNames ++ classFieldNames
                     ++ enumNames ++ enumValueNames
                     ++ resourceNames ++ resHandlerNames 
                     ++ handlerParams

        sameNameOrd (_,n1) (_,n2) = compare n1 n2
        sortedNames = sortBy sameNameOrd allNames

        sameName (_,n1) (_,n2) = n1 == n2
        groupedNames = groupBy sameName sortedNames

        factorName :: [(Location,String)] -> (String,[Location])
        factorName (all@((_,name):rest)) = (name, [l | (l,_) <- all ])
        nameGroups = map factorName groupedNames

duplicateNameErrors :: [(String,[Location])] -> String
duplicateNameErrors nameGroups 
    | not $ null duplicates = "Duplicate names:\n" ++ unlines $ map f duplicates
    | otherwise = ""
    where
        f (n, l) = n ++ " : " ++ show l
        duplicates = [ (n, locs) | (n, locs) <- nameGroups, length locs > 1 ]
