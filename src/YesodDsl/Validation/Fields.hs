module YesodDsl.Validation.Fields (fieldErrors) where
import YesodDsl.AST
import Data.List
import YesodDsl.Generator.Models (baseFieldType, boolToMaybe, hsFieldType)


fieldErrors :: Module -> String
fieldErrors m = (concatMap fieldError $ [ (entityName e, entityLoc e, f)
                              | e <- modEntities m, f <- entityFields e ]
                           ++ [ (className c, classLoc c, f)
                              | c <- modClasses m, f <- classFields c ])  
                 ++ (concatMap checkError $ fieldCheckGroups)
    where                              
        sameFieldType (f1, _, _) (f2,_,_) = hsFieldType f1 == hsFieldType f2
        fieldCheckGroups :: [[(Field, FunctionName, String)]]
        fieldCheckGroups = map (nubBy sameFieldType) $
                        groupBy (\(_, fu1, _) (_, fu2, _) -> fu1 == fu2) $
                        sortBy (\(_, fu1, _) (_, fu2, _) -> compare fu1 fu2) $
                        ([ (f, func, entityFieldInfo e f) 
                          | e <- modEntities m, f <- entityFields e,
                                      func <- fieldChecks f ]
                         ++ [ (f,func, classFieldInfo c f) 
                            | c <- modClasses m, f <- classFields c,
                                         func <- fieldChecks f])
    
        isClass en = (not . null) [ c | c <- modClasses m, className c == en]
        fieldError (i, l, (Field False _ n (EntityField en)) )
            | isClass en = "Non-maybe reference to class " ++ en
                         ++ " is not allowed in " ++ i ++ "." ++ n
                         ++ " in " ++ show l ++ "\n"
        fieldError _= ""


        entityFieldInfo e f = entityName e ++ "." ++ fieldName f ++ " in "
                            ++ (show $ entityLoc e)
        classFieldInfo e f = className e ++ "." ++ fieldName f ++ " in "
                            ++ (show $ classLoc e)
                   
        checkError :: [(Field, FunctionName, String)] -> String 
        checkError xs
            | length xs > 1 = "Field-check function used for different field types:\n" ++ (unlines $ map formatCheckError xs)
            | otherwise = ""
        formatCheckError (f, func, info) = "    function " ++ func ++ " has type " ++ (hsFieldType f) ++ " -> Bool based on the field " ++ info
