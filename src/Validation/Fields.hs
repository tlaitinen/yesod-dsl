module Validation.Fields (fieldErrors) where
import AST
import Validation.Names

fieldErrors :: Module -> NameList -> String
fieldErrors m nl = concatMap fieldError $ [ (entityName e, entityLoc e, f)
                              | e <- modEntities m, f <- entityFields e ]
                           ++ [ (className c, classLoc c, f)
                              | c <- modClasses m, f <- classFields c ]  
    where                              
        isClass en = (not . null) [ n | (ns, names) <- nl, (n, _) <- names, 
                                    ns == ClassNS, en == n ] 
        fieldError (i, l, (Field False n (EntityField en)) )
            | isClass en = "Non-maybe reference to class " ++ en
                         ++ " is not allowed in " ++ i ++ "." ++ n
                         ++ " in " ++ show l ++ "\n"
        fieldError _= ""
