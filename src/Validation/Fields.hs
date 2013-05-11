module Validation.Fields (fieldErrors) where

import Validation.Names

fieldErrors :: Module -> NameList -> String
fieldErrors m nl = concatMap fieldError fields 
    where
        fields = concatMap entityFields (modEntities m)
               ++ concatMap classFields (modClasses m)
        isClass en = (not . null) [ n | (ns, names) <- nl, (n, _) <- names, 
                                    ns == ClassNS, en == n ] 
        fieldError (Field False n (EntityField en)) 
            | isClass en = ""
        fieldError _= ""
