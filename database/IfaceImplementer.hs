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
            dbDocs  = implInDocs ifaces (dbDocs db)
        }

ifaceLookup :: [Iface] -> IfaceName -> Maybe Iface
ifaceLookup ifaces name =  find (\i -> name == ifaceName i) ifaces

implInDocs :: [Iface] -> [Doc] -> [Doc]
implInDocs ifaces entities = map (implInDoc ifaces) entities


docError :: Doc -> String -> a
docError e msg = error $ msg ++ " (" ++ docPath e++ ")"
implInDoc :: [Iface] -> Doc -> Doc
implInDoc ifaces e 
    | null invalidIfaceNames = e {
        docFields  = docFields e ++ extraFields,
        docIndices = docIndices e ++ extraIndices
    }
    | otherwise        = docError e $ "Invalid interfaces " 
                                        ++ show invalidIfaceNames
    where
        implements = docImplements e
        invalidIfaceNames = [ name | name <- implements, 
                                 isNothing $ ifaceLookup ifaces name ]
        implementedIfaces = map (ifaceLookup ifaces) implements
        validIfaces = catMaybes implementedIfaces
                                     
        extraFields = concat $ map ifaceFields validIfaces
        extraIndices = concat $ map ifaceIndices validIfaces
        
    
