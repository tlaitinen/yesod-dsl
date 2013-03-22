module CheckServices (checkServices) where
import DbTypes
checkServices :: DbModule -> DbModule
checkServices db 
    | null duplicateServices = db
    | otherwise = error $ "Duplicate service definitions:\n" 
                        ++ (unlines $ map formatService duplicateServices)
 
    where 
          duplicateServices = [ (e,s1,s2) | e <- dbEntities db,
                         (i1,s1@(Service t1 _)) <- zip [1..] (entityServices e),
                         (i2,s2@(Service t2 _)) <- zip [1..] (entityServices e),
                         i1 < i2, t1 == t2 ]
          formatService (e,(Service t1 _),_) = "    " ++ show t1 ++ " in " ++ show (entityLoc e)

          
   
