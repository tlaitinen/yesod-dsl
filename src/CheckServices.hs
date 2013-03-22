module CheckServices (checkServices) where
import DbTypes
checkServices :: DbModule -> DbModule
checkServices db 
    | null duplicateServices && null invalidParams = db
    | otherwise = error $ "Invalid service definitions:\n" 
                        ++ (unlines $ map formatDuplicateService duplicateServices ++ map formatInvalidParams invalidParams)
 
    where 
          duplicateServices = [ (e,s1,s2) | e <- dbEntities db,
                         (i1,s1@(Service t1 _)) <- zip [1..] (entityServices e),
                         (i2,s2@(Service t2 _)) <- zip [1..] (entityServices e),
                         i1 < i2, t1 == t2 ]
          formatDuplicateService (e,(Service t1 _),_) = 
                    "    " ++ show t1 ++ " defined more than once in " ++ show (entityLoc e)
          invalidParam t (ServiceDefaultFilterSort) = t /= GetService
          invalidParam t (ServiceFilter _) = t /= GetService
          invalidParam t (ServiceSort _) = t /= GetService
          invalidParam _ _ = False
                
          invalidParams = [ (e,t,p) | e <- dbEntities db,
                                      s@(Service t params) <- entityServices e,   
                                      p <- params,
                                      invalidParam t p ]
                                      
          formatInvalidParams (e,t,p) = "    " ++ show p ++ " not allowed for " ++ show t ++ " in " ++ show (entityLoc e)

          
   
