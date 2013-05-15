module Validation.Handlers (handlerErrors) where
import AST
import Validation.Names
import Data.Maybe
handlerErrors :: Module -> String
handlerErrors m = (concatMap notAllowedError $ 
                    [ (r, ht, resLoc r, p)
                    | r <- modResources m, (Handler ht ps) <- resHandlers r,
                      p <- ps, not $ allowed ht p ])
                ++ (concatMap missingError $
                     [ (r, ht, resLoc r, pt)
                    | r <- modResources m, (Handler ht ps) <- resHandlers r,
                      pt <- missing ht ps ])
                        
    where             
        
        allowed _ Public = True
        allowed PostHandler (Insert _ _) = True
        allowed PostHandler (Replace _ _ _) = True
        allowed PutHandler (Insert _ _) = True
        allowed PutHandler (Replace _ _ _) = True
        allowed DeleteHandler (DeleteFrom _ _ _) =True
        allowed GetHandler DefaultFilterSort = True
        allowed GetHandler (TextSearchFilter _ _) = True
        allowed GetHandler (Join _ _ _ _) = True
        allowed GetHandler (Where _) = True
        allowed GetHandler (OrderBy _) = True
        allowed GetHandler (SelectFrom _ _) = True
        allowed GetHandler (ReturnEntity _) = True
        allowed GetHandler (ReturnFields _) = True
        allowed GetHandler (Limit _) = True
        allowed GetHandler (Offset _) = True
        allowed _ _ = False

        missing ht ps  
            | ht == GetHandler = mapMaybe (requireMatch ps) [
           (\p -> case p of (SelectFrom _ _) -> True; _ -> False, "select from"),
           (\p -> case p of (ReturnEntity _) -> True ; (ReturnFields _) -> True ; _ -> False, "return")]
            | ht == PutHandler || ht == PostHandler = mapMaybe (requireMatch ps) [   
           (\p -> case p of (Insert _ _) -> True ; (Replace _ _ _ ) -> True ; _ -> False, "insert or replace")]
            | otherwise = []
        requireMatch ps (f,err) = case listToMaybe (filter f ps) of
            Just _ -> Nothing
            Nothing -> Just err     
        notAllowedError (r, ht, l, p) = show p ++ " not allowed in " 
                                   ++ show ht ++ " of " ++ show (resRoute r)
                                   ++ " in " ++ show l ++ "\n"
        missingError (r,ht,l,p) = "Missing " ++ p ++ " in " ++ show ht 
                                ++ " of " ++ show (resRoute r) ++ " in "
                                ++ show l ++ "\n"

