module Validation.Handlers (handlerErrors) where
import AST
import Validation.Names
import Data.Maybe
handlerErrors :: Module -> String
handlerErrors m = (concatMap notAllowedError $ 
                    [ (r, ht, resLoc r, p)
                    | r <- modRoutes m, (Handler ht ps) <- resHandlers r,
                      p <- ps, not $ allowed ht p ])
                ++ (concatMap missingError $
                     [ (r, ht, resLoc r, pt)
                    | r <- modRoutes m, (Handler ht ps) <- resHandlers r,
                      pt <- missing ht ps ])
                        
    where             
        
        allowed _ Public = True
        allowed PostHandler (Insert _ _) = True
        allowed PostHandler (Replace _ _ _) = True
        allowed PutHandler (Insert _ _) = True
        allowed PutHandler (Replace _ _ _) = True
        allowed DeleteHandler (DeleteFrom _ _ _) =True
        allowed GetHandler DefaultFilterSort = True
        allowed GetHandler (Select _) = True
        allowed GetHandler (IfFilter (_, joins, _)) = onlyInnerJoins joins
        allowed _ _ = False

        onlyInnerJoins js = all (\j -> joinType j == InnerJoin) js

        missing ht ps  
            | ht == GetHandler = mapMaybe (requireMatch ps) [
           (\p -> case p of (Select _) -> True; _ -> False, "select from")]
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

