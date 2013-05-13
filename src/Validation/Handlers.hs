module Validation.Handlers (handlerErrors) where
import AST
import Validation.Names

handlerErrors :: Module -> String
handlerErrors m = concatMap handlerError $ 
                    [ (r, ht, resLoc r, p)
                    | r <- modResources m, (Handler ht ps) <- resHandlers r,
                      p <- ps, not $ allowed ht p ] 
    where             
        allowed _ Public = True
        allowed PutHandler (ReadJson _) = True
        allowed PostHandler (ReadJson _) = True
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
        allowed _ _ = False
        handlerError (r, ht, l, p) = show p ++ " not allowed in " 
                                   ++ show ht ++ " of " ++ show (resRoute r)
                                   ++ " in " ++ show l ++ "\n"
