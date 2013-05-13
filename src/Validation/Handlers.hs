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
        allowed _ (BeforeHandler _) = True
        allowed _ (AfterHandler _)= True
        allowed PutHandler (HandlerEntity _) = True
        allowed PostHandler (HandlerEntity _) = True
        allowed DeleteHandler (HandlerEntity _) =True
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
