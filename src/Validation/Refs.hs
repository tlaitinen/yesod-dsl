{-# LANGUAGE FlexibleInstances #-}
module Validation.Refs (refs, refErrors) where
import AST
import Validation.Names
import Data.List
import Data.Maybe 

type Info = String
type Ref = (Location, Info, NameSpace, Name)
type Refs = [Ref]

class HasRefs a where
    getRefs :: a -> Refs

instance HasRefs a => HasRefs [a] where
    getRefs = concatMap getRefs

instance HasRefs Module where
    getRefs m = getRefs (modEntities m)
              ++ getRefs (modClasses m)
              ++ getRefs (modResources m)

instance HasRefs Entity where
    getRefs e = getRefs [ (e,f) | f <- entityFields e]

instance HasRefs (Entity, Field) where
    getRefs (e,(Field _ n (EntityField en))) = 
        [(entityLoc e, "field " ++ n ++ " in entity " ++ entityName e, 
         FieldTypeNS, en)]
    getRefs _ = []

instance HasRefs Class where
    getRefs c = getRefs [ (c,f) | f <- classFields c ]
    
instance HasRefs (Class, Field) where
    getRefs (c,(Field _ n (EntityField en))) = 
        [(classLoc c, "field " ++ n ++ " in class " ++ className c, 
         FieldTypeNS, en)]
    getRefs _ = []

instance HasRefs Resource where
    getRefs r = getRefs [ (r,h) | h <- resHandlers r ]

instance HasRefs (Resource, Handler) where
    getRefs (r, (Handler ht ps)) = getRefs [ (r,ht,p) | p <- ps ]

instance HasRefs (Resource, HandlerType, HandlerParam) where
    getRefs (r,ht,(Insert en io)) = 
        [(resLoc r, "insert in " ++ handlerInfo r ht,EntityNS, en)]
        ++ getRefs (r,ht,resLoc r, "insert input expression in " ++ handlerInfo r ht, io) 
    getRefs (r,ht,(Replace en fr io)) = 
        [(resLoc r, "replace in " ++ handlerInfo r ht, EntityNS, en)]
        ++ getRefs (r, ht, resLoc r, "replace-with in " ++ handlerInfo r ht, fr)
        ++ getRefs (r, ht, resLoc r, "replace-from in " ++ handlerInfo r ht, io)
                        
    getRefs (r,ht,(TextSearchFilter p fs)) = 
        getRefs [(r,ht,resLoc r, "text-search-filter in " ++ handlerInfo r ht, f)
                | f <- fs ]
    getRefs (r,ht,(SelectFrom en vn)) = 
        [(resLoc r, "select-from in " ++ handlerInfo r ht, EntityNS, en)]
    getRefs (r,ht,(DeleteFrom en vn me)) =
        [(resLoc r, "delete-from in " ++ handlerInfo r ht, EntityNS, en)]
        ++ (case me of 
                (Just e) -> getRefs (r,ht,resLoc r, "where-expression of delete in " ++ handlerInfo r ht, e)
                _ -> [])
    getRefs (r,ht,(Join jt en vn je)) =
        [(resLoc r, show jt ++ " in " ++ handlerInfo r ht, EntityNS, en)]
        ++ getRefs (r,ht,resLoc r, "join condition in " ++ show jt ++ " in " 
                            ++ handlerInfo r ht, je)
    getRefs (r,ht,(Where e)) = 
        getRefs (r,ht,resLoc r, "where-expression in " ++ handlerInfo r ht, e)
    getRefs (r,ht,(OrderBy fs)) = 
        getRefs [(r,ht,resLoc r, "order-by in " ++ handlerInfo r ht,
                  f) | (f,_) <- fs ]
    getRefs (r,ht,(ReturnEntity vn)) = 
        [(resLoc r, "return-expression in " ++ handlerInfo r ht, 
          GlobalNS, 
          handlerName r ht ++ " " ++ vn)]
    getRefs (r,ht,(ReturnFields fs)) =
        getRefs [(r,ht,
                  (resLoc r), "return-expression in " ++ handlerInfo r ht, f)
                | (_, f) <- fs ]
    getRefs _ = []            
                  
instance HasRefs (Resource, HandlerType, Location, Info, FieldRef) where
    getRefs (r,ht,l,i,(FieldRefId vn)) = 
        [(l,i,GlobalNS, handlerName r ht ++ " " ++ vn)]
    getRefs (r,ht,l,i,(FieldRefNormal vn fn)) =
        [(l,i,GlobalNS, handlerName r ht ++ " " ++ vn)]
        ++ case lookupEntityByVariable r ht vn of
               (Just en) -> [(l,i,FieldNS, en ++ "." ++ fn)]
               Nothing -> []
    getRefs (r,ht,l,i,(FieldRefPathParam pi)) = 
        [(l,i,RouteNS, show (resRoute r) ++ " $" ++ show pi)]
    getRefs _ = []

instance HasRefs (Resource, HandlerType, Location, Info, Maybe [InputField]) where
    getRefs (r,ht,l,i,Just io) = getRefs [ (r,ht,l,i,f) | (pn,f) <- io ]
    getRefs _ = []

    
instance HasRefs (Resource, HandlerType, Location, Info, InputFieldRef) where
    getRefs (r,ht,l,i,(InputFieldNormal fn)) = []
    getRefs (r,ht,l,i,(InputFieldPathParam pi)) = 
        [(l,i,RouteNS, show (resRoute r) ++ " $" ++ show pi)]    
    getRefs _ = []

lookupEntityByVariable :: Resource -> HandlerType -> VariableName 
                       -> Maybe EntityName
lookupEntityByVariable r ht vn = 
    case find (\(ht',vn',_) -> ht == ht' && vn == vn') aliases of
        (Just (_, _, en)) -> Just en
        Nothing -> Nothing 
    where   
        aliases = mapMaybe getAlias [ (ht,p) 
                                    | (Handler ht ps) <- resHandlers r,
                                    p <- ps ] 
        getAlias (ht,(SelectFrom en vn)) = Just (ht,vn,en)
        getAlias (ht,(Join _ en vn _)) = Just (ht,vn,en)
        getAlias _ = Nothing    

instance HasRefs (Resource, HandlerType, Location, Info, (Maybe (FieldRef, BinOp, FieldRef))) where
    getRefs (r,ht,l, i, (Just (f1, _, f2))) = getRefs (r,ht,l,i,f1) ++ getRefs (r,ht,l,i,f2)
    getRefs _ = []

instance HasRefs (Resource, HandlerType, Location, Info, Expr) where
    getRefs (r,ht,l, i, (AndExpr e1 e2)) = getRefs (r,ht,l,i,e1) ++ getRefs (r,ht,l,i,e2)
    getRefs (r,ht,l, i, (OrExpr e1 e2)) = getRefs (r,ht,l,i,e1) ++ getRefs (r,ht,l,i,e2)
    getRefs (r,ht,l, i, (BinOpExpr e1 op e2)) = getRefs (r,ht,l,i,e1) ++ getRefs (r,ht,l,i,e2)

instance HasRefs (Resource, HandlerType, Location, Info, ValExpr) where
    getRefs (r,ht,l, i, (FieldExpr f)) = getRefs (r,ht,l,i,f)
    getRefs _ = []

handlerInfo :: Resource-> HandlerType -> String
handlerInfo r ht = show ht ++ " of resource " ++ show (resRoute r)

refs :: Module -> Refs
refs = getRefs

refErrors :: NameList -> Refs -> String
refErrors nl refs = 
    concatMap refError $ filter (not . (hasRef  nl)) refs
        where
            refError (l,i,ns,n) = "Reference to an undefined name '" ++ n 
                                ++ "' in " ++ i ++ " in " ++ show l ++ "\n"

hasRef :: NameList -> Ref -> Bool
hasRef nl (_,_,ns,n) = isJust $ findName nl ns n

