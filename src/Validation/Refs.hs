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
              ++ getRefs (modRoutes m)

instance HasRefs Entity where
    getRefs e = getRefs [ (e,f) | f <- entityFields e]

instance HasRefs (Entity, Field) where
    getRefs (e,(Field _ n (EntityField en))) = 
        [(entityLoc e, "field " ++ n ++ " in entity " ++ entityName e, 
         FieldTypeNS, en)]
    getRefs (e,(Field _ n (EnumField en))) = 
        [(entityLoc e, "field " ++ n ++ " in entity " ++ entityName e,
            EnumNS, en)]    
    getRefs _ = []

instance HasRefs Class where
    getRefs c = getRefs [ (c,f) | f <- classFields c ]
    
instance HasRefs (Class, Field) where
    getRefs (c,(Field _ n (EntityField en))) = 
        [(classLoc c, "field " ++ n ++ " in class " ++ className c, 
         FieldTypeNS, en)]
    getRefs (c,(Field _ n (EnumField en))) = 
        [(classLoc c, "field " ++ n ++ " in class " ++ className c, 
         EnumNS, en)]
 
    getRefs _ = []

instance HasRefs Route where
    getRefs r = getRefs [ (r,h) | h <- routeHandlers r ]

instance HasRefs (Route, Handler) where
    getRefs (r, (Handler l ht ps)) = getRefs [ (r,ht,p) | p <- ps ]

instance HasRefs (Route, HandlerType, HandlerParam) where
    getRefs (r,ht,(Insert en io)) = 
        [(routeLoc r, "insert in " ++ handlerInfo r ht,EntityNS, en)]
        ++ getRefs (r,ht,routeLoc r, "insert input expression in " ++ handlerInfo r ht, io) 
    getRefs (r,ht,(Replace en fr io)) = 
        [(routeLoc r, "replace in " ++ handlerInfo r ht, EntityNS, en)]
        ++ getRefs (r, ht, routeLoc r, "replace-with in " ++ handlerInfo r ht, fr)
        ++ getRefs (r, ht, routeLoc r, "replace-from in " ++ handlerInfo r ht, io)
                        
    getRefs (r,ht,(Select sq)) = getRefs (r,ht,sq)
    getRefs (r,ht,(DeleteFrom en vn me)) =
        [(routeLoc r, "delete-from in " ++ handlerInfo r ht, EntityNS, en)]
        ++ (case me of 
                (Just e) -> getRefs (r,ht,routeLoc r, "where-expression of delete in " ++ handlerInfo r ht, e)
                _ -> [])
    getRefs _ = []            

instance HasRefs (Route, HandlerType, SelectQuery) where
    getRefs (r,ht,sq) = let
        l= routeLoc r
        i = "select query in " ++ handlerName r ht
        (en,vn) = sqFrom sq
        in [(l,i,EntityNS,en), 
            (l,i,GlobalNS, handlerName r ht ++ " " ++ vn)]
           ++ (case sqWhere sq of
                  Just e -> getRefs (r,ht,l,i,e)
                  Nothing -> [])
           ++ getRefs [ (r,ht,l,i,j) | j <- sqJoins sq ]
           ++ getRefs [ (r,ht,l,i,fr) | (fr,_) <- sqOrderBy sq ]
           
instance HasRefs (Route, HandlerType, Location, Info, Join) where
    getRefs (r,ht,l,i,j) = [(l,i,EntityNS, joinEntity j)]
                         ++ [(l,i,GlobalNS, handlerName r ht ++ " " 
                                    ++ joinAlias j)]
                         ++ (case joinExpr j of
                                Just (fr1, _, fr2) ->
                                    getRefs [(r,ht,l,i,fr1), (r,ht,l,i,fr2)]
                                Nothing -> [])

instance HasRefs (Route, HandlerType, Location, Info, FieldRef) where
    getRefs (r,ht,l,i,(FieldRefId vn)) = 
        [(l,i,GlobalNS, handlerName r ht ++ " " ++ vn)]
    getRefs (r,ht,l,i,(FieldRefNormal vn fn)) =
        [(l,i,GlobalNS, handlerName r ht ++ " " ++ vn)]
        ++ case lookupEntityByVariable r ht vn of
               (Just en) -> [(l,i,FieldNS, en ++ "." ++ fn)]
               Nothing -> []
    getRefs (r,ht,l,i,(FieldRefPathParam pi)) = 
        [(l,i,RouteNS, show (routePath r) ++ " $" ++ show pi)]
    getRefs _ = []

instance HasRefs (Route, HandlerType, Location, Info, Maybe [InputField]) where
    getRefs (r,ht,l,i,Just io) = getRefs [ (r,ht,l,i,f) | (pn,f) <- io ]
    getRefs _ = []

    
instance HasRefs (Route, HandlerType, Location, Info, InputFieldRef) where
    getRefs (r,ht,l,i,(InputFieldNormal fn)) = []
    getRefs (r,ht,l,i,(InputFieldPathParam pi)) = 
        [(l,i,RouteNS, show (routePath r) ++ " $" ++ show pi)]    
    getRefs _ = []

lookupEntityByVariable :: Route -> HandlerType -> VariableName 
                       -> Maybe EntityName
lookupEntityByVariable r ht vn = 
    case find (\(_, vn') -> vn == vn') aliases of
        (Just (en,_)) -> Just en
        Nothing -> Nothing 
    where   
        aliases = concatMap getAlias [ p 
                                    | (Handler l ht' ps) <- routeHandlers r,
                                    p <- ps, ht == ht' ] 
        getAlias (Select sq) = sqAliases sq
        getAlias _ = []

instance HasRefs (Route, HandlerType, Location, Info, (Maybe (FieldRef, BinOp, FieldRef))) where
    getRefs (r,ht,l, i, (Just (f1, _, f2))) = getRefs (r,ht,l,i,f1) ++ getRefs (r,ht,l,i,f2)
    getRefs _ = []

instance HasRefs (Route, HandlerType, Location, Info, Expr) where
    getRefs (r,ht,l, i, (AndExpr e1 e2)) = getRefs (r,ht,l,i,e1) ++ getRefs (r,ht,l,i,e2)
    getRefs (r,ht,l, i, (OrExpr e1 e2)) = getRefs (r,ht,l,i,e1) ++ getRefs (r,ht,l,i,e2)
    getRefs (r,ht,l, i, (BinOpExpr e1 op e2)) = getRefs (r,ht,l,i,e1) ++ getRefs (r,ht,l,i,e2)

instance HasRefs (Route, HandlerType, Location, Info, ValExpr) where
    getRefs (r,ht,l, i, (FieldExpr f)) = getRefs (r,ht,l,i,f)
    getRefs (r,ht,l,i, (ConcatExpr f1 f2)) = getRefs [(r,ht,l,i,f1),(r,ht,l,i,f2)]
    getRefs _ = []

handlerInfo :: Route-> HandlerType -> String
handlerInfo r ht = show ht ++ " of route " ++ show (routePath r)

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

