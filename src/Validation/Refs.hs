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
    getRefs (r, h) = getRefs [ (r,h,p) | p <- handlerParams h ]

instance HasRefs (Route, Handler, HandlerParam) where
    getRefs (r,h,(Insert en io)) = 
        [(routeLoc r, "insert in " ++ handlerInfo r h,EntityNS, en)]
        ++ getRefs (r,h, handlerLoc h, "insert input expression in " ++ handlerInfo r h, io) 
    getRefs (r,h,(Update en fr io)) = 
        [(routeLoc r, "update in " ++ handlerInfo r h, EntityNS, en)]
        ++ getRefs (r,h, handlerLoc h, "update-with in " ++ handlerInfo r h, fr)
        ++ getRefs (r,h, handlerLoc h, "update in " ++ handlerInfo r h, io)
                        
    getRefs (r,h,(Select sq)) = getRefs (r,h,sq)
    getRefs (r,h,(IfFilter ps)) = getRefs (r,h,ps)
    getRefs (r,h,(DeleteFrom en vn me)) =
        [(routeLoc r, "delete-from in " ++ handlerInfo r h, EntityNS, en)]
        ++ (case me of 
                (Just e) -> getRefs (r,h, handlerLoc h, "where-expression of delete in " ++ handlerInfo r h, e,1000::Int)
                _ -> [])
    getRefs _ = []            

instance HasRefs (Route, Handler, IfFilterParams) where
    getRefs (r,h,(pn,joins,e)) = let
        l = routeLoc r
        i = "if param \"" ++ pn ++ "\" = $$ then" 
        in getRefs (r,h,l,i,e,1000::Int) 
           ++ getRefs [ (r,h,l,i,j,lvl) 
                         | (j,lvl) <- zip joins ([1..] :: [Int]) ]

instance HasRefs (Route, Handler, SelectQuery) where
    getRefs (r,h,sq) = let
        l= routeLoc r
        i = "select query in " ++ handlerName r h
        (en,vn) = sqFrom sq
        in [(l,i,EntityNS,en), 
            (l,i,GlobalNS, handlerName r h ++ " " ++ vn)]
           ++ (case sqWhere sq of
                  Just e -> getRefs (r,h,l,i,e,1000::Int)
                  Nothing -> [])
           ++ getRefs [ (r,h,l,i,j,lvl) | (j,lvl) <- zip (sqJoins sq) ([1..] :: [Int])]
           ++ getRefs [ (r,h,l,i,fr, 1000::Int) | (fr,_) <- sqOrderBy sq ]
           
instance HasRefs (Route, Handler, Location, Info, Join,Int) where
    getRefs (r,h,l,i,j,lvl) = [(l,i,EntityNS, joinEntity j)]
                         ++ [(l,i,NestedNS lvl, handlerName r h ++ " " 
                                    ++ joinAlias j)]
                         ++ (case joinExpr j of
                                Just e ->
                                    getRefs (r,h,l,i,e,lvl)
                                Nothing -> [])

instance HasRefs (Route, Handler, Location, Info, FieldRef, Int) where
    getRefs (r,h,l,i,(FieldRefId vn), lvl) = 
        [(l,i,NestedNS lvl, handlerName r h ++ " " ++ vn)]
    getRefs (r,h,l,i,(FieldRefNormal vn fn), lvl) =
        [(l,i,NestedNS lvl, handlerName r h ++ " " ++ vn)]
        ++ case lookupEntityByVariable r h vn of
               (Just en) -> [(l,i,FieldNS, en ++ "." ++ fn)]
               Nothing -> []
    getRefs (r,h,l,i,(FieldRefPathParam pi), lvl) = 
        [(l,i,RouteNS, show (routePath r) ++ " $" ++ show pi)]
    getRefs _ = []

instance HasRefs (Route, Handler, Location, Info, Maybe [InputField]) where
    getRefs (r,h,l,i,Just io) = getRefs [ (r,h,l,i,f) | (pn,f) <- io ]
    getRefs _ = []

    
instance HasRefs (Route, Handler, Location, Info, InputFieldRef) where
    getRefs (r,h,l,i,(InputFieldNormal fn)) = []
    getRefs (r,h,l,i,(InputFieldPathParam pi)) = 
        [(l,i,RouteNS, show (routePath r) ++ " $" ++ show pi)]    
    getRefs _ = []

lookupEntityByVariable :: Route -> Handler -> VariableName 
                       -> Maybe EntityName
lookupEntityByVariable r h vn = 
    case find (\(_, vn') -> vn == vn') aliases of
        (Just (en,_)) -> Just en
        Nothing -> Nothing 
    where   
        aliases = concatMap getAlias [ p 
                                    | (Handler l ht ps) <- routeHandlers r,
                                    p <- ps, ht == handlerType h ] 
        getAlias (Select sq) = sqAliases sq
        getAlias _ = []

instance HasRefs (Route, Handler, Location, Info, (Maybe (FieldRef, BinOp, FieldRef)), Int) where
    getRefs (r,h,l, i, (Just (f1, _, f2)), lvl) = getRefs (r,h,l,i,f1, lvl) ++ getRefs (r,h,l,i,f2, lvl)
    getRefs _ = []

instance HasRefs (Route, Handler, Location, Info, Expr,Int) where
    getRefs (r,h,l, i, (AndExpr e1 e2),lvl) = getRefs (r,h,l,i,e1,lvl) ++ getRefs (r,h,l,i,e2,lvl)
    getRefs (r,h,l, i, (OrExpr e1 e2),lvl) = getRefs (r,h,l,i,e1,lvl) ++ getRefs (r,h,l,i,e2,lvl)
    getRefs (r,h,l, i, (BinOpExpr e1 op e2),lvl) = getRefs (r,h,l,i,e1,lvl) ++ getRefs (r,h,l,i,e2,lvl)
    getRefs (r,h,l,i, (ListOpExpr fr1 op fr2),lvl) = getRefs (r,h,l,i,fr1,lvl) ++ getRefs (r,h,l,i,fr2,lvl)

instance HasRefs (Route, Handler, Location, Info, ValExpr,Int) where
    getRefs (r,h,l, i,(FieldExpr f),lvl) = getRefs (r,h,l,i,f,lvl)
    getRefs (r,h,l,i, (ConcatExpr f1 f2),lvl) = getRefs [(r,h,l,i,f1,lvl),(r,h,l,i,f2,lvl)]
    getRefs _ = []

handlerInfo :: Route-> Handler -> String
handlerInfo r h = show (handlerType h) ++ " of route " ++ show (routePath r)

refs :: Module -> Refs
refs = getRefs

refErrors :: NameList -> Refs -> String
refErrors nl refs = 
    (concatMap refError $ filter (not . (hasRef  nl)) refs)
        where
            refError (l,i,ns,n) = "Reference to an undefined name '" ++ n 
                                ++ "' in " ++ i ++ " in " ++ show l ++ "\n"

hasRef :: NameList -> Ref -> Bool
hasRef nl (_,_,ns,n) = isJust $ findName nl ns n

