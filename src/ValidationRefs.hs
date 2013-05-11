{-# LANGUAGE FlexibleInstances #-}
module ValidationRefs (refs, invalidRefErrors) where
import AST
import ValidationNames
import Data.List

type Info = String
type Refs = [(Location, Info, NameSpace, Name)]

class HasRefs a where
    getRefs :: a -> Refs

instance HasReps a => HasReps [a] where
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
        [(classLoc c, "field " ++ n ++ " in class " ++ entityName c, 
         FieldTypeNS, en)]
    getRefs _ = []

instance HasRefs Resource where
    getRefs r = getRefs [ (r,h) | h <- resHandlers r ]

instance HasRefs (Resource, Handler) where
    getRefs (r, (Handler ht ps)) = getRefs [ (r,ht,p) | p <- ps ]

instance HasRefs (Resource, HandlerType, HandlerParam) where
    getRefs (r,ht,(HandlerEntity en)) = 
        [(resLoc r, "entity-reference in " ++ handlerInfo r ht,EntityNS, en)]
    getRefs (r,ht,(TextSearchFilter p fs)) = 
        getRefs [(resLoc r, "text-search-filter in " ++ handlerInfo r ht, f)
                | f <- fs ]
    getRefs (r,ht,(SelectFrom en vn)) = 
        [(resLoc r, "select-from in " ++ handlerInfo r ht, EntityNS, en)]
    getRefs (r,ht,(Join jt en vn je)) =
        [(resLoc r, show jt ++ " in " ++ handlerInfo r ht, EntityNS, en)]
        ++ getRefs (resLoc r, "join condition in " ++ show jt ++ " in " 
                            ++ handlerInfo r ht, je)
    getRefs (r,ht,(Where e)) = 
        getRefs (resLoc r, "where-expression in " ++ handlerInfo r ht, e)
    getRefs (r,ht,(OrderBy fs)) = 
        getRefs [(resLoc r, "order-by in " ++ handlerInfo r ht,
                  f) | (f,_) <- fs ]
    getRefs (r,ht,(ReturnEntity vn)) = 
        [(l,
instance HasRefs (Resource, HandlerType, Location, Info, FieldRef) where
    getRefs (r,ht,l,i,(FieldRefId vn)) = 
        [(l,i,GlobalNS, handlerName r ht ++ " " ++ vn)]
    getRefs (r,ht,l,i,(FieldRefNormal vn fn)) =
        [(l,i,GlobalNS, handlerName r ht ++ " " ++ vn)]
        ++ case lookupEntityByVariable r ht vn of
               (Just en) -> [(l,i,FieldNS, en ++ "." ++ fn)]
               Nothing -> []
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
                                    p < ps ] 
        getAlias (ht,(SelectFrom en vn)) = Just (ht,vn,en)
        getAlias (ht,(Join _ en vn _)) = Just (ht,vn,en)
        getAlias _ = Nothing    

instance HasRefs (Location, Info, (Maybe (FieldRef, BinOp, FieldRef))) where
    getRefs (l, i, (Just (f1, _, f2))) = getRefs (l,i,f1) ++ getRefs (l,i,f2)
    getRefs _ = []

instance HasRefs (Location, Info, Expr) where
    getRefs (l, i, (AndExpr e1 e2)) = getRefs (l,i,e1) ++ getRefs (l,i,e2)
    getRefs (l, i, (OrExpr e1 e2)) = getRefs (l,i,e1) ++ getRefs (l,i,e2)
    getRefs (l, i, (BinOpExpr e1 e2)) = getRefs (l,i,e1) ++ getRefs (l,i,e2)

instance HasRefs (Location, Info, ValExpr) where
    getRefs (l, i, (FieldExpr f)) = getRefs (l,i,f)
    getRefs _ = []

fieldRefs :: Location -> Info -> [FieldRef] -> Refs
fieldRefs l i refs = [ 
handlerInfo :: Route -> HandlerType -> String
handlerInfo r ht = show ht ++ " of resource " ++ show (resRoute r)

refs :: Module -> Refs
refs = getRefs
    where
        entityNames  = [(entityLoc e, entityName e) | e <- modEntities m ]
        entityFieldNames = [(entityLoc e, entityName e ++ "." ++ fieldName f)
                           | e <- modEntities m, f <- entityFields e ]

        classNames   = [(classLoc i, className i) | i <- modClasses m ]
        classFieldNames = [(classLoc i, className i ++ "." ++ fieldName f)
                           | i <- modClasses m, f <- classFields i ]

        enumNames    = [(enumLoc e, enumName e) | e <- modEnums m ]
        enumValueNames = [(enumLoc e, enumName e ++ "." ++ v) 
                         | e <- modEnums m, v <- enumValues e ]

        resNames = [(resLoc r, show $ resRoute r) | r <- modResources m ]
        resHandlerNames = [(resLoc r, show (resRoute r) ++ " " ++ show ht)
                          | r <- modResources m, 
                            (Handler ht _) <- resHandlers r ]
        handlerParams = [(resLoc r, show (resRoute r) ++ " " ++ show ht 
                           ++ " " ++ handlerParamName p)
                         | r <- modResources m,
                           (Handler ht ps) <- resHandlers r,
                           p <- ps ]

        allNames     = entityNames ++ entityFieldNames
                     ++ classNames ++ classFieldNames
                     ++ enumNames ++ enumValueNames
                     ++ resourceNames ++ resHandlerNames 
                     ++ handlerParams

        sameNameOrd (_,n1) (_,n2) = compare n1 n2
        sortedNames = sortBy sameNameOrd allNames

        sameName (_,n1) (_,n2) = n1 == n2
        groupedNames = groupBy sameName sortedNames

        factorName :: [(Location,String)] -> (String,[Location])
        factorName (all@((_,name):rest)) = (name, [l | (l,_) <- all ])
        nameGroups = map factorName groupedNames

duplicateNameErrors :: [(String,[Location])] -> String
duplicateNameErrors nameGroups 
    | not $ null duplicates = "Duplicate names:\n" ++ unlines $ map f duplicates
    | otherwise = ""
    where
        f (n, l) = n ++ " : " ++ show l
        duplicates = [ (n, locs) | (n, locs) <- nameGroups, length locs > 1 ]
