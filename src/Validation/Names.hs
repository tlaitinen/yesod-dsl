{-# LANGUAGE FlexibleInstances #-}
module Validation.Names (names, 
                         findName, 
                         NameList, Name, NameSpace(..), nameErrors) where

import AST
import Data.List

data NameSpace = GlobalNS | ClassNS | EntityNS | EnumNS | FieldNS | FieldTypeNS
               deriving (Eq, Ord)
type Name = String
type NameList = [(NameSpace, [(Name, [Location])])]

findName :: NameList -> NameSpace -> Name -> Maybe Location
findName nl ns' n' = case matching of
    ((_,_,l):_)  -> Just (head l)
    _ -> Nothing
    where matching = [ (ns, n, l) | (ns, names) <- nl, (n, l) <- names,
                      ns == ns', n == n' ]

             
class HasNames a where 
    getNames :: a -> [([NameSpace], Name, Location)]

instance HasNames a => HasNames [a] where
    getNames = concatMap getNames

instance HasNames Module where
    getNames m = getNames (modEntities m) 
               ++ getNames (modClasses m)
               ++ getNames (modEnums m)
               ++ getNames (modResources m) 

instance HasNames Entity where
    getNames e = [([GlobalNS, EntityNS, FieldTypeNS], entityName e, entityLoc e)]
               ++ getNames [ (e, f) | f <- (entityFields e) ]
               ++ [([GlobalNS], uniqueName u, entityLoc e) 
                    | u <- entityUniques e ]

instance HasNames (Entity, Field) where
    getNames (e,f) = [([GlobalNS, FieldNS], entityName e ++ "." ++ fieldName f,
                      entityLoc e)]
               
instance HasNames Class where 
    getNames c = [([GlobalNS, ClassNS, EntityNS, FieldTypeNS], className c, classLoc c)]
               ++ getNames [ (c,f) | f <- (classFields c)]

               ++ [([GlobalNS], uniqueName u, classLoc c) 
                  | u <- classUniques c ]



instance HasNames (Class, Field) where
    getNames (c,f) = [([GlobalNS, FieldNS], className c ++ "." ++ fieldName f,
                      classLoc c)]

instance HasNames EnumType where
    getNames e = [([GlobalNS, EnumNS, FieldTypeNS], enumName e, enumLoc e)]
               ++ getNames [ (e, v) | v <- enumValues e]

instance HasNames (EnumType, String) where
    getNames (e,v) = [([GlobalNS, EnumNS], enumName e ++ "." ++ v, enumLoc e)]


instance HasNames Resource where
    getNames r = [([GlobalNS], show $ resRoute r, resLoc r)]
               ++ getNames [ (r, h) | h <- resHandlers r ]
               
instance HasNames (Resource, Handler) where
    getNames (r,(Handler ht ps)) = [([GlobalNS], handlerName r ht, resLoc r)]
                                 ++ getNames [ (r,ht,p) | p <- ps ]
instance HasNames (Resource, HandlerType, HandlerParam) where
    getNames (r,ht,p) = [([GlobalNS],
                          handlerName r ht ++ " "++ handlerParamName p,
                          resLoc r)]
     

handlerParamName :: HandlerParam -> String
handlerParamName Public = "public"
handlerParamName (HandlerEntity en) = "entity " ++ en
handlerParamName DefaultFilterSort = "default-filter-sort"
handlerParamName (TextSearchFilter pn _) = "text-search-filter " ++ pn
handlerParamName (SelectFrom en v) = v
handlerParamName (Join _ en v _) =  v
handlerParamName (Where e) = "where" 
handlerParamName (MapBy f) = "map-by"
handlerParamName (OrderBy fs) = "order-by"
handlerParamName (ReturnEntity _) = "return"
handlerParamName (ReturnFields _) = "return"
handlerParamName (BeforeHandler f) = "before-handler " ++ f
handlerParamName (AfterHandler f) = "after-handler " ++ f
groupByName :: [(Name, Location)] -> [(Name, [Location])]
groupByName ns = nameGroups 
    where
        sortedNames = sortBy (\(n1,_) (n2,_) -> compare n1 n2) ns
        groupedNames = groupBy (\(n1,_) (n2,_) -> n1 == n2) sortedNames
        factorName (all@((name,_):rest)) = (name, [l | (_,l) <- all ])
        nameGroups = map factorName groupedNames


toNameList ::  [([NameSpace], Name, Location)] -> NameList
toNameList xs = [ (ns, groupByName names) | (ns, names) <- groups ]
    where
        xs' = [ (ns, n, l) | (nss, n, l) <- xs, ns <- nss ]
        sorted = sortBy (\(n1,_,_) (n2,_,_) -> compare n1 n2) xs'

        grouped = groupBy (\(n1,_,_) (n2,_,_) -> n1 == n2) sorted

        factor (all@((ns,_,_):rest)) = (ns, [(n,l) | (_,n,l) <- all ])
        groups = map factor grouped


names :: Module -> NameList
names = toNameList . getNames

duplicates :: [(Name,[Location])] -> [(Name, Location)]
duplicates nl = [ (n, l) | (n, locs) <- nl, l <- locs, length locs > 1 ]

nameErrors :: NameList -> String
nameErrors xs 
    | not $ null dups = "Duplicate definitions:\n" ++ (unlines $ map f dups)
    | otherwise = ""
    where
        f (n, l) = n ++ " : " ++ show l
        dups = case find (\(ns,_) -> ns == GlobalNS) xs of
            (Just (ns, names)) -> duplicates names
            Nothing -> []
