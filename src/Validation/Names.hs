{-# LANGUAGE FlexibleInstances #-}
module Validation.Names (names, 
                         findName, 
                         NameList, Name, NameSpace(..), nameErrors) where

import AST
import Data.List

data NameSpace = GlobalNS | ClassNS | EntityNS | EnumNS | FieldNS | FieldTypeNS | InputNS | RouteNS | NestedNS Int deriving (Eq, Ord, Show)
type Name = String
type NameList = [(NameSpace, [(Name, [Location])])]

findName :: NameList -> NameSpace -> Name -> Maybe Location
findName nl ns' n' = case matching of
    ((_,_,l):_)  -> Just (head l)
    _ -> Nothing
    where matching = [ (ns, n, l) | (ns, names) <- nl, (n, l) <- names,
                      ns `subNameSpaceOf` ns', n == n' ]
subNameSpaceOf :: NameSpace -> NameSpace -> Bool             
subNameSpaceOf ns1 ns2 = case (ns1,ns2) of
    (NestedNS l1, NestedNS l2) -> l1 <= l2
    (ns1', ns2') -> ns1' == ns2'
             
class HasNames a where 
    getNames :: a -> [([NameSpace], Name, Location)]

instance HasNames a => HasNames [a] where
    getNames = concatMap getNames

instance HasNames Module where
    getNames m = getNames (modEntities m) 
               ++ getNames (modClasses m)
               ++ getNames (modEnums m)
               ++ getNames (modRoutes m) 

instance HasNames Entity where
    getNames e = [([GlobalNS, EntityNS, FieldTypeNS], entityName e, entityLoc e)]
               ++ getNames [ (e, f) | f <- (entityFields e) ]
               ++ [([GlobalNS], "Unique" ++ uniqueName u, entityLoc e) 
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


instance HasNames Route where
    getNames r = [([GlobalNS], show $ routePath r, routeLoc r)]
               ++ getNames [ (r, h) | h <- routeHandlers r ]
               ++ [([GlobalNS, RouteNS], show (routePath r) ++ " $" ++ show i,
                    routeLoc r) | i <- [1..length (routePathParams r)] ]
               
instance HasNames (Route, Handler) where
    getNames (r,h) = [([GlobalNS], handlerName r h, routeLoc r)]
                                 ++ getNames [ (r,h,p) | p <- handlerParams h ]
instance HasNames (Route, Handler, HandlerParam) where
    getNames (r,h,p) = [([GlobalNS],
                          handlerName r h ++ " "++ pn,
                          routeLoc r) | pn <- handlerParamName p]
        ++ case p of
            Select sq -> [([NestedNS lvl],
                          handlerName r h ++ " " ++ vn,
                          routeLoc r) | (vn,lvl) <- sqScopedAliases sq]
            _         -> []
                        

sqScopedAliases :: SelectQuery -> [(VariableName, Int)]
sqScopedAliases sq = [(vn,0)] 
    ++ [ (joinAlias j,lvl) | (j,lvl) <- zip (sqJoins sq) [1..] ]
    where (_,vn) = sqFrom sq
          

handlerParamName :: HandlerParam -> [String]
handlerParamName Public = ["public"]
handlerParamName DefaultFilterSort = ["default-filter-sort"]
handlerParamName (Select sq) = ["select from"] ++ [ vn | (_,vn) <- sqAliases sq ]
handlerParamName (DeleteFrom _ v _) = [v, "delete"]
handlerParamName (Update _ _ _) = [""]
handlerParamName (Insert _ _) = [""] 
handlerParamName (IfFilter (pn,_,_)) = ["filter " ++ pn]
groupByName :: [(Name, Location)] -> [(Name, [Location])]
groupByName ns = nameGroups 
    where
        sortedNames = sortBy (\(n1,_) (n2,_) -> compare n1 n2) ns
        groupedNames = groupBy (\(n1,_) (n2,_) -> n1 == n2) sortedNames
        factorName (all@((name,_):_)) = (name, [l | (_,l) <- all ])
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
