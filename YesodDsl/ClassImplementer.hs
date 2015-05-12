{-# LANGUAGE TupleSections #-}
module YesodDsl.ClassImplementer (implementClasses) where
import Data.List
import YesodDsl.AST
import System.IO.Unsafe
import Data.Maybe
import Data.Generics
import Data.Generics.Uniplate.Data
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.List as L

lookupField' :: Module -> EntityName -> FieldName -> Maybe Field
lookupField' m en fn = listToMaybe [ f | e <- modEntities m,
                                    f <- entityFields e,
                                    entityName e == en,
                                    fieldName f == fn ] 


implementClasses :: Module -> Module
implementClasses m = let m' = m {
        modEntities  = [ implInEntity m (modClasses m) e | e <- modEntities m ]
    } in m' {
        modRoutes = everywhere (mkT $ trSq m') $ modRoutes m'
    }

trSq :: Module -> SelectQuery -> SelectQuery
trSq m sq = sq {
        sqFields = concatMap trSelectField $ sqFields sq,
        sqJoins =  map snd newJoins,
        sqWhere = everywhere (mkT trExpr) $ sqWhere sq
    }
    where
        vnMap :: [(VariableName, (VariableName, Maybe EntityName))]
        vnMap = mapMaybe fst newJoins
        aliases = map (\(er,vn) -> (entityRefName er, vn)) $ sqFrom sq : [ (joinEntity j,joinAlias j) | j <- sqJoins sq ]
        newAliases vn = Map.findWithDefault [(vn,Nothing)] vn $ Map.fromListWith (++) $ [ (s,[d]) | (s,d) <- vnMap ]
        allAliases = aliases ++ catMaybes [ men >>= Just . (,vn) | (_,(vn,men)) <- vnMap ]
        newJoins = concatMap expandJoin $ sqJoins sq
        expandJoin j = fromMaybe [(Nothing, j)] $ do
            c <- classLookup (modClasses m) $ entityRefName $ joinEntity j
            Just $ [ 
                    let a = joinAlias j
                        a' = joinAlias j ++ "_" ++ entityName e
                    in (Just (a, (a', Just $ entityName e)), j {
                        joinAlias = a',
                        joinEntity = (Left $ entityName e),
                        joinExpr = joinExpr j >>= Just . (everywhere $ (mkT $ trClassField (entityName e)) . (mkT $ trVar a a'))
                    }) | e <- modEntities m, className c `elem` entityInstances e
                ]
        
        trClassField en fr = case fr of
            SqlField v'@(Var vn _ _) fn -> fromMaybe fr $ do
                (en',_) <- L.find ((==vn) . snd) aliases
                f <- lookupField' m en' (lowerFirst en ++ upperFirst fn)
                Just $ SqlField v' $ fieldName f
            _ -> fr    
                
        trVar srcVn dstVn fr = case fr of
            SqlField (Var vn _ _) fn -> if vn == srcVn then SqlField (Var dstVn (Left "") False) fn else fr
            SqlId (Var vn _ _) -> if vn == srcVn then SqlId (Var dstVn (Left "") False) else fr
            _ -> fr
        aliasName :: FieldName -> Maybe VariableName -> Maybe EntityName -> Maybe VariableName    

        aliasName fn man (Just en) = Just $ fromMaybe (fn ++ en) $ man >>= Just . (++en)
        aliasName fn man Nothing = man
    
        trSelectField sf = 
            case sf of
                SelectAllFields (Var vn _ _) -> [
                        SelectAllFields (Var vn' (Left "") False)
                        | (vn',_) <- newAliases vn
                    ]
                SelectField (Var vn _ _) fn man -> [
                        SelectField (Var vn' (Left "") False) fn $ aliasName fn man men
                        | (vn',men) <- newAliases vn,
                          validField (vn',fn)
                    ]
                SelectIdField (Var vn _ _) man -> [
                        SelectIdField (Var vn' (Left "") False) $ aliasName "id" man men
                        | (vn',men) <- newAliases vn
                    ]
                SelectValExpr _ _ -> [sf]  

        trExpr e = 
            let r = catMaybes [
                        let e' = everywhere (mkT $ trVar s d) e
                        in if e' /= e && validExpr e' then Just e' else Nothing
                        | (s,(d,_)) <- vnMap
                    ] 
                in if null r then e else foldl1 OrExpr r
                      
        validExpr e = let fs = [ (vn,fn) | SqlField (Var vn _ _) fn <- universeBi e ]
                      in all validField fs   
        validField (vn,fn) = fromMaybe False $ do
            (en,_) <- L.find ((==vn) . snd) allAliases
            e <- lookupField' m en fn
            Just True

classLookup :: [Class] -> ClassName -> Maybe Class
classLookup classes name =  find (\i -> name == className i) classes


expandClassField :: Module -> ClassName -> Entity ->  Field -> [Field]
expandClassField m cn e f@(Field _ _ internal _ (EntityField iName) _) 
    | not $ fieldOptional f = error $ show (entityLoc e) ++ ": non-maybe reference to class not allowed"
    | otherwise = [ mkField re | re <- modEntities m,  
                                 iName `elem` (entityInstances re) ]
    where mkField re = Field {
            fieldLoc = fieldLoc f,
            fieldOptional = True,
            fieldInternal = internal,
            fieldName = lowerFirst (entityName re) ++ upperFirst (fieldName f),
            fieldContent = EntityField (entityName re),
            fieldClassName = Just (cn, fieldName f)
        } 
expandClassField _ _ _ _ = []

expandClassRefFields :: Module -> Entity -> Field -> [Field]
expandClassRefFields m e f = expand (fieldContent f)
    where       
        expand (EntityField "ClassInstance") = [ 
                f { 
                    fieldContent = EntityField (entityName e)
                }
            ]
        expand (EntityField name) = case classLookup (modClasses m) name of
            Just _ -> expandClassField m name e f 
            Nothing -> [f]
        expand _ = [f]                           
            
implInEntity :: Module -> [Class] -> Entity -> Entity
implInEntity m classes' e = e { 
        entityFields  = concatMap (expandClassRefFields m e) $ 
                            entityFields e ++ extraFields,
        entityClassFields = filter isClassField $ entityFields e,
        entityUniques = entityUniques e ++ (map (addEntityNameToUnique e) $ concatMap classUniques validClasses)
    }
    where
        instances = entityInstances e
        classes = sortBy (\c1 c2 -> maybeCompare (elemIndex (className c1) instances) 
                                                 (elemIndex (className c2) instances))
                         classes'
        maybeCompare (Just a1) (Just a2) = compare a1 a2
        maybeCompare (Just _) Nothing = Prelude.LT
        maybeCompare Nothing (Just _) = Prelude.GT
        maybeCompare Nothing Nothing = Prelude.EQ
        validClasses = mapMaybe (classLookup classes) $ entityInstances e
        extraFields = concatMap classFields validClasses
        isClassField (Field _ _ _ _ (EntityField iName) _) = iName `elem` (map className $ modClasses m)
        isClassField _ = False
        addEntityNameToUnique e (Unique name fields) = Unique (entityName e ++ name) fields
        
    
