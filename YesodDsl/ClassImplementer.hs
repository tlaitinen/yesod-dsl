{-# LANGUAGE TupleSections #-}
module YesodDsl.ClassImplementer (implementClasses) where
import Data.List
import YesodDsl.AST
import Data.Maybe
import Data.Generics
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map
import qualified Data.List as L
import Debug.Trace


lookupField' :: Module -> EntityName -> FieldName -> Maybe Field
lookupField' m en fn = listToMaybe [ f | e <- modEntities m,
                                    f <- entityFields e,
                                    entityName e == en,
                                    fieldName f == fn ] 


implementClasses :: Module -> Module
implementClasses m = let m' = m {
        modEntities  = [ implInEntity m (modClasses m) e | e <- modEntities m ]
    } in m' {
        modRoutes = everywhere ((mkT $ trStmt m) . (mkT $ trSq m')) $ modRoutes m'
    }

trStmt :: Module -> Stmt -> Stmt
trStmt m s = case s of
    Update er@(Left en) fr (Just frm) -> Update er fr (Just $ concatMap (f en) frm)
    Insert er@(Left en) (Just (mvn1, frm)) mvn2 -> Insert er (Just (mvn1, concatMap (f en) frm)) mvn2
                   
    x -> x
    where
        f en x@(pn,RequestField fn,mfn) = if pn == fn 
                then fromMaybe [x] $ do
                    f <- lookupField' m en pn
                    case fieldContent f of
                        EntityField name -> do
                            c <- classLookup (modClasses m) name

                            Just $ [ 
                                    (lowerFirst (entityName e) ++ upperFirst pn,
                                     RequestField (lowerFirst (entityName e) ++ upperFirst pn),
                                     mfn) 
                                    | e <- modEntities m, 
                                      className c `elem` entityInstances e
                                ]
                        _ -> Nothing
                else [x]
        f _ x = [x]

            

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
                        joinExpr = joinExpr j >>= Just . (everywhere $ (mkT $ trDropInvalidExprs) . (mkT $ trClassField (entityName e)) . (mkT $ trVar a a'))
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
        aliasName _ man Nothing = man
    
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
                SelectExpr _ _ -> [sf]  

        trExpr e = 
            let r = catMaybes [
                        let e' = everywhere (mkT $ trVar s d) e
                        in if e' /= e && validExpr e' then Just e' else Nothing
                        | (s,(d,_)) <- vnMap
                    ] 
                in if null r then e else foldl1 mkOrExpr r
        mkOrExpr e1 e2 = BinOpExpr e1 Or e2              
        trueExpr = let c = (FieldExpr (Const (BoolValue True))) in BinOpExpr c Eq c
        trDropInvalidExprs e = 
            let  
                me = case e of
                    BinOpExpr e1 And e2 -> Just (e1,e2)
                    BinOpExpr e1 Or e2 -> Just (e1,e2)
                    _ -> Nothing
            in case me >>= \(e1,e2) -> Just  (validExpr e1, validExpr e2, e1, e2) of
                    Just (True, True, _, _) -> e
                    Just (False, True, _, e2) -> e2
                    Just (True, False, e1, _) -> e1
                    Just (False, False, _, _) -> trueExpr
                    Nothing -> e

        validExpr e = let fs = [ (vn,fn) | SqlField (Var vn _ _) fn <- universeBi e ]
                      in all validField fs   
        validField (vn,fn) = fromMaybe False $ do
            (en,_) <- L.find ((==vn) . snd) allAliases
            _ <- lookupField' m en fn
            Just True

classLookup :: [Class] -> ClassName -> Maybe Class
classLookup classes name =  find (\i -> name == className i) classes


expandClassField :: Module -> ClassName -> Entity ->  Field -> [Field]
expandClassField m cn e f@(Field _ _ _ (EntityField iName) opts _) 
    | not $ fieldOptional f = error $ show (entityLoc e) ++ ": non-maybe reference to class not allowed"
    | otherwise = [ mkField re | re <- modEntities m,  
                                 iName `elem` (entityInstances re) ]
    where mkField re = Field {
            fieldLoc = fieldLoc f,
            fieldOptional = True,
            fieldName = lowerFirst (entityName re) ++ upperFirst (fieldName f),
            fieldContent = EntityField (entityName re),
            fieldOptions = opts,
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
            
expandClassRefUniques :: Module -> Entity -> Unique -> [Unique]           
expandClassRefUniques m e u = expand [u] cFields
    where
        expand us (f:fs) = expand (concatMap (expandField f) us) fs
        expand us [] = us
        expandField f u 
            | fieldName f `elem` uniqueFields u = [ 
                    u { 
                        uniqueName = uniqueName u ++ fieldEntityName f',
                        uniqueFields = (uniqueFields u L.\\ [ fieldName f ] ) ++ [ fieldName f' ] 
                    }
                    | f' <- expandClassRefFields m e f
                ]
            | otherwise = [ u ] 
        fieldEntityName f = case fieldContent f of
            EntityField en -> en
            _ -> ""
        cFields = [ f | fn <- uniqueFields u, 
                         f <- entityFields e,
                         fieldName f == fn,
                         isClassField m f ]
         
isClassField :: Module -> Field -> Bool
isClassField m (Field _ _ _ (EntityField iName) _ _) = iName `elem` (map className $ modClasses m)
isClassField _ _ = False
implInEntity :: Module -> [Class] -> Entity -> Entity
implInEntity m classes' e = e { 
        entityFields  = concatMap (expandClassRefFields m e) $ 
                            entityFields e ++ extraFields,
        entityClassFields = filter (isClassField m) $ entityFields e,
        entityUniques = concatMap (expandClassRefUniques m e) $ entityUniques e ++ (map addEntityNameToUnique $ concatMap classUniques validClasses)
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
        addEntityNameToUnique (Unique name fields) = Unique (entityName e ++ name) fields
        
    
