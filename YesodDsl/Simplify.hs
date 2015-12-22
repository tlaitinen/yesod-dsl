module YesodDsl.Simplify (simplify) where

import YesodDsl.AST
import Data.Generics
import Data.Either
import Data.Generics.Uniplate.Data
import qualified Data.List as L
import Data.Maybe
import qualified Data.Map as Map


simplify :: Module -> Module
simplify m = everywhere ((mkT sHandler) . (mkT sExpr) 
                         . (mkT sStmt) . (mkT mapEntityRef)) m
    where
        sExpr (SubQueryExpr sq) = SubQueryExpr $ mapSq sq
        sExpr (ExistsExpr sq) = ExistsExpr $ mapSq sq
        sExpr x = x
        sStmt (Require sq) = Require $ mapSq sq
        sStmt (IfFilter (pn,js,be,ob,uf)) = IfFilter (pn, map mapJoin js, be, ob, uf)
        sStmt (Select sq) = Select $ mapSq sq
        sStmt x = x

        mapSq sq = let sq' = sq {
                sqJoins = map mapJoin $ sqJoins sq
               } in sq' {
                sqFields = concatMap (expand sq') $ sqFields sq',
                sqWhere = everywhere (mkT sExpr) $ sqWhere sq'
            }
        mapJoin j =  j { 
                joinEntity = mapEntityRef $ joinEntity j,
                joinExpr = joinExpr j >>= Just . (everywhere $ (mkT sExpr))
            }

        lookupEntity en = L.find ((==en) . entityName) $ modEntities m
        mapEntityRef l@(Left en) = fromMaybe l $ lookupEntity en >>= Just . Right
        mapEntityRef x = x    
        expand sq (SelectField vr@(Var vn _ _) fn Nothing) = fromMaybe [] $ do
            (e,_) <- Map.lookup vn $ sqAliases sq
            Just $ [ 
                    SelectField vr fn (Just $ fieldJsonName f)
                    | f <- entityFields e, fieldName f == fn
                ]
        expand sq (SelectAllFields (Var vn _ _)) = fromMaybe [] $ do
            (e,_) <- Map.lookup vn $ sqAliases sq
            Just $ [
                    SelectField (Var vn (Left "") False) (fieldName f) (Just $ fieldJsonName f) 
                    | f <- entityFields e, fieldInternal f == False
                ]
        expand _ x = [x]         


sHandler :: Handler -> Handler
sHandler h = everywhere ((mkT mapVarRef) . (mkT mapStmt) . (mkT mapSq)) h
    where
        baseAliases = Map.unions [ sqAliases sq | Select sq <- universeBi h ]
        mapStmt df@(DeleteFrom er vn _) = everywhere (mkT $ mapSqVarRef $ Map.unions [ baseAliases, Map.fromList $ rights [ er >>= \e -> Right (vn, (e, False)) ] ]) df
        mapStmt i@(IfFilter (_,js,_,_,_)) = everywhere (mkT $ mapSqVarRef $ Map.unions [ baseAliases, Map.fromList $ rights [  joinEntity j >>= \e -> Right (joinAlias j,(e, isOuterJoin $ joinType j)) | j <- js ] ]) i
        mapStmt i = i 
        mapSq sq = everywhere (mkT $ mapSqVarRef $ sqAliases sq) sq
        mapSqVarRef aliases (Var vn (Left "") _) = case Map.lookup vn aliases of
            Just (e,mf) -> Var vn (Right e) mf
            _ -> Var vn (lookupEntityRef vn) False
        mapSqVarRef _ v = v
        lookupEntityRef vn = case listToMaybe [ er | GetById er _ vn' <- universeBi h, vn' == vn ] of
            Just er -> er
            Nothing -> Left ""

        mapVarRef (Var vn (Left "") _) = Var vn (lookupEntityRef vn) False
        mapVarRef v = v

