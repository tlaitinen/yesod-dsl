module YesodDsl.Simplify (simplify) where

import YesodDsl.AST
import Data.Generics
import qualified Data.List as L
import Data.Maybe

simplify :: Module -> Module
simplify m = everywhere ((mkT sValExpr) . (mkT sBoolExpr)
                         . (mkT sStmt) . (mkT mapEntityRef)) m
    where
        sValExpr (SubQueryExpr sq) = SubQueryExpr $ mapSq sq
        sValExpr x = x

        sBoolExpr (ExistsExpr sq) = ExistsExpr $ mapSq sq
        sBoolExpr x = x
        sStmt (Require sq) = Require $ mapSq sq
        sStmt (IfFilter (pn,js,be,uf)) = IfFilter (pn, map mapJoin js, be, uf)
        sStmt (Select sq) = Select $ mapSq sq
        sStmt x = x

        mapSq sq = let sq' = sq {
                sqJoins = map mapJoin $ sqJoins sq
               } in sq' {
                sqFields = concatMap (expand sq') $ sqFields sq',
                sqWhere = everywhere ((mkT sValExpr) . (mkT sBoolExpr)) $ sqWhere sq'
            }
        mapJoin j =  j { 
                joinEntity = mapEntityRef $ joinEntity j,
                joinExpr = joinExpr j >>= Just . (everywhere $ (mkT sValExpr) . (mkT sBoolExpr))
            }

        mapEntityRef l@(Left en) = fromMaybe l $ lookupEntity m en >>= Just . Right
        mapEntityRef x = x    
        expand sq (SelectAllFields (Var vn _ _)) = fromMaybe [] $ do
            (e,_,mf) <- L.find (\(_,vn',_) -> vn == vn') (sqAliases sq)
            Just $ [
                    SelectField (Var vn Nothing False) (fieldName f) Nothing
                    | f <- entityFields e, fieldInternal f == False
                ]
        expand _ x = [x]         
