module YesodDsl.Simplify (simplify) where

import YesodDsl.AST
import Data.Generics
import qualified Data.List as L
import Data.Maybe

simplify :: Module -> Module
simplify m = everywhere ((mkT sValExpr) . (mkT sBoolExpr)
                         . (mkT sHandlerParam) . (mkT mapEntityRef)) m
    where
        sValExpr (SubQueryExpr sq) = SubQueryExpr $ mapSq sq
        sValExpr x = x

        sBoolExpr (ExistsExpr sq) = ExistsExpr $ mapSq sq
        sBoolExpr x = x
        sHandlerParam (Require sq) = Require $ mapSq sq
        sHandlerParam (IfFilter (pn,js,be,uf)) = IfFilter (pn, map mapJoin js, be, uf)
        sHandlerParam (Select sq) = Select $ mapSq sq
        sHandlerParam x = x

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
        expand sq (SelectAllFields vn) = fromMaybe [] $ do
            (e,_,mf) <- L.find (\(_,vn',_) -> vn == vn') (sqAliases sq)
            Just $ [
                    SelectField vn (fieldName f) Nothing
                    | f <- entityFields e, fieldInternal f == False
                ]
        expand _ x = [x]         
