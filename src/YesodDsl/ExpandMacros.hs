module YesodDsl.ExpandMacros (expandMacros) where
import YesodDsl.AST
import Data.List
import Data.Generics

expandMacros :: Module -> Module
expandMacros m = everywhere (mkT f) m 
    where
        f (ApplyExpr fn ps) = expandApplyExpr fn ps
        f x = x
        expandApplyExpr fn ps = case find (\d -> defineName d == fn) (modDefines m) of
            Just d -> if length (defineParams d) == length ps
                then case defineContent d of
                    (DefineSubQuery sq) -> SubQueryExpr (expandSubQuery sq $ zip (defineParams d) ps)
                else error $ "Expected " ++ show (length $ defineParams d)
                             ++ " parameters for macro " ++ fn ++ " got " ++
                             show (length ps)
            _ -> error $ "Reference to undefined macro " ++ fn
                        
        expandSubQuery sq subs = foldl repSubQuery sq subs
        repSubQuery sq sub = everywhere (mkT $ repFr sub) $ everywhere (mkT $ repSf sub) sq 
        repSf (pn,pv) spf@(SelectParamField vn pn' mvn) 
            | pn == pn' = SelectField vn pv mvn
            | otherwise = spf
        repSf _ x = x    
        repFr (pn,pv) fr@(FieldRefParamField vn pn') 
            | pn == pn' = FieldRefNormal vn pv
            | otherwise = fr
        repFr _ x  =x  
