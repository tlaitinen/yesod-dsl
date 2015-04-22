module YesodDsl.Simplify where

import YesodDsl.AST
import Data.Generics
import qualified Data.List as L
import Data.Maybe

simplify :: Module -> Module
simplify m = everywhere ((mkT f2) . (mkT f)) m
    where
        f2 (SubQueryExpr sq) = SubQueryExpr $ mapSq sq
        f2 x = x
        f (Select sq) = Select $ mapSq sq
        f x = x
        mapSq sq = sq {
                sqFields = concatMap (expand sq) $ sqFields sq
            }
        expand sq (SelectAllFields vn) = fromMaybe [] $ do
            (en,_,mf) <- L.find (\(_,vn',_) -> vn == vn') (sqAliases sq)
            e <- lookupEntity m en
            Just $ [
                    SelectField vn (fieldName f) Nothing
                    | f <- entityFields e, fieldInternal f == False
                ]
        expand _ x = [x]         
