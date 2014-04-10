module ExpandMacros (expandMacros) where
import AST
import Data.List
expandMacros :: Module -> Module
expandMacros m = m {
        modRoutes = map fRoute $ modRoutes m
    }
    where
        fRoute r = r {
            routeHandlers = map fHandler $ routeHandlers r
        }

        fHandler h = h {
            handlerParams = map fHandlerParam $ handlerParams h
        }

        fHandlerParam (Select sq) = Select (fSelectQuery sq)
        fHandlerParam (Require sq) = Require (fSelectQuery sq)
        fHandlerParam (DeleteFrom en vn (Just e)) = DeleteFrom en vn (Just $ fExpr e)
        fHandlerParam hp = hp

        fSelectQuery sq = sq {
            sqJoins = map fJoin $ sqJoins sq,
            sqWhere = maybe Nothing (Just . fExpr) $ sqWhere sq
        }
        fJoin j = j {
            joinExpr = maybe Nothing (Just . fExpr) $ joinExpr j
        }
        fExpr (AndExpr e1 e2) = AndExpr (fExpr e1) (fExpr e2)
        fExpr (OrExpr e1 e2) = OrExpr (fExpr e1) (fExpr e2)
        fExpr (NotExpr e) = NotExpr (fExpr e)
        fExpr (BinOpExpr ve1 bo ve2) = BinOpExpr (fValExpr ve1) bo
                                                 (fValExpr ve2)
        fValExpr (ConcatExpr ve1 ve2) = ConcatExpr (fValExpr ve1) (fValExpr ve2)
        fValExpr (ConcatManyExpr ves) = ConcatManyExpr $ map fValExpr ves
        fValExpr (ValBinOpExpr ve1 op ve2) = ValBinOpExpr (fValExpr ve1) op (fValExpr ve2)
        fValExpr (FloorExpr ve) = FloorExpr (fValExpr ve)
        fValExpr (CeilingExpr ve) = CeilingExpr (fValExpr ve)
        fValExpr (ExtractExpr fn ve) = ExtractExpr fn (fValExpr ve)
        fValExpr (SubQueryExpr sq) = SubQueryExpr $ fSelectQuery sq
        fValExpr (ApplyExpr fn ps) = expandApplyExpr fn ps
        fValExpr ve = ve


        expandApplyExpr fn ps = case find (\d -> defineName d == fn) (modDefines m) of
            Just d -> if length (defineParams d) == length ps
                then case defineContent d of
                    (DefineSubQuery sq) -> SubQueryExpr (expandSubQuery sq $ zip (defineParams d) ps)
                else error $ "Expected " ++ show (length $ defineParams d)
                             ++ " parameters for macro " ++ fn ++ " got " ++
                             show (length ps)
            _ -> error $ "Reference to undefined macro " ++ fn
                        

        expandSubQuery sq subs = foldl repSubQuery sq subs
        repSubQuery sq sub = sq {
            sqFields = map (repSelectField sub) $ sqFields sq
        }
        repSelectField (pn,pv) spf@(SelectParamField vn pn' mvn) 
            | pn == pn' = SelectField vn pv mvn
            | otherwise = spf
        repSelectField _ sf = sf
