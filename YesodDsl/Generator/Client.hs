module YesodDsl.Generator.Client where
import YesodDsl.AST
import Data.Maybe
import YesodDsl.Generator.Input

mkField :: FieldName -> (Bool,FieldContent) -> Field
mkField n (o,c) = Field (Loc "" 0 0) o n c [] Nothing

handlerInputFields :: Handler -> [(FieldName, Maybe Field)]
handlerInputFields h = nubAttrs $ concatMap requestAttrs $ handlerStmts h

handlerOutputFields :: Module -> Handler ->  [Field]
handlerOutputFields m h = concatMap stmtOutputs $ handlerStmts h
    where
        stmtOutputs s = case s of
            Select sq -> mapMaybe selectFieldToField $ sqFields sq
            Return ofs -> mapMaybe (\(pn,fr,_) -> fieldRefToContent fr >>= Just . (mkField pn)) ofs
            _ -> []
        selectFieldToField sf = case sf of
            SelectField (Var _ (Right e) mf) fn mvn -> do
                f <- lookupField e fn
                let f' = f { fieldOptional = fieldOptional f || mf }
                Just $ case mvn of
                    Just vn -> f' { fieldName = vn }
                    Nothing -> f'
            SelectIdField (Var _ (Right e) mf) mvn -> Just $ 
                mkField (fromMaybe "id" mvn) (mf, EntityField $ entityName e)
            SelectValExpr ve vn -> do
                fc <- case ve of
                    FieldExpr fr -> fieldRefToContent fr
                    ValBinOpExpr _ op _ -> Just $ if op `elem` [Add,Sub,Div,Mul]
                        then (False, NormalField FTDouble)
                        else (False, NormalField FTText)
                    RandomExpr -> Just (False, NormalField FTDouble)
                    FloorExpr _ -> Just (False, NormalField FTDouble)
                    CeilingExpr _ -> Just (False, NormalField FTDouble)
                    ExtractExpr _ _ -> Just (False, NormalField FTDouble)
                    ConcatManyExpr _ -> Just (False, NormalField FTText)
                    _ -> Nothing
                Just $ mkField vn fc
            _ -> Nothing    
        fieldRefToContent fr = case fr of
            SqlId (Var _ (Right e) mf)     -> Just (mf, EntityField $ entityName e)
            SqlField (Var _ (Right e) mf) fn -> do
                f <- lookupField e fn
                Just $ (fieldOptional f || mf, fieldContent f)
            AuthId -> Just (False, EntityField "User")
            AuthField fn -> listToMaybe [ (fieldOptional f, fieldContent f)
                                          | e <- modEntities m,   
                                            f <- entityFields e,  
                                            entityName e == "User",
                                            fieldName f == fn ]
            LocalParamField (Var _ (Right e) mf) fn -> do
                f <- lookupField e fn
                Just $ (fieldOptional f || mf, fieldContent f)
            _ -> Nothing
 
