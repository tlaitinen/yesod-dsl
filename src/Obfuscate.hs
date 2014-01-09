module Obfuscate where
import Data.Hash.MD5
import AST
import Data.Char
hash :: String -> String
hash (s@(x:xs))
   | isUpper x = "I" ++ h
   | otherwise = "i" ++ h
    where h = (md5s (Str s))

obfuscate :: [(FilePath,Module)] -> [(FilePath,Module)]
obfuscate dbs = [ (fp,f m) | (fp,m) <- dbs ]
    where 
    f m = m {
        modEntities = [ oEntity e | e <- modEntities m ],
        modClasses = [ oClass c | c <- modClasses m ],
        modEnums = [ oEnum e | e <- modEnums m ],
        modRoutes = map oRoute $ modRoutes m
    }
    oEntity e = e {
        entityName = hash $ entityName e,
        entityInstances = [ hash i | i <- entityInstances e],
        entityFields = map oField $ entityFields e,
        entityUniques = map oUnique $ entityUniques e,
        entityChecks = map hash $ entityChecks e
    }
    oField f = f { 
        fieldName = hash $ fieldName f,
        fieldContent = case (fieldContent f) of
            NormalField _ _ -> fieldContent f
            EntityField en  -> EntityField (hash en)    
            EnumField en    -> EnumField (hash en)
    }
    oClass c = c {
        className = hash $ className c,
        classFields = map oField $ classFields c,
        classUniques = map oUnique $ classUniques c
    }
    oUnique u = u {
        uniqueName = hash $ uniqueName u,
        uniqueFields = map hash $ uniqueFields u                   
    }
    oEnum e = e {
        enumName = hash $ enumName e,
        enumValues = map hash $ enumValues e
    }
    oRoute r = r {
        routePath = map oPathPiece $ routePath r,
        routeHandlers = map oHandler $ routeHandlers r                  
    }
    oPathPiece (PathText s) = PathText $ hash s
    oPathPiece (PathId en) = PathId $ hash en

    oHandler h = h {
        handlerParams = map oParam $ handlerParams h
    }
    oParam (Select sq) = Select (oSelectQuery sq)
    oParam (IfFilter (pn,joins,expr, useFlag)) = IfFilter (hash pn, map oJoin joins,oExpr expr, useFlag)
    oParam (DeleteFrom en vn expr) = DeleteFrom (hash en) vn (maybe Nothing (Just . oExpr) expr)
    oParam (GetById en ifr vn) = GetById (hash en) (oInputFieldRef ifr) vn
    oParam (Update en ifr ifs) = Update (hash en) (oInputFieldRef ifr)
                       (maybe Nothing (Just . (map oInputField)) ifs)
    oParam (Insert en ifs vn) = Insert (hash en) (maybe Nothing (Just . (map oInputField)) ifs) vn
    oParam (Return ofs) = Return ofs
    oParam (Require sq) = Require $ oSelectQuery sq
    oParam p = p
    oSelectQuery sq = sq {
        sqFields = map oSelectField $ sqFields sq,
        sqFrom = (hash en, vn),
        sqJoins = map oJoin $ sqJoins sq,
        sqWhere = maybe Nothing (Just . oExpr) (sqWhere sq),
        sqOrderBy = [ (oFieldRef fr, sd) | (fr,sd) <- sqOrderBy sq ]
    }
        where (en,vn) = sqFrom sq
    oSelectField (SelectField vn fn mvn) = SelectField vn (hash fn) mvn
    oSelectField sf = sf    
    oJoin join = join {
        joinEntity = hash $ joinEntity join,
        joinExpr = maybe Nothing (Just . oExpr) (joinExpr join)                   
    }        
    oExpr (AndExpr e1 e2) = AndExpr (oExpr e1) (oExpr e2)
    oExpr (NotExpr e) = NotExpr (oExpr e)
    oExpr (OrExpr e1 e2) = OrExpr (oExpr e1) (oExpr e2)
    oExpr (ListOpExpr fr1 lo fr2) = ListOpExpr (oFieldRef fr1) lo (oFieldRef fr2)
    oExpr (BinOpExpr ve1 op ve2) = BinOpExpr (oValExpr ve1) op (oValExpr ve2)

    oValExpr (FieldExpr fr) = FieldExpr (oFieldRef fr)
    oValExpr (ConcatExpr ve1 ve2) = ConcatExpr (oValExpr ve1) (oValExpr ve2)
    oValExpr ve = ve
    oFieldRef (FieldRefNormal vn fn) = FieldRefNormal vn (hash fn)
    oFieldRef (FieldRefEnum en fn) = FieldRefEnum (hash en) (hash fn)
    oFieldRef (FieldRefSubQuery sq) = FieldRefSubQuery (oSelectQuery sq)
    oFieldRef fr = fr
    oInputField (pn,ifr) = (hash pn,oInputFieldRef ifr)
    oInputFieldRef (InputFieldNormal fn) = (InputFieldNormal $ hash fn)
    oInputFieldRef (InputFieldLocalParamField vn fn) = InputFieldLocalParamField vn (hash fn)
    oInputFieldRef ifr = ifr

