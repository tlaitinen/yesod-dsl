module ASTToInt (astToIntermediate) where

import qualified AST as A
import qualified Intermediate as I
import Data.List
import Data.Maybe

import Control.Monad.Error

class CE a where
    getName :: a -> String
    getLoc  :: a -> A.Location

instance CE A.Entity where
    getName   = A.entityName
    getLoc    = A.entityLoc

instance CE A.Class where
    getName   = A.className
    getLoc    = A.classLoc

fieldLoc :: CE a => a -> A.Field -> String
fieldLoc e (A.Field _ fn _) = getName e ++ "." 
                         ++ fn ++ " in " ++ (show $ getLoc e) 

lookupFieldType :: CE a => A.Module -> a -> A.Field -> Either String I.FieldType
lookupFieldType m e f@(A.Field _ _ (A.NormalField ft _)) = Right $ I.DataField ft 
lookupFieldType m e f@(A.Field o _ (A.EntityField n))
    | isJust enum = Right $ I.EnumField (fromJust enum)
    | n `elem` (map A.className (A.modClasses m)) = 
        if o then Right $ I.CRefField n
             else Left $ "Non-maybe references to classes are not allowed: "
                         ++ fieldLoc e f
    | n `elem` (map A.entityName (A.modEntities m)) = Right $ I.ERefField n
    | otherwise = Left $ "Reference to an undefined entity or enum: "
                         ++ fieldLoc e f
    where enum = find (\en -> A.enumName en == n) (A.modEnums m)                     

getFieldDefault :: CE a => a -> A.Field -> Either String (Maybe A.FieldValue)
getFieldDefault e f 
    | length defs == 0 = Right Nothing
    | length defs == 1 = Right $ Just $ (\(A.FieldDefault d) -> d) . head $ defs
    | otherwise = Left $ "Multiple default values specified: " 
                      ++ fieldLoc e f
    where
        opts = A.fieldOptions f
        defs = filter isDefault opts
        isDefault (A.FieldDefault _) = True
        isDefault _ = False


mapField :: CE a => A.Module -> a -> A.Field -> Either String I.Field
mapField m e f = do
    ft <- lookupFieldType m e f
    d <- getFieldDefault e f
    return $ I.Field {
        I.fieldOptional = A.fieldOptional f,
        I.fieldName     = A.fieldName f,
        I.fieldType     = ft,
        I.fieldChecks   = getChecks (A.fieldOptions f),
        I.fieldDefault  = d
    }
    where getChecks = (map (\(A.FieldCheck f) -> f)) . (filter isCheck) 
          isCheck (A.FieldCheck _) = True
          isCheck _ = False

lookupField :: String -> [I.Field] -> String -> Either String I.Field
lookupField loc fields name = case find (\f -> I.fieldName f == name) fields of
    Just f -> Right f
    Nothing -> Left $ "Reference to an undefined field: " ++ name ++ " in " ++ loc
          
mapUnique :: CE a => A.Module -> a -> [I.Field] -> A.Unique -> Either String I.Unique
mapUnique m e fs (A.Unique u names) = do
    fields <- mapM (lookupField (getName e ++ " unique " ++ u 
                                  ++ " in " ++ (show $ getLoc e)) fs) names
    return $ I.Unique u fields

mapClass :: A.Module -> A.Class -> Either String I.Class
mapClass m c = do
    fields <- mapM (mapField m c) (A.classFields c)
    uniques <- mapM (mapUnique m c fields) (A.classUniques c)
    return $ I.Class {
        I.classLoc = A.classLoc c,
        I.className = A.className c,
        I.classFields = fields,
        I.classUniques = uniques
    }

mapPathPiece :: A.Module -> String -> [I.Entity] -> A.PathPiece -> Either String I.PathPiece
mapPathPiece _ _ _ (A.PathText s) = I.PathText s
mapPathPiece m loc es (A.PathId en) = case find (\e -> I.entityName e == en) es of
    Just e -> Right $ I.PathId e
    Nothing -> Left $ "Reference to undefined entity in route: " ++ loc

mapHandler :: A.Module -> String -> [I.Entity] -> A.Handler -> Either String I.Handler
mapHandler m loc es (A.Handler ht ps) = (mapM (checkParam ht) ps) >> (f ht)
    where 
          allowed _ A.Public = True
          allowed _ A.PreHook = True
          allowed _ A.PreHook = True
          allowed A.PutService (A.PreTransform _)= True
          allowed A.PostService (A.PreTransform _)= True
          allowed A.GetService (A.PostTransform _)= True
          allowed A.GetService A.DefaultFilterSort = True
          allowed A.GetService (A.TextSearchFilter _ _) = True
          allowed A.GetService (A.Join _ _ _ _) = True
          allowed A.GetService (A.Where _) = True
          allowed A.GetService (A.SortBy _) = True
          allowed A.GetService (A.SelectFrom _ _) = True
          allowed _ _ = False

          checkParam ht p = if allowed ht p 
                                then Right () 
                                else Left $ show p ++ " not allowed in " 
                                            ++ (show ht) ++ ": " ++ loc
          mapTextSearch (A.TextSearchFilter param fs) = do
              refs <- mapM mapFieldRef fs  
              return $ Just (param, refs)
          mapTextSearch _ = return Nothing
       

          lookupEntity en = case find (\e -> I.entityName e == en) es of
                Just e -> Right e
                Nothing -> Left $ "Reference to an undefined entity " ++ en 
                                + " in " ++ (show ht) ++ ": " ++ loc
               
                                
          getSelectFrom = case find isSelectFrom ps of
                Just (SelectFrom en v) -> do
                    e <- lookupEntity en
                    return $ (e, v)
                Nothing -> Left $ "Missing 'select from' in " ++
                                 (show ht) ++ ": " ++ loc
                                 
          lookupAliasedEntity a = do
              (e, a') <- getSelectFrom
              if a' == a 
                  then return e
                  else lookupJoinEntity a
                    
          joins = filter isJoin ps
          isJoin (A.Join _ _ _ _) = True
          isJoin _ = False

          lookupJoinEntity a = case find (\(Join _ _ a' _) -> a' == a) joins of
              Just (Join _ en _ _) -> lookupEntity en
              Nothing -> Left $ "Reference to an undefined Entity-alias "
                              ++ a ++ " in " ++ (show ht) ++  ": " ++ loc
                        
          lookupEntityField e fn = case find (\f -> I.fieldName f == fn) 
                                                  (I.entityFields e) of
                Just f -> Right f
                Nothing -> Left $ "Reference to an undefined field "
                                  ++ fn ++ " in entity " ++ (I.entityName e)
                                ++ " in " ++ (show ht) ++ " 'where': " ++ loc
 
          mapFieldRef (A.FieldRefId en) = do
              e <- lookupAliasedEntity  en
              return $ I.FieldRefId e
          mapFieldRef (A.FieldRefNormal en fn) = do
              e <- lookupAliasedEntity  en
              f <- lookupEntityField  e fn
                  

          mapValExpr (A.FieldExpr ref) = do
              ref' <- mapFieldRef ref
              return $ I.FieldExpr ref'
          mapValExpr (A.ConstExpr fv) = do
              return $ I.ConstExpr fv

          mapExpr (A.AndExpr lhs rhs) = do
              lhs' <- mapExpr lhs
              rhs' <- mapExpr rhs
              return $ I.AndExpr lhs' rhs'
          mapExpr (A.OrExpr lhs rhs) = do
              lhs' <- mapExpr lhs
              rhs' <- mapExpr rhs
              return $ I.OrExpr lhs' rhs'
          mapExpr (A.BinOpExpr lhs rhs) = do
              lhs' <- mapValExpr lhs
              rhs' <- mapValExpr rhs
              return $ I.BinOpExpr lhs' rhs'

          mapWhere (A.Where e) = do
              e' <- mapExpr e                       
              return $ Just e'
          mapWhere _ = Right $ Nothing 

          mapJoinExpr (Just (lhs, op, rhs)) = do
              lhs' <- mapFieldRef lhs
              rhs' <- mapFieldRef rhs
              return $ Just (lhs', op, rhs')
          mapJoinExpr Nothing = return Nothing
          mapJoin (A.Join jt en vn je) = do
              e <- lookupEntity en
              je' <- mapJoinExpr
              return $ I.Join {
                  I.joinType = jt,
                  I.joinEntity = e,
                  I.joinAlias = vn,
                  I.joinExpr = je'
              }
          f A.GetHandler = do
              textSearchFilters <- mapMaybeM mapTextSearch ps
              selectFrom <- getSelectFrom
              joins' <- mapM mapJoin joins
              wheres <- mapM mapWhere ps 
              orderBy <- mapM mapOrderBy ps
              return $ I.GetHandler (I.GetHandlerParams {
                         I.ghPublic = public,
                         I.ghDefaultFilterSort = A.DefaultFilterSort `elem` ps,
                         I.ghTextSearchFilter = textSearchFilters,
                         I.ghSelectFrom = selectFrom,
                         I.ghJoins = joins'
                         I.ghWhere = wheres,
                         I.ghPostTransforms = postTransforms,
                         I.ghOrderBy = orderBy,
                         I.ghPreHooks = preHooks,
                         I.ghPostHooks = postHooks
                      })
          f A.PutHandler = do
              e <- getEntity
              return $ I.PutHandler {
                  I.puthPublic = public,
                  I.puthPreTransforms = preTransforms,
                  I.puthPreHooks = preHooks,
                  I.puthPostHooks = postHooks,
                  I.puthEntity = e
              }
          f A.PostHandler = do
              e <- getEntity
              return $ I.PostHandler {
                  I.posthPublic = public
                  I.posthPreTransforms = preTransforms,
                  I.posthPreHooks = preHooks,
                  I.posthPostHooks = postHooks,
                  I.posthEntity = e
              }
          f A.DeleteHandler = do
              e <- getEntity
              return $ I.DeleteHandler {
                  I.dhPublic = public,
                  I.dhPreHooks = preHooks,
                  I.dhPostHooks = postHooks,
                  I.dhEntity = e
              }
          public = A.PublicService `elem` ps
          preTransforms = map (\A.PreTransform f -> f) $filter isPreTransform ps
          isPreTransform (A.PreTransform _) = True
          isPreTransform _ = False
          postTransforms = map (\A.PostTransform f -> f) $filter isPostTransform ps
          isPostTransform (A.PostTransform _) = True
          isPostTransform _ = False
    

    
mapResource :: A.Module -> [I.Entity] -> A.Resource -> Either String I.Resource
mapResource m es r = doA
    let loc = "resource " ++ A.resourceName r ++ " in " 
            ++ (show $ A.resourceLoc r)
    route <- mapM (mapPathPiece m loc es) (A.resRoute r)
    handlers <- mapM (mapHandler m loc es) (A.resHandlers r)
    return $ I.Resource {
        I.resLoc      = A.resLoc r,
        I.resRoute    = route,
        I.resHandlers = handlers
    }

astToIntermediate :: A.Module -> Either String I.Module
astToIntermediate m = do
    classes <- mapM (mapClass m) (A.modClasses m)
    entities <- mapM (mapEntity m) (A.modEntities m)
    resources <- mapM (mapResource m entities) (A.modResources m) 
    return $ I.Module {
        I.modClasses = classes,
        I.modEnums = A.modEnums m,
        I.modEntities = entities,
        I.modResources = resources
    }
    


