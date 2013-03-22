module Generator (generateModels) where
import System.IO (FilePath)
import DbTypes
import DbLexer
import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils

-- from Database.Persist.TH
recName :: String -> String -> String
recName dt f = lowerFirst dt ++ upperFirst f

lowerFirst :: String -> String
lowerFirst (a:b) = (toLower a):b
lowerFirst a = a

upperFirst :: String -> String
upperFirst (a:b) = (toUpper a):b
upperFirst a = a
-- ^^^^ Database.Persist.TH        
entityFieldDeps :: DbModule -> String -> [String]
entityFieldDeps db name 
    | name `elem` [ entityName entity | entity <- dbEntities db ] = [name]
    | otherwise = [name ++ "Inst", name ++ "InstRef"]

getFieldDeps :: DbModule -> Field -> [String]
getFieldDeps db field = case (fieldContent field) of
    (NormalField _ _) -> []
    (EntityField entityName) -> entityFieldDeps db entityName

lookupDeps :: DbModule -> String -> [String]
lookupDeps db name = concatMap (getFieldDeps db) $ (dbdefFields . (dbLookup db)) name

genUnique :: Unique -> String
genUnique (Unique name fields) = "Unique" ++ name ++ " " ++ intercalate " " fields

genFieldType :: DbModule -> Field -> String
genFieldType db field = case (fieldContent field) of
    (NormalField ftype _)   -> fromTkType ftype
    (EntityField entityName) -> entityName ++ "Id"
    where 
        fromTkType TWord32 = "Word32"
        fromTkType TWord64 = "Word64"
        fromTkType TInt32  = "Int32"
        fromTkType TInt64  = "Int64"
        fromTkType TText   = "Text"
        fromTkType TBool   = "Bool"
        fromTkType TDouble = "Double"
        fromTkType TTime   = "TimeOfDay"
        fromTkType TDate   = "Day"
        fromTkType TDateTime = "UTCTime"
        fromTkType TZonedTime = "ZonedTime"
        fromTkType ft = error $ "Unknown field type: " ++ show ft 

haskellFieldType :: DbModule -> Field -> String
haskellFieldType db field = (maybeMaybe (fieldOptional field)) ++ genFieldType db field 
        where
            maybeMaybe True = "Maybe "
            maybeMaybe False = ""

persistFieldType :: DbModule -> Field -> String
persistFieldType db field = genFieldType db field ++ (maybeMaybe (fieldOptional field))
        where
            maybeMaybe True = " Maybe "
            maybeMaybe False = " "

genField :: DbModule -> Field -> String
genField db field = fieldName field ++ " " ++ persistFieldType db field

genModel :: DbModule -> Entity -> String
genModel db entity = unlines $ [ entityName entity ++ " json"] 
                            ++ (indent $ (map (genField db) (entityFields entity))
                                    ++ (map genUnique (entityUniques entity)))

handlerName :: Entity -> String -> String
handlerName e name =  entityName e ++ name ++ "R"

genRoutes :: DbModule -> Entity -> [String]
genRoutes db e = manyHandler ++ oneHandler ++ validateHandler
    where
        services = [ t | (Service t _) <- entityServices e ] 
        getService 
            | GetService `elem` services = " GET"  
            | otherwise = ""
        postService
            | PostService `elem` services = " POST"
            | otherwise = ""
        putService 
            | PutService `elem` services = " PUT" 
            | otherwise = "" 
        deleteService
            | DeleteService `elem` services = " DELETE" 
            | otherwise = "" 
        manyHandler 
            | GetService `elem` services || PostService `elem` services
             =  ["/" ++ routeName e ++ " " ++ handlerName e "Many" ++ getService ++ postService]
            | otherwise =  []
        oneServices = getService ++ putService ++ deleteService
        oneHandler
            | (not . null) oneServices =  
               ["/" ++ routeName e ++ "/#" ++ entityName e ++ "Id" ++ " " 
                  ++ handlerName e "" ++ oneServices]
            | otherwise = []
        routeName = (map toLower) . entityName
        validateHandler
            | ValidateService `elem` services =  ["/validate/" ++ routeName e ++ " " ++ handlerName e "Validate" ++ " POST"]
            | otherwise = []
 
genFilters :: Entity -> [ServiceParam] -> [String]
genFilters e params 
    | null filters = ["let filters = [] :: Filter " ++ entityName e]
    | otherwise = ["let filters = " ++ (intercalate " ++ " filters) ]
                                
    where
        maybeUser 
            | PublicService `elem` params = "Nothing"
            | otherwise = "(Just user)"
        filters = mapMaybe mkFilter params ++ defaultFilter
        mkFilter (ServiceFilter f) = Just $ f ++ " " ++ maybeUser ++ " req"
        mkFilter _ = Nothing
        defaultFilter 
            | ServiceDefaultFilterSort `elem` params = []
            | otherwise = []  

        
            

    
    

genSelectOpts :: Entity -> [ServiceParam] -> [String]
genSelectOpts e params 
    | null opts = ["let selectOpts = []"]
    | otherwise = ["let selectOpts = " ++ (intercalate " ++ " opts) ]
                                
    where
        maybeUser 
            | PublicService `elem` params = "Nothing"
            | otherwise = "(Just user)"
        opts = mapMaybe mkOpt params ++ defaultSort
        mkOpt (ServiceSort f) = Just $ f ++ " " ++ maybeUser ++ " req"
        mkOpt _ = Nothing
        defaultSort 
            | ServiceDefaultFilterSort `elem` params = []
            | otherwise = []  



genHandler :: DbModule -> Entity -> [String]
genHandler db e = concatMap genService (entityServices e)
    where 
        genService (Service GetService params) =
               ["get" ++ handlerName e "Many" ++ " :: Handler RepJson",
                "get" ++ handlerName e "Many" ++ " = do"]
                ++ (indent $ ["req <- getRequest"] 
                     ++ maybeRequireAuth params 
                     ++ genFilters e params
                     ++ genSelectOpts e params
                   ++ ["entities <- runDB $ selectList filters selectOpts"] 
                   ++ (postHook " entities" params ++
                   [ "jsonToRepJson $ object [ \"entities\" .= toJSON entities ] "
                                   ]))
                ++ 
                             ["", "get" ++ handlerName e "" ++ " :: " 
                                         ++ entityName e ++ "Id -> Handler RepJson",
                             "get" ++ handlerName e "" ++ " key = do"]
                             ++ (indent $ ["req <- getRequest"]  ++
                                            maybeRequireAuth params ++ [
                                         "entity <- runDB $ get key"]
                                         ++ (cond " entity" params (
                                             postHook " key entity" params ++ [
                                         "jsonToRepJson $ toJSON entity"])))
        genService (Service PutService params) =                             
                             ["","put" ++ handlerName e "" ++ " :: " 
                                     ++ entityName e ++ "Id -> Handler RepJson",
                              "put" ++ handlerName e "" ++ " key = do"]
                          ++ (indent $ ["req <- getRequest",
                                      "entity <- parseJsonBody_"]
                                     ++ 
                                      (maybeRequireAuth params) ++ 
                                       (cond " entity" params 
                                        (validate [
                                      "runDB $ repsert key entity"]
                                      ++ postHook " key entity" params ++ [
                                      "jsonToRepJson $ emptyObject"])))
        genService (Service PostService params) =                  
                             ["","post" ++ handlerName e "Many" ++ " :: Handler RepJson" ,
                              "post" ++ handlerName e "Many" ++ " = do"]
                          ++ (indent $ ["req <- getRequest", 
                                      "entity <- parseJsonBody_"]
                                      ++ (maybeRequireAuth params) ++
                                          (cond " entity" params 
                                      (validate $ [
                                      "key <- runDB $ insert (entity :: " ++ entityName e ++ ")"] ++ (postHook " key entity" params) ++ [
                                      "jsonToRepJson $ object [ \"id\" .= toJSON key ]"])))
        genService (Service ValidateService params) =                  
                             ["","post" ++ handlerName e "Validate" ++ " :: Handler RepJson" ,
                              "post" ++ handlerName e "Validate" ++ " = do"]
                          ++ (indent $ ["req <- getRequest", 
                                      "entity <- parseJsonBody_"]
                                      ++ (maybeRequireAuth params) ++
                                          (cond " entity" params 
                                      (validate $ (postHook " entity" params) 
                                       ++ ["jsonToRepJson $ emptyObject"])))
 
        genService (Service DeleteService params) =                  
                             ["","delete" ++ handlerName e "" ++ " :: "
                                     ++ entityName e ++ "Id -> Handler RepJson",
                              "delete" ++ handlerName e "" ++ " key = do"]
                       ++ (indent $ ["req <- getRequest"] 
                                 ++ (cond " key" params (maybeRequireAuth params ++ 
                                   ["runDB $ delete key"]
                                   ++ (postHook "" params) ++ [
                                   "jsonToRepJson $ emptyObject"])))
        maybeRequireAuth params
            | PublicService `elem` params = []
            | otherwise = ["user <- requireAuth"]
        validate lines = ["errors <- runDB $ validate entity",
                          "if null errors"]
                          ++ (indent $ ["then do"] ++ (indent lines))
                          ++ (indent $ ["else jsonToRepJson $ object [ \"errors\" .= toJSON errors ]"])
                          
        matchCond (ServiceCond f) = Just f
        matchCond _ = Nothing
        formatServiceTrigger extra params f
            | PublicService `elem` params = f ++ " Nothing req" ++ extra
            | otherwise = f ++ " (Just user) req" ++ extra


        condFunctions params = mapMaybe matchCond params
        cond extra params lines = cond' extra params (condFunctions params) lines
        cond' extra params fs lines 
            | null fs = lines
            | otherwise = [
                           "errors <- sequence [" 
                                 ++ (intercalate "," $ 
                                  map (formatServiceTrigger extra params) fs )++ "]",
                           "if null errors"]
                           ++ (indent $ ["then do"] ++ (indent lines))
                           ++ (indent $ ["else jsonToRepJson $ object [ \"errors\" .= toJSON errors ]"])
        matchPostHook (ServicePostHook f) = Just f
        matchPostHook _ = Nothing        
        postHookFunctions params = mapMaybe matchPostHook params
        postHook extra params = postHook' extra params (postHookFunctions params) 
        postHook' extra params fs 
            | null fs = []
            | otherwise = ["sequence_ [" 
                                 ++ (intercalate "," $ 
                                  map (formatServiceTrigger extra params) fs )++ "]"]

genHandlers :: DbModule -> String
genHandlers db = unlines $ ["module Handler.Generated where ",
                            "import Import",
                            "import Yesod.Auth",
                            "import Model.Validation",
                            "import Model.Json ()",
                            "import Data.Aeson.Types (emptyObject)",
                            "import Handler.Hooks"]
                           ++ concatMap (genHandler db) (dbEntities db)
        
generateModels :: DbModule -> [(FilePath,String)]
generateModels db =  [("config/generated-models", unlines $ map (genModel db) (dbEntities db)),
                      ("config/generated-routes", 
                       unlines $ concatMap (genRoutes db) (dbEntities db)),
                      ("Model/Validation.hs", genValidation db ),
                      ("Model/Classes.hs", genInterfaces db ),
                      ("Model/Json.hs", genJson db),
                      ("Handler/Generated.hs", genHandlers db) ]

genJson :: DbModule -> String
genJson db = unlines $  ["{-# LANGUAGE FlexibleInstances #-}",
                         "module Model.Json where",
                         "import Import",
                         "import Data.Aeson",
                         "import qualified Data.HashMap.Lazy as HML"
                         ] 
                         ++ (concatMap genJsonInstance $ dbEntities db)
    where genJsonInstance e = 
            [
            "instance ToJSON (Entity " ++ entityName e ++ ") where"]
            ++ (indent $ [
              "toJSON (Entity k v) = case toJSON v of"]
              ++ (indent [
                  "Object o -> Object $ HML.insert \"id\" (toJSON k) o",
                  "_ -> error \"unexpected JS encode error\""]))
genFieldChecker :: Entity -> Field -> Maybe String
genFieldChecker e f@(Field _ fname (NormalField _ opts)) 
        | null opts = Nothing
        | otherwise = Just $ join "," $ catMaybes (map maybeCheck opts)
        where
            maybeCheck (FieldCheck func) = Just $ "checkResult \"" ++ entityName e ++ "." ++ fname ++ " " ++ func ++ "\" (V." ++ func ++ " $ " ++ entityFieldName e f ++ " e)"
genFieldChecker name _ = Nothing

genEntityChecker :: Entity -> [String]
genEntityChecker e 
    | (null . entityChecks) e = []
    | otherwise = [ join "," $ [ "checkResult \"" ++ entityName e ++ " " ++ func ++ "\" (V." ++ func ++ " e)"
                       | func <- entityChecks e ] ]
genEntityValidate :: DbModule -> Entity -> [String]
genEntityValidate db e = ["instance Validatable " ++ (entityName e) ++ " where "]
                       ++ (indent (["validate e = sequence ["]
                           ++ (indent $ 
                                   fieldChecks ++ genEntityChecker e
                                 ++ ["]"]))) ++ [""]
              where fieldChecks = mapMaybe (genFieldChecker e) (entityFields e)



genValidation :: DbModule -> String
genValidation db = unlines $ [
    "{-# LANGUAGE OverloadedStrings #-}",
    "{-# LANGUAGE ExistentialQuantification #-}",
    "module Model.Validation (Validatable(..)) where",
    "import Data.Text",
    "import qualified Model.ValidationFunctions as V",
    "import Import",
    "checkResult :: forall (m :: * -> *). (Monad m) => Text -> m Bool -> m Text",
    "checkResult msg f = do",
    "   result <- f",
    "   return $ if result then \"\" else msg",
    "",
    "class Validatable a where",
    "    validate :: forall m. (PersistQuery m, PersistEntityBackend a ~ PersistMonadBackend m) => a -> m [Text]"
    ] ++ concatMap (genEntityValidate db) (dbEntities db)
                   
ifaceFieldName :: Iface -> Field -> String
ifaceFieldName i f = (lowerFirst . ifaceName) i ++ (upperFirst . fieldName) f

entityFieldName :: Entity -> Field -> String
entityFieldName e f = (lowerFirst . entityName) e ++ (upperFirst . fieldName) f

    
genInterfaces :: DbModule -> String
genInterfaces db = unlines $ [
    "module Model.Classes where",
    "import Import",
    "import Data.Int",
    "import Data.Word",
    "import Data.Time"
    ] ++ concatMap genInterface (dbIfaces db)
    where
        genInterface i = [ "class " ++ ifaceName i ++ " a where" ]
                      ++ (indent $ [ ifaceFieldName i f 
                                     ++ " :: a -> " ++ haskellFieldType db f 
                                     | f <- ifaceFields i ] )
                      ++ [""]
                      ++ concatMap (genInstance i) [ e | e <- dbEntities db, 
                                                     (ifaceName i) `elem` entityImplements e ]

        genInstance i e = [ "instance " ++ ifaceName i ++ " " ++ entityName e ++ " where " ]
                        ++ (indent $ [ ifaceFieldName i f ++ " = " 
                                        ++ entityFieldName e f | f <- ifaceFields i ])
                        ++ [""]

                              

indent :: [String] -> [String]
indent = map (\l -> "    " ++ l)
        



