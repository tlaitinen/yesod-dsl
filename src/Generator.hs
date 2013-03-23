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
             =  ["/data/" ++ routeName e ++ " " ++ handlerName e "Many" ++ getService ++ postService]
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

genDefaultFilter :: Entity -> [String]
genDefaultFilter e = ["do"]
                   ++ (indent $ [
                  "filter <- lookupGetParam \"filter\"",
                  "if isJust filter"]
                 ++ (indent $ ["then do"]
                      ++ (indent $ ["case json (fromJust filter) of"]
                             ++ (indent $ ["(Object o) -> do"]
                                   ++ (indent ["return [] -- TODO: filter with o"])
                                   ++ ["_ -> invalidArgs [fromJust filter]"]))
                      ++ ["else return []"]))
genFilters :: Entity -> [ServiceParam] -> [String]
genFilters e params 
    | null filters = ["let filters = [] :: [[Filter " ++ entityName e ++ "]]"]
    | otherwise =  ["filters <- sequence ["] ++ (indent $ filters ++ ["]"])
                                
    where
        filters :: [String]
        filters = intercalate [","] $ mapMaybe mkFilter params ++ defaultFilter
        mkFilter :: ServiceParam -> Maybe [String]
        mkFilter (ServiceFilter f) = Just $ ["H." ++ f]
        mkFilter _ = Nothing
        hasDefaultFilter = ServiceDefaultFilterSort `elem` params
        defaultFilter 
            | ServiceDefaultFilterSort `elem` params = [genDefaultFilter e]
            | otherwise = []  

        
            

    
    
genDefaultSelectOpts :: Entity -> [String]
genDefaultSelectOpts e = ["do"]
                   ++ (indent $ [
                  "sortParam <- lookupGetParam \"sort\"",
                  "if isJust sortParam"]
                 ++ (indent $ ["then do"]
                      ++ (indent $ ["case json (fromJust sortParam) of"]
                             ++ (indent $ ["(Object o) -> do"]
                                   ++ (indent ["return [] -- TODO: sort with o"])
                                   ++ ["_ -> invalidArgs [fromJust sortParam]"]))
                      ++ ["else return []"]))

genSelectOpts :: Entity -> [ServiceParam] -> [String]
genSelectOpts e params 
    | null opts = ["let selectOpts = [] :: [[SelectOpt " ++ entityName e ++ "]]"]
    | otherwise = ["selectOpts <- sequence ["] ++ (indent $ opts ++ ["]"])
    where
        opts = intercalate [","] $ mapMaybe mkOpt params ++ defaultSort
        mkOpt (ServiceSelectOpts f) = Just $ ["H." ++ f]
        mkOpt _ = Nothing
        defaultSort 
            | ServiceDefaultFilterSort `elem` params = [genDefaultSelectOpts e]
            | otherwise = []  



genHandler :: DbModule -> Entity -> [String]
genHandler db e = concatMap genService (entityServices e)
    where 
        genService (Service GetService params) =
               ["get" ++ handlerName e "Many" ++ " :: Handler RepJson",
                "get" ++ handlerName e "Many" ++ " = do"]
                ++ (indent $ 
                     maybeRequireAuth params 
                     ++ genFilters e params
                     ++ genSelectOpts e params
                   ++ ["entities <- runDB $ selectList (concat filters) (concat selectOpts)"] 
                   ++ (postHook " entities" params ++
                   [ "jsonToRepJson $ object [ \"entities\" .= toJSON entities ] "
                                   ]))
                ++ 
                             ["", "get" ++ handlerName e "" ++ " :: " 
                                         ++ entityName e ++ "Id -> Handler RepJson",
                             "get" ++ handlerName e "" ++ " key = do"]
                             ++ (indent $ 
                                            maybeRequireAuth params ++ [
                                         "entity <- runDB $ get key"]
                                         ++ (cond " entity" params (
                                             postHook " key entity" params ++ [
                                         "jsonToRepJson $ toJSON entity"])))
        genService (Service PutService params) =                             
                             ["","put" ++ handlerName e "" ++ " :: " 
                                     ++ entityName e ++ "Id -> Handler RepJson",
                              "put" ++ handlerName e "" ++ " key = do"]
                          ++ (indent $ 
                                      ["entity <- parseJsonBody_"]
                                     ++ 
                                      (maybeRequireAuth params) ++ 
                                       (cond " entity" params $
                                        (validate e $ [
                                      "runDB $ repsert key entity"]
                                      ++ postHook " key entity" params ++ [
                                      "jsonToRepJson $ emptyObject"])))
        genService (Service PostService params) =                  
                             ["","post" ++ handlerName e "Many" ++ " :: Handler RepJson" ,
                              "post" ++ handlerName e "Many" ++ " = do"]
                          ++ (indent $ 
                                      ["entity <- parseJsonBody_"]
                                      ++ (maybeRequireAuth params) ++
                                          (cond " entity" params 
                                      (validate e $ [
                                      "key <- runDB $ insert (entity :: " ++ entityName e ++ ")"] ++ (postHook " key entity" params) ++ [
                                      "jsonToRepJson $ object [ \"id\" .= toJSON key ]"])))
        genService (Service ValidateService params) =                  
                             ["","post" ++ handlerName e "Validate" ++ " :: Handler RepJson" ,
                              "post" ++ handlerName e "Validate" ++ " = do"]
                          ++ (indent $ 
                                      ["entity <- parseJsonBody_ "]
                                      ++ (maybeRequireAuth params) ++
                                          (cond " entity" params 
                                      (validate e $ (postHook " entity" params) 
                                       ++ ["jsonToRepJson $ emptyObject"])))
 
        genService (Service DeleteService params) =                  
                             ["","delete" ++ handlerName e "" ++ " :: "
                                     ++ entityName e ++ "Id -> Handler RepJson",
                              "delete" ++ handlerName e "" ++ " key = do"]
                       ++ (indent $ 
                                  (cond " key" params $ (maybeRequireAuth params ++ 
                                   ["runDB $ delete key"]
                                   ++ (postHook "" params) ++ [
                                   "jsonToRepJson $ emptyObject"])))
        maybeRequireAuth params
            | PublicService `elem` params = []
            | otherwise = ["_ <- requireAuthId"]
        validate e lines = ["errors <- runDB $ validate (entity :: " 
                             ++ entityName e ++ ")",
                          "if null errors"]
                          ++ (indent $ ["then do"] ++ (indent lines))
                          ++ (indent $ ["else jsonToRepJson $ object [ \"errors\" .= toJSON errors ]"])
                          
        matchPreHook (ServicePreHook f) = Just f
        matchPreHook _ = Nothing


        condFunctions params = mapMaybe matchPreHook params
        cond extra params lines = cond' extra params (condFunctions params) lines
        cond' extra params fs lines 
            | null fs = lines
            | otherwise = [
                           "errors <- sequence [" 
                                 ++ (intercalate ", " 
                                           [ "H." ++ f ++ extra | f <- fs ]) ++ "]",
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
                                 ++ (intercalate ", "   
                                            [ "H." ++ f ++ extra | f <- fs]) ++ "]"]

genHandlers :: DbModule -> String
genHandlers db = unlines $ ["module Handler.Generated where ",
                            "import Import",
                            "import Yesod.Auth",
                            "import Model.Validation",
                            "import Model.Json ()",
                            "import Data.Aeson (json)",
                            "import Data.Maybe",
                            "import Data.Aeson.Types (emptyObject)",
                            "import qualified Handler.Hooks as H"]
                           ++ concatMap (genHandler db) (dbEntities db)
        
generateModels :: DbModule -> [(FilePath,String,Bool)]
generateModels db =  [("config/models", unlines $ map (genModel db) (dbEntities db), True),
                      ("config/routes", 
                       unlines $ concatMap (genRoutes db) (dbEntities db), True),
                      ("Model/Validation.hs", genValidation db, False ),
                      ("Model/Classes.hs", genInterfaces db, False ),
                      ("Model/Json.hs", genJson db, False),
                      ("Handler/Generated.hs", genHandlers db, False) ]

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
        | otherwise = Just $ join "," $ mapMaybe maybeCheck opts
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
                           ++ (indent $ commas 
                                   (fieldChecks ++ genEntityChecker e)
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
                   
ifaceFieldName :: Class -> Field -> String
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
    ] ++ concatMap genInterface (dbClasses db)
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

commas :: [String] -> [String]
commas (x1:x2:xs) = (x1 ++ ","):commas (x2:xs)
commas (x:xs) = x : commas xs
commas _ = []


