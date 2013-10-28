{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Handler.Example.Internal where
import Handler.Example.Enums
import Prelude
import Database.Esqueleto
import Database.Esqueleto.Internal.Sql (unsafeSqlBinOp)
import qualified Database.Persist as P
import Database.Persist.TH
import Yesod.Auth (requireAuth, requireAuthId, YesodAuth, AuthId, YesodAuthPersist)
import Yesod.Core
import Yesod.Persist (runDB, YesodPersist, YesodPersistBackend)
import Data.Aeson ((.:), (.:?), (.!=), FromJSON, parseJSON, decode)
import Data.Aeson.TH
import Data.Int
import Data.Word
import Data.Time
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import qualified Data.Attoparsec as AP
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import qualified Data.Text.Read
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.List as DL
import Control.Monad (mzero)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Conduit as C
import qualified Network.Wai as W
import Data.Conduit.Lazy (lazyConsume)
import Network.HTTP.Types (status200, status400, status403, status404)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Control.Applicative ((<$>), (<*>))  
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashMap.Strict as HMS

data FilterJsonMsg = FilterJsonMsg {
    filterJsonMsg_type :: Text,
    filterJsonMsg_value :: Text,
    filterJsonMsg_field :: Text,
    filterJsonMsg_property :: Text,
    filterJsonMsg_comparison :: Text
} 
filterJsonMsg_field_or_property :: FilterJsonMsg -> Text
filterJsonMsg_field_or_property fjm
    | not $ T.null $ filterJsonMsg_field fjm = filterJsonMsg_field fjm
    | otherwise = filterJsonMsg_property fjm

instance FromJSON FilterJsonMsg where
    parseJSON (A.Object v) = FilterJsonMsg <$>
        v .:? "type" .!= "string" <*> 
        (parseStringOrInt v) <*>
        v .:? "field" .!= "" <*>
        v .:? "property" .!= "" <*>
        v .:? "comparison" .!= "eq"
    parseJSON _ = mzero

parseStringOrInt :: HMS.HashMap Text A.Value -> AT.Parser Text
parseStringOrInt hm = case HMS.lookup "value" hm of
    Just (A.Number n) -> return $ T.pack $ show n
    Just (A.String s) -> return s
    _ -> mzero

data SortJsonMsg = SortJsonMsg {
    sortJsonMsg_property :: Text,
    sortJsonMsg_direction :: Text
}

$(deriveJSON (drop 12) ''SortJsonMsg)

-- defaultFilterOp :: forall v typ. PersistField typ => Text -> EntityField v typ -> typ -> Filter v
defaultFilterOp "eq" = (==.)
defaultFilterOp "neq" = (!=.)
defaultFilterOp "lt" = (<.)
defaultFilterOp "gt" = (>.)
defaultFilterOp "le" = (<=.)
defaultFilterOp "ge" = (>=.)
defaultFilterOp _ = (==.)

ilike = unsafeSqlBinOp " ILIKE "
is = unsafeSqlBinOp " IS "
safeRead :: forall a. Read a => Text -> Maybe a
safeRead s = case (reads $ T.unpack s) of
   [(v,_)] -> Just v
   _ -> Nothing

instance PathPiece Int32 where
    fromPathPiece s = 
        case Data.Text.Read.decimal s of
            Right (i, _) -> Just i
            Left _ -> Nothing
    toPathPiece = T.pack . show

instance PathPiece Word32 where
    fromPathPiece s =
        case Data.Text.Read.decimal s of
            Right (i, _) -> Just i
            Left _ -> Nothing

    toPathPiece = T.pack . show

instance PathPiece Word64 where
    fromPathPiece s = 
        case Data.Text.Read.decimal s of
            Right (i, _) -> Just i
            Left _ -> Nothing

    toPathPiece = T.pack . show

instance PathPiece Double where
    fromPathPiece s = 
        case Data.Text.Read.double s of
            Right (i, _) -> Just i
            Left _ -> Nothing
    toPathPiece = T.pack . show

instance PathPiece Bool where
    fromPathPiece "true" = Just True
    fromPathPiece "false" = Just False
    fromPathPiece "True" = Just True
    fromPathPiece "False" = Just False
    fromPathPiece  _ = Nothing
    toPathPiece = T.pack . show

instance PathPiece TimeOfDay where
    fromPathPiece = safeRead
    toPathPiece = T.pack . show

instance PathPiece UTCTime where
    fromPathPiece = safeRead
    toPathPiece = T.pack . show

instance PathPiece ZonedTime where
    fromPathPiece = safeRead
    toPathPiece = T.pack . show

instance (PathPiece a, Show a) => PathPiece [a] where
    fromPathPiece s = do
        parts <- safeRead s
        values <- mapM fromPathPiece parts
        return values
    toPathPiece = T.pack . show
            

getDefaultFilter maybeGetParam defaultFilterJson p = do
    f <- maybe maybeGetParam Just getFilter
    fromPathPiece f
    where 
        getFilter = do            
            j <- defaultFilterJson
            v <- DL.find (\fjm -> filterJsonMsg_property fjm == p) j
            return (filterJsonMsg_value v)
share [mkPersist sqlOnlySettings, mkMigrate "migrateExample" ] [persistLowerCase|
User json
    firstName Text  
    lastName Text  
    age Int32 Maybe  
    name Text  
    deletedVersionId VersionId Maybe   default=NULL
    UniqueUserName name !force
    deriving Typeable
Group json
    name Text  
    UniqueGroupName name !force
BlogPost json
    authorId UserId  
    content Text  
    time UTCTime  
    name Text  
    UniqueBlogPostName name !force
Comment json
    blogPostId BlogPostId  
    authorId UserId  
    comment Text  
    time UTCTime  
    commentState CommentState  
Version json
    time UTCTime  
    userId UserId  
ChangeRecord json
    field Text  
    oldValue Text  
    newValue Text  
    versionId VersionId  
    userEntityId UserId Maybe   default=NULL
|]
class Named a where
    namedName :: a -> Text
instance Named User where
    namedName = userName
instance Named Group where
    namedName = groupName
instance Named BlogPost where
    namedName = blogPostName
class Versioned a where
    versionedDeletedVersionId :: a -> Maybe VersionId
instance Versioned User where
    versionedDeletedVersionId = userDeletedVersionId
checkResult :: forall (m :: * -> *). (Monad m) => Text -> m Bool -> m (Maybe Text)
checkResult msg f = do
   result <- f
   return $ if result then Nothing else (Just msg)

class Validatable a where
    validate :: forall master b. (P.PersistMonadBackend (b (HandlerT master IO)) ~ P.PersistEntityBackend a,
                 b ~ YesodPersistBackend master,
                 P.PersistQuery (b (HandlerT master IO)),
                 P.PersistUnique (b (HandlerT master IO)),
                 YesodPersist master,
                 ExampleValidation master) 
             => a -> HandlerT master IO [Text]

class Yesod master => ExampleValidation master where
    nonEmpty :: (YesodPersist master) => Text -> HandlerT master IO Bool
    maxTwoPendingComments :: (P.PersistMonadBackend (b (HandlerT master IO)) ~ P.PersistEntityBackend Comment, 
                b ~ YesodPersistBackend master,
                P.PersistQuery (b (HandlerT master IO)),
                P.PersistUnique (b (HandlerT master IO)),
                YesodPersist master)
            => Comment -> HandlerT master IO Bool
instance Validatable User where
    validate v = do
        results <- sequence [
                checkResult "User.name nonEmpty" (nonEmpty $ userName v)            ]
        return $ catMaybes results
instance Validatable Group where
    validate v = do
        results <- sequence [
                checkResult "Group.name nonEmpty" (nonEmpty $ groupName v)            ]
        return $ catMaybes results
instance Validatable BlogPost where
    validate v = do
        results <- sequence [
                checkResult "BlogPost.name nonEmpty" (nonEmpty $ blogPostName v)            ]
        return $ catMaybes results
instance Validatable Comment where
    validate v = do
        results <- sequence [
                checkResult "Comment maxTwoPendingComments" $ maxTwoPendingComments v            ]
        return $ catMaybes results
instance Validatable Version where
    validate v = do
        results <- sequence [
            ]
        return $ catMaybes results
instance Validatable ChangeRecord where
    validate v = do
        results <- sequence [
            ]
        return $ catMaybes results
instance ToJSON Day where
    toJSON = toJSON . show

instance FromJSON Day where
    parseJSON x = do
        s <- parseJSON x
        case reads s of
            (d, _):_ -> return d
            [] -> mzero 

instance ToJSON TimeOfDay where
    toJSON = toJSON . show

instance FromJSON TimeOfDay where
    parseJSON x = do
        s <- parseJSON x
        case reads s of
            (d, _):_ -> return d
            [] -> mzero
getUsersR :: forall master. (ExampleValidation master, 
    YesodAuthPersist master,
    KeyEntity (AuthId master) ~ User,
    YesodPersistBackend master ~ SqlPersistT)
    => HandlerT Example (HandlerT master IO) A.Value
getUsersR  = do
    authId <- lift $ requireAuthId
    defaultFilterParam <- lookupGetParam "filter"
    let defaultFilterJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultFilterParam) :: Maybe [FilterJsonMsg]
    defaultSortParam <- lookupGetParam "sort"
    let defaultSortJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultSortParam) :: Maybe [SortJsonMsg]
    defaultOffsetParam <- lookupGetParam "start"
    defaultLimitParam <- lookupGetParam "limit"
    let defaultOffset = (maybe Nothing fromPathPiece defaultOffsetParam) :: Maybe Int64
    let defaultLimit = (maybe Nothing fromPathPiece defaultLimitParam) :: Maybe Int64
    let baseQuery limitOffsetOrder = from $ \(p ) -> do
        let pId' = p ^. UserId

        _ <- if limitOffsetOrder
            then do 
                offset 0
                limit 1000
                case defaultSortJson of 
                    Just xs -> mapM_ (\sjm -> case sortJsonMsg_property sjm of
                            "firstName" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (p  ^.  UserFirstName) ] 
                                "DESC" -> orderBy [ desc (p  ^.  UserFirstName) ] 
                                _      -> return ()
                            "lastName" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (p  ^.  UserLastName) ] 
                                "DESC" -> orderBy [ desc (p  ^.  UserLastName) ] 
                                _      -> return ()
                            "age" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (p  ^.  UserAge) ] 
                                "DESC" -> orderBy [ desc (p  ^.  UserAge) ] 
                                _      -> return ()
                            "name" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (p  ^.  UserName) ] 
                                "DESC" -> orderBy [ desc (p  ^.  UserName) ] 
                                _      -> return ()
                            "deletedVersionId" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (p  ^.  UserDeletedVersionId) ] 
                                "DESC" -> orderBy [ desc (p  ^.  UserDeletedVersionId) ] 
                                _      -> return ()
                
                            _ -> return ()
                        ) xs
                    Nothing -> orderBy [ asc (p ^. UserName) ]

                case defaultOffset of
                    Just o -> offset o
                    Nothing -> return ()
                case defaultLimit of
                    Just l -> limit (min 10000 l)
                    Nothing -> return ()
                 
            else return ()
        case defaultFilterJson of 
            Just xs -> mapM_ (\fjm -> case filterJsonMsg_field_or_property fjm of
                "firstName" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (p  ^.  UserFirstName) (val v) 
                    _        -> return ()
                "lastName" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (p  ^.  UserLastName) (val v) 
                    _        -> return ()
                "age" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (p  ^.  UserAge) (just (val v)) 
                    _        -> return ()
                "name" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (p  ^.  UserName) (val v) 
                    _        -> return ()
                "deletedVersionId" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (p  ^.  UserDeletedVersionId) (just (val v)) 
                    _        -> return ()

                _ -> return ()
                ) xs
            Nothing -> return ()  
        return (p ^. UserId, p ^. UserFirstName, p ^. UserLastName, p ^. UserAge, p ^. UserName, p ^. UserDeletedVersionId)
    count <- lift $ runDB $ select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- lift $ runDB $ select $ baseQuery True
    return $ A.object [
        "totalCount" .= (T.pack $ (\(Database.Esqueleto.Value v) -> show (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3), (Database.Esqueleto.Value f4), (Database.Esqueleto.Value f5), (Database.Esqueleto.Value f6)) -> A.object [
                    "id" .= toJSON f1,
                    "firstName" .= toJSON f2,
                    "lastName" .= toJSON f3,
                    "age" .= toJSON f4,
                    "name" .= toJSON f5,
                    "deletedVersionId" .= toJSON f6                                    
                    ]
                _ -> A.object []
            ) results)
       ]
putUsersUserIdR :: forall master. (ExampleValidation master, 
    YesodAuthPersist master,
    KeyEntity (AuthId master) ~ User,
    YesodPersistBackend master ~ SqlPersistT)
    => UserId -> HandlerT Example (HandlerT master IO) A.Value
putUsersUserIdR p1 = do
    authId <- lift $ requireAuthId
    yReq <- getRequest
    let wReq = reqWaiRequest yReq
    jsonResult <- parseJsonBody
    jsonBody <- case jsonResult of
         A.Error err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         A.Success o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    attr_age <- case HML.lookup "age" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute age in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute age in the JSON object in request body" :: Text)
            ]
    attr_lastName <- case HML.lookup "lastName" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute lastName in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute lastName in the JSON object in request body" :: Text)
            ]
    attr_firstName <- case HML.lookup "firstName" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute firstName in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute firstName in the JSON object in request body" :: Text)
            ]
    _ <- do
        result <- lift $ runDB $ select $ from $ \(u ) -> do
            let uId' = u ^. UserId
            where_ (((u ^. UserId) ==. ((val p1))) &&. (((val p1)) ==. ((val authId))))

            limit 1
            return u
        case result of
            ((Entity _ _):_) -> return ()
            _ -> sendResponseStatus status403 (A.object [
                    "message" .= ("require condition #1 failed" :: Text)
                    ])
    runDB_result <- lift $ runDB $ do
        e2 <- do
            es <- lift $ runDB $ select $ from $ \o -> do
                where_ (o ^. UserId ==. (val p1))
                limit 1
                return o
            e <- case es of
                [(Entity _ e')] -> return e'    
                _ -> sendResponseStatus status404 $ A.object [ 
                        "message" .= ("Could not update a non-existing User" :: Text)
                    ]
    
            return $ User {
                            userFirstName = attr_firstName
                    
                    ,
                            userLastName = attr_lastName
                    
                    ,
                            userAge = (Just attr_age
                    )
                    ,
                            userName = userName e
                    
                    ,
                            userDeletedVersionId = userDeletedVersionId e
                    
     
                }
        vErrors <- lift $ validate e2
        case vErrors of
             xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                         "message" .= ("Entity validation failed" :: Text),
                         "errors" .= toJSON xs 
                     ])
             _ -> P.repsert p1 (e2 :: User)
        return A.Null
    return $ runDB_result
getBlogpostsR :: forall master. (ExampleValidation master, 
    YesodAuthPersist master,
    KeyEntity (AuthId master) ~ User,
    YesodPersistBackend master ~ SqlPersistT)
    => HandlerT Example (HandlerT master IO) A.Value
getBlogpostsR  = do
    filterParam_blogPostName <- lookupGetParam "blogPostName"
    defaultFilterParam <- lookupGetParam "filter"
    let defaultFilterJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultFilterParam) :: Maybe [FilterJsonMsg]
    defaultSortParam <- lookupGetParam "sort"
    let defaultSortJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultSortParam) :: Maybe [SortJsonMsg]
    defaultOffsetParam <- lookupGetParam "start"
    defaultLimitParam <- lookupGetParam "limit"
    let defaultOffset = (maybe Nothing fromPathPiece defaultOffsetParam) :: Maybe Int64
    let defaultLimit = (maybe Nothing fromPathPiece defaultLimitParam) :: Maybe Int64
    let baseQuery limitOffsetOrder = from $ \(bp  `InnerJoin` p) -> do
        on ((p ^. UserId) ==. (bp ^. BlogPostAuthorId))
        let bpId' = bp ^. BlogPostId

        _ <- if limitOffsetOrder
            then do 
                offset 0
                limit 1000
                case defaultSortJson of 
                    Just xs -> mapM_ (\sjm -> case sortJsonMsg_property sjm of
                            "authorId" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (bp  ^.  BlogPostAuthorId) ] 
                                "DESC" -> orderBy [ desc (bp  ^.  BlogPostAuthorId) ] 
                                _      -> return ()
                            "content" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (bp  ^.  BlogPostContent) ] 
                                "DESC" -> orderBy [ desc (bp  ^.  BlogPostContent) ] 
                                _      -> return ()
                            "time" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (bp  ^.  BlogPostTime) ] 
                                "DESC" -> orderBy [ desc (bp  ^.  BlogPostTime) ] 
                                _      -> return ()
                            "name" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (bp  ^.  BlogPostName) ] 
                                "DESC" -> orderBy [ desc (bp  ^.  BlogPostName) ] 
                                _      -> return ()
                            "authorFirstName" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (p  ^.  UserFirstName) ] 
                                "DESC" -> orderBy [ desc (p  ^.  UserFirstName) ] 
                                _      -> return ()
                            "authorLastName" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (p  ^.  UserLastName) ] 
                                "DESC" -> orderBy [ desc (p  ^.  UserLastName) ] 
                                _      -> return ()
                
                            _ -> return ()
                        ) xs
                    Nothing -> orderBy [ asc (bp ^. BlogPostName) ]

                case defaultOffset of
                    Just o -> offset o
                    Nothing -> return ()
                case defaultLimit of
                    Just l -> limit (min 10000 l)
                    Nothing -> return ()
                 
            else return ()
        case defaultFilterJson of 
            Just xs -> mapM_ (\fjm -> case filterJsonMsg_field_or_property fjm of
                "firstName" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (p  ^.  UserFirstName) (val v) 
                    _        -> return ()
                "lastName" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (p  ^.  UserLastName) (val v) 
                    _        -> return ()
                "age" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (p  ^.  UserAge) (just (val v)) 
                    _        -> return ()
                "name" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (p  ^.  UserName) (val v) 
                    _        -> return ()
                "deletedVersionId" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (p  ^.  UserDeletedVersionId) (just (val v)) 
                    _        -> return ()
                "authorId" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (bp  ^.  BlogPostAuthorId) (val v) 
                    _        -> return ()
                "content" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (bp  ^.  BlogPostContent) (val v) 
                    _        -> return ()
                "time" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (bp  ^.  BlogPostTime) (val v) 
                    _        -> return ()
                "name" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (bp  ^.  BlogPostName) (val v) 
                    _        -> return ()

                _ -> return ()
                ) xs
            Nothing -> return ()  
        case getDefaultFilter filterParam_blogPostName defaultFilterJson "blogPostName" of
            Just localParam -> do 

                where_ $ ((bp ^. BlogPostName) `ilike` (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%"))))) ||. (((p ^. UserFirstName) `ilike` (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%"))))) ||. ((p ^. UserLastName) `ilike` (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%"))))))
            Nothing -> return ()
        return (bp ^. BlogPostId, bp ^. BlogPostAuthorId, bp ^. BlogPostContent, bp ^. BlogPostTime, bp ^. BlogPostName, p ^. UserFirstName, p ^. UserLastName)
    count <- lift $ runDB $ select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- lift $ runDB $ select $ baseQuery True
    return $ A.object [
        "totalCount" .= (T.pack $ (\(Database.Esqueleto.Value v) -> show (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3), (Database.Esqueleto.Value f4), (Database.Esqueleto.Value f5), (Database.Esqueleto.Value f6), (Database.Esqueleto.Value f7)) -> A.object [
                    "id" .= toJSON f1,
                    "authorId" .= toJSON f2,
                    "content" .= toJSON f3,
                    "time" .= toJSON f4,
                    "name" .= toJSON f5,
                    "authorFirstName" .= toJSON f6,
                    "authorLastName" .= toJSON f7                                    
                    ]
                _ -> A.object []
            ) results)
       ]
postBlogpostsR :: forall master. (ExampleValidation master, 
    YesodAuthPersist master,
    KeyEntity (AuthId master) ~ User,
    YesodPersistBackend master ~ SqlPersistT)
    => HandlerT Example (HandlerT master IO) A.Value
postBlogpostsR  = do
    authId <- lift $ requireAuthId
    yReq <- getRequest
    let wReq = reqWaiRequest yReq
    jsonResult <- parseJsonBody
    jsonBody <- case jsonResult of
         A.Error err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         A.Success o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    attr_content <- case HML.lookup "content" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute content in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute content in the JSON object in request body" :: Text)
            ]
    attr_name <- case HML.lookup "name" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute name in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute name in the JSON object in request body" :: Text)
            ]
    __currentTime <- liftIO $ getCurrentTime
    runDB_result <- lift $ runDB $ do
        e1 <- do
    
            return $ BlogPost {
                            blogPostAuthorId = authId
                    
                    ,
                            blogPostContent = attr_content
                    
                    ,
                            blogPostTime = __currentTime
                    
                    ,
                            blogPostName = attr_name
                    
     
                }
        vErrors <- lift $ validate e1
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        P.insert (e1 :: BlogPost)
        return A.Null
    return $ runDB_result
putBlogpostsBlogPostIdR :: forall master. (ExampleValidation master, 
    YesodAuthPersist master,
    KeyEntity (AuthId master) ~ User,
    YesodPersistBackend master ~ SqlPersistT)
    => BlogPostId -> HandlerT Example (HandlerT master IO) A.Value
putBlogpostsBlogPostIdR p1 = do
    authId <- lift $ requireAuthId
    yReq <- getRequest
    let wReq = reqWaiRequest yReq
    jsonResult <- parseJsonBody
    jsonBody <- case jsonResult of
         A.Error err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         A.Success o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    runDB_result <- lift $ runDB $ do
        e1 <- case A.fromJSON jsonBody of
            A.Success e -> return e
            A.Error err -> sendResponseStatus status400 ("Could not decode an entity of type BlogPost from JSON object in the request body : " ++ err )
        vErrors <- lift $ validate e1
        case vErrors of
             xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                         "message" .= ("Entity validation failed" :: Text),
                         "errors" .= toJSON xs 
                     ])
             _ -> P.repsert p1 (e1 :: BlogPost)
        return A.Null
    return $ runDB_result
getCommentsR :: forall master. (ExampleValidation master, 
    YesodAuthPersist master,
    KeyEntity (AuthId master) ~ User,
    YesodPersistBackend master ~ SqlPersistT)
    => HandlerT Example (HandlerT master IO) A.Value
getCommentsR  = do
    filterParam_authorId <- lookupGetParam "authorId"
    filterParam_query <- lookupGetParam "query"
    defaultFilterParam <- lookupGetParam "filter"
    let defaultFilterJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultFilterParam) :: Maybe [FilterJsonMsg]
    defaultSortParam <- lookupGetParam "sort"
    let defaultSortJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultSortParam) :: Maybe [SortJsonMsg]
    defaultOffsetParam <- lookupGetParam "start"
    defaultLimitParam <- lookupGetParam "limit"
    let defaultOffset = (maybe Nothing fromPathPiece defaultOffsetParam) :: Maybe Int64
    let defaultLimit = (maybe Nothing fromPathPiece defaultLimitParam) :: Maybe Int64
    let baseQuery limitOffsetOrder = from $ \(bp ) -> do
        let bpId' = bp ^. BlogPostId

        _ <- if limitOffsetOrder
            then do 
                offset 0
                limit 1000
                case defaultSortJson of 
                    Just xs -> mapM_ (\sjm -> case sortJsonMsg_property sjm of
                            "authorId" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (bp  ^.  BlogPostAuthorId) ] 
                                "DESC" -> orderBy [ desc (bp  ^.  BlogPostAuthorId) ] 
                                _      -> return ()
                            "content" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (bp  ^.  BlogPostContent) ] 
                                "DESC" -> orderBy [ desc (bp  ^.  BlogPostContent) ] 
                                _      -> return ()
                            "time" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (bp  ^.  BlogPostTime) ] 
                                "DESC" -> orderBy [ desc (bp  ^.  BlogPostTime) ] 
                                _      -> return ()
                            "name" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (bp  ^.  BlogPostName) ] 
                                "DESC" -> orderBy [ desc (bp  ^.  BlogPostName) ] 
                                _      -> return ()
                
                            _ -> return ()
                        ) xs
                    Nothing -> orderBy [ asc (bp ^. BlogPostName) ]

                case defaultOffset of
                    Just o -> offset o
                    Nothing -> return ()
                case defaultLimit of
                    Just l -> limit (min 10000 l)
                    Nothing -> return ()
                 
            else return ()
        case defaultFilterJson of 
            Just xs -> mapM_ (\fjm -> case filterJsonMsg_field_or_property fjm of
                "authorId" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (bp  ^.  BlogPostAuthorId) (val v) 
                    _        -> return ()
                "content" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (bp  ^.  BlogPostContent) (val v) 
                    _        -> return ()
                "time" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (bp  ^.  BlogPostTime) (val v) 
                    _        -> return ()
                "name" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (bp  ^.  BlogPostName) (val v) 
                    _        -> return ()

                _ -> return ()
                ) xs
            Nothing -> return ()  
        case getDefaultFilter filterParam_authorId defaultFilterJson "authorId" of
            Just localParam -> from $ \(p) -> do
 
                where_ ((bp ^. BlogPostAuthorId) ==. (p ^. UserId))

                where_ $ (p ^. UserId) ==. ((val localParam))
            Nothing -> return ()
        case getDefaultFilter filterParam_query defaultFilterJson "query" of
            Just localParam -> do 

                where_ $ (bp ^. BlogPostContent) `ilike` (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%"))))
            Nothing -> return ()
        return (bp ^. BlogPostId, bp ^. BlogPostAuthorId, bp ^. BlogPostContent, bp ^. BlogPostTime, bp ^. BlogPostName)
    count <- lift $ runDB $ select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- lift $ runDB $ select $ baseQuery True
    return $ A.object [
        "totalCount" .= (T.pack $ (\(Database.Esqueleto.Value v) -> show (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3), (Database.Esqueleto.Value f4), (Database.Esqueleto.Value f5)) -> A.object [
                    "id" .= toJSON f1,
                    "authorId" .= toJSON f2,
                    "content" .= toJSON f3,
                    "time" .= toJSON f4,
                    "name" .= toJSON f5                                    
                    ]
                _ -> A.object []
            ) results)
       ]
postCommentsR :: forall master. (ExampleValidation master, 
    YesodAuthPersist master,
    KeyEntity (AuthId master) ~ User,
    YesodPersistBackend master ~ SqlPersistT)
    => HandlerT Example (HandlerT master IO) A.Value
postCommentsR  = do
    authId <- lift $ requireAuthId
    yReq <- getRequest
    let wReq = reqWaiRequest yReq
    jsonResult <- parseJsonBody
    jsonBody <- case jsonResult of
         A.Error err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         A.Success o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    runDB_result <- lift $ runDB $ do
        e1 <- case A.fromJSON jsonBody of
            A.Success e -> return e
            A.Error err -> sendResponseStatus status400 ("Could not decode an entity of type BlogPost from JSON object in the request body : " ++ err )
        vErrors <- lift $ validate e1
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        P.insert (e1 :: BlogPost)
        return A.Null
    return $ runDB_result
getCommentsCommentIdR :: forall master. (ExampleValidation master, 
    YesodAuthPersist master,
    KeyEntity (AuthId master) ~ User,
    YesodPersistBackend master ~ SqlPersistT)
    => CommentId -> HandlerT Example (HandlerT master IO) A.Value
getCommentsCommentIdR p1 = do
    let baseQuery limitOffsetOrder = from $ \(c ) -> do
        let cId' = c ^. CommentId
        where_ ((c ^. CommentId) ==. ((val p1)))

        _ <- if limitOffsetOrder
            then do 
                offset 0
                limit 10000

                 
            else return ()
        return (c ^. CommentBlogPostId, c ^. CommentAuthorId, c ^. CommentComment, c ^. CommentTime, c ^. CommentCommentState)
    count <- lift $ runDB $ select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- lift $ runDB $ select $ baseQuery True
    return $ A.object [
        "totalCount" .= (T.pack $ (\(Database.Esqueleto.Value v) -> show (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3), (Database.Esqueleto.Value f4), (Database.Esqueleto.Value f5)) -> A.object [
                    "blogPostId" .= toJSON f1,
                    "authorId" .= toJSON f2,
                    "comment" .= toJSON f3,
                    "time" .= toJSON f4,
                    "commentState" .= toJSON f5                                    
                    ]
                _ -> A.object []
            ) results)
       ]
putCommentsCommentIdR :: forall master. (ExampleValidation master, 
    YesodAuthPersist master,
    KeyEntity (AuthId master) ~ User,
    YesodPersistBackend master ~ SqlPersistT)
    => CommentId -> HandlerT Example (HandlerT master IO) A.Value
putCommentsCommentIdR p1 = do
    authId <- lift $ requireAuthId
    yReq <- getRequest
    let wReq = reqWaiRequest yReq
    jsonResult <- parseJsonBody
    jsonBody <- case jsonResult of
         A.Error err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         A.Success o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    runDB_result <- lift $ runDB $ do
        e1 <- case A.fromJSON jsonBody of
            A.Success e -> return e
            A.Error err -> sendResponseStatus status400 ("Could not decode an entity of type Comment from JSON object in the request body : " ++ err )
        vErrors <- lift $ validate e1
        case vErrors of
             xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                         "message" .= ("Entity validation failed" :: Text),
                         "errors" .= toJSON xs 
                     ])
             _ -> P.repsert p1 (e1 :: Comment)
        return A.Null
    return $ runDB_result
deleteCommentsCommentIdR :: forall master. (ExampleValidation master, 
    YesodAuthPersist master,
    KeyEntity (AuthId master) ~ User,
    YesodPersistBackend master ~ SqlPersistT)
    => CommentId -> HandlerT Example (HandlerT master IO) A.Value
deleteCommentsCommentIdR p1 = do
    authId <- lift $ requireAuthId
    yReq <- getRequest
    let wReq = reqWaiRequest yReq
    jsonResult <- parseJsonBody
    jsonBody <- case jsonResult of
         A.Error err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         A.Success o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    runDB_result <- lift $ runDB $ do
        delete $ from $ (\c -> where_ $ (c ^. CommentId) ==. ((val p1)))
        return A.Null
    return $ runDB_result

data Example = Example

getExample :: a -> Example
getExample = const Example

mkYesodSubData "Example" [parseRoutes|
/users        UsersR      GET
/users/#UserId        UsersUserIdR      PUT
/blogposts        BlogpostsR      GET POST
/blogposts/#BlogPostId        BlogpostsBlogPostIdR      PUT
/comments        CommentsR      GET POST
/comments/#CommentId        CommentsCommentIdR      GET PUT DELETE
|]

