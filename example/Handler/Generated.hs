{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Generated(
    postNoteValidateR,
    putNoteR,
    deleteNoteR,
    postNoteManyR,
    postNoteR,
    getNoteManyR,
    getNoteR,
    postPersonValidateR,
    putPersonR,
    deletePersonR,
    postPersonManyR,
    postPersonR,
    getPersonManyR,
    getPersonR
) where
import Import
import Yesod.Auth
import Model.Validation
import Model.Json ()
import Data.Aeson ((.:), (.:?), (.!=), FromJSON, parseJSON, decode)
import Data.Aeson.TH
import Data.Int
import Data.Word
import Data.Time
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import qualified Data.Text.Read
import Data.Aeson.Types (emptyObject)
import qualified Handler.Hooks as H
import qualified Data.Text as T
import Control.Monad (mzero)

data FilterJsonMsg = FilterJsonMsg {
    filterJsonMsg_type :: Text,
    filterJsonMsg_value :: Text,
    filterJsonMsg_field :: Text,
    filterJsonMsg_comparison :: Text
} 
instance FromJSON FilterJsonMsg where
    parseJSON (Object v) = FilterJsonMsg <$>
        v .: "type" <*>
        v .: "value" <*>
        v .: "field" <*>
        v .:? "comparison" .!= "eq"
    parseJSON _ = mzero

data SortJsonMsg = SortJsonMsg {
    sortJsonMsg_property :: Text,
    sortJsonMsg_direction :: Text
}

$(deriveJSON (drop 12) ''SortJsonMsg)

defaultFilterOp :: forall v typ. PersistField typ => Text -> EntityField v typ -> typ -> Filter v
defaultFilterOp "eq" = (==.)
defaultFilterOp "neq" = (!=.)
defaultFilterOp "lt" = (<.)
defaultFilterOp "gt" = (>.)
defaultFilterOp "le" = (<=.)
defaultFilterOp "ge" = (>=.)
defaultFilterOp _ = (==.)

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



-- getRangeSelectOpts :: forall s m v. GHandler s m [SelectOpt v]
getRangeSelectOpts = do
    start <- lookupGetParam "start"
    limit <- lookupGetParam "limit"
    return $ mkOpts (parseInt start) (parseInt limit)
        where
            parseInt (Just s) = case (Data.Text.Read.decimal s) of
              Right (x,_) -> Just x
              _ -> Nothing
            parseInt _ = Nothing
            mkOpts (Just s) (Just l) = [ OffsetBy s, LimitTo l ]
            mkOpts _ _ = []



postNoteValidateR :: Handler Value
postNoteValidateR = do
    entity <- parseJsonBody_ 
    errors <- runDB $ validate (entity :: Note)
    if null errors
        then do
            return $ emptyObject
        else return $ object [ "errors" .= toJSON errors ]

putNoteR :: NoteId -> Handler Value
putNoteR key = do
    entity <- parseJsonBody_
    errors <- runDB $ validate (entity :: Note)
    if null errors
        then do
            runDB $ repsert key entity
            jsonToRepJson $ emptyObject
        else jsonToRepJson $ object [ "errors" .= toJSON errors ]

deleteNoteR :: NoteId -> Handler Value
deleteNoteR key = do
    runDB $ delete key
    jsonToRepJson $ emptyObject

postNoteManyR :: Handler Value
postNoteManyR = do
    entity <- parseJsonBody_
    errors <- runDB $ validate (entity :: Note)
    if null errors
        then do
            key <- runDB $ insert (entity :: Note)
            jsonToRepJson $ object [ "id" .= toJSON key ]
        else jsonToRepJson $ object [ "errors" .= toJSON errors ]

postNoteR :: NoteId -> Handler Value
postNoteR _ = postNoteManyR
getNoteManyR :: Handler Value
getNoteManyR = do

    let filters = [] :: [[Filter Note]]

    let selectOpts = [] :: [[SelectOpt Note]]

    entities <- runDB $ selectList (concat filters) (concat selectOpts)

    return $ object [ "entities" .= toJSON entities ]    

getNoteR :: NoteId -> Handler Value
getNoteR key = do
    entity <- runDB $ get key
    jsonToRepJson $ toJSON entity

postPersonValidateR :: Handler Value
postPersonValidateR = do
    entity <- parseJsonBody_ 
    errors <- runDB $ validate (entity :: Person)
    if null errors
        then do
            jsonToRepJson $ emptyObject
        else jsonToRepJson $ object [ "errors" .= toJSON errors ]

putPersonR :: PersonId -> Handler Value
putPersonR key = do
    entity <- parseJsonBody_
    errors <- runDB $ validate (entity :: Person)
    if null errors
        then do
            runDB $ repsert key entity
            jsonToRepJson $ emptyObject
        else jsonToRepJson $ object [ "errors" .= toJSON errors ]

deletePersonR :: PersonId -> Handler Value
deletePersonR key = do
    runDB $ delete key
    jsonToRepJson $ emptyObject

postPersonManyR :: Handler Value
postPersonManyR = do
    entity <- parseJsonBody_
    errors <- runDB $ validate (entity :: Person)
    if null errors
        then do
            key <- runDB $ insert (entity :: Person)
            jsonToRepJson $ object [ "id" .= toJSON key ]
        else jsonToRepJson $ object [ "errors" .= toJSON errors ]

postPersonR :: PersonId -> Handler Value
postPersonR _ = postPersonManyR
toDefaultFilterPerson :: FilterJsonMsg -> Maybe (Filter Person)
toDefaultFilterPerson f = case (filterJsonMsg_field f) of
    "birthDate" -> case (fromPathPiece $ filterJsonMsg_value f) of (Just v) -> Just $ defaultFilterOp (filterJsonMsg_comparison f) PersonBirthDate v ; _ -> Nothing

    "email" -> case (fromPathPiece $ filterJsonMsg_value f) of (Just v) -> Just $ defaultFilterOp (filterJsonMsg_comparison f) PersonEmail v ; _ -> Nothing

    "timezone" -> case (fromPathPiece $ filterJsonMsg_value f) of (Just v) -> Just $ defaultFilterOp (filterJsonMsg_comparison f) PersonTimezone v ; _ -> Nothing

    "lastName" -> case (fromPathPiece $ filterJsonMsg_value f) of (Just v) -> Just $ defaultFilterOp (filterJsonMsg_comparison f) PersonLastName v ; _ -> Nothing

    "firstName" -> case (fromPathPiece $ filterJsonMsg_value f) of (Just v) -> Just $ defaultFilterOp (filterJsonMsg_comparison f) PersonFirstName v ; _ -> Nothing


    _ -> Nothing

toDefaultSortPerson :: SortJsonMsg -> Maybe (SelectOpt Person)
toDefaultSortPerson s = case (sortJsonMsg_property s) of
    "birthDate" -> case (sortJsonMsg_direction s) of "ASC" -> Just $ Asc PersonBirthDate ; "DESC" -> Just $ Desc PersonBirthDate ; _ -> Nothing

    "email" -> case (sortJsonMsg_direction s) of "ASC" -> Just $ Asc PersonEmail ; "DESC" -> Just $ Desc PersonEmail ; _ -> Nothing

    "timezone" -> case (sortJsonMsg_direction s) of "ASC" -> Just $ Asc PersonTimezone ; "DESC" -> Just $ Desc PersonTimezone ; _ -> Nothing

    "lastName" -> case (sortJsonMsg_direction s) of "ASC" -> Just $ Asc PersonLastName ; "DESC" -> Just $ Desc PersonLastName ; _ -> Nothing

    "firstName" -> case (sortJsonMsg_direction s) of "ASC" -> Just $ Asc PersonFirstName ; "DESC" -> Just $ Desc PersonFirstName ; _ -> Nothing


    _ -> Nothing

getPersonManyR :: Handler Value
getPersonManyR = do

    filters <- sequence [
            do 
                f <- lookupGetParam "filter" 
                let f' = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) f) :: Maybe [FilterJsonMsg] 
                return $ maybe [] (mapMaybe toDefaultFilterPerson) f'
        ]

    selectOpts <- sequence [
            do
                s <- lookupGetParam "sort"
                rangeOpts <- getRangeSelectOpts
                let s' = (maybe Nothing (decode . LBS.fromChunks .(:[]) . encodeUtf8) s) :: Maybe [SortJsonMsg]
                return $ maybe [] (mapMaybe toDefaultSortPerson) s' ++ rangeOpts
        ]

    entities <- runDB $ selectList (concat filters) (concat selectOpts)

    return $ object [ "entities" .= toJSON entities ]    

getPersonR :: PersonId -> Handler Value
getPersonR key = do
    entity <- runDB $ get key
    jsonToRepJson $ toJSON entity
