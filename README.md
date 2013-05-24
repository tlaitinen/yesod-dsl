# yesod-dsl

A domain specific language and a code generator to create RESTful
JSON-only web services for managing an RDBMS with [Yesod web
framework](http://www.yesodweb.com/),
[Persistent](http://www.yesodweb.com/book/persistent), and
[Esqueleto](http://hackage.haskell.org/package/esqueleto-1.2).

## Features
 * Yesod 1.2-compatible (subsite)
 * boilerplate code for entity validation
 * supported field types : Word32, Word64, Int32, Int64, Text, Bool, Double, TimeOfDay, Day, UTCTime, ZonedTime
 * filtering and sorting code compatible with ExtJS grids
 * support code for implementing polymorphic relations and accessing common fields

## License
 * The code generator is distributed under the terms of [Simplified BSD license](src/LICENSE)

## Why not using Yesod, Persistent and Esqueleto directly?

By all means, please do, they are great tools. However, if you have a
database-intensive web service where the majority of the handlers follow the
simple [CRUD](http://en.wikipedia.org/wiki/Create,_read,_update_and_delete)
pattern, results of GET handlers need to be filtered dynamically based on the
query string in complex ways, there a lot of entities sharing common patterns,
and especially if you are using
[ExtJS](http://www.sencha.com/products/extjs/) or a similar rich
Javascript library for rendering HTML in the browser, then a part of the web
service may well be expressed succinctly using **yesod-dsl**.

## DSL syntax

The syntax for the input of yesod-dsl is illustrated by the following complete
example: 
```
-- single-line comments with double-hyphens
module MyModule; 

import "other.ydsl"; 

class Named {
    name Text check nonEmpty;
    unique Name name;
}

entity Person {
    instance of Named;
    age Int32;
}

entity BlogPost {
    instance of Named;
    authorId PersonId;
}
enum CommentState = PendingComment | AcceptedComment | SpamComment;

entity Comment {
    blogPostId BlogPostId;
    authorId PersonId;
    comment Text;
    time UTCTime;
    commentState CommentState;
    check maxFivePendingComments;
}

route /persons {
    get {
        select p.id, p.* from Person as p
            order by p.name asc
            limit 1000;
        default-filter-sort;
    }
    post {
        insert Person;
    }
}

route /person/#PersonId {
    get {
        select p.* from Person as p where p.id = $1;
    }
    put {
        update Person identified by $1;
    }
    delete {
        delete from Person as p where p.id = $1;
    }
}

route /blogposts {
    get {
        public;
        select bp.id, bp.*, p.name as authorName 
            from BlogPost as bp
            inner join Person as p on p.id = bp.authorId
            order by bp.name asc
            limit 1000;

        if param "blogPostName" = $$ then
            where bp.name like "%" || $$ || "%";
        default-filter-sort;
    }
    post {
        insert BlogPost;
    }
}

route /blogposts/#BlogPostId {
    get {
        public;
        select bp.*, p.name as authorName from BlogPost as bp 
            inner join Person as p on p.id = bp.authorId
            where bp.id = $1;
    }
    put {
        update BlogPost identified by $1;
    }
    delete {
        delete from BlogPost as bp where bp.id = $1;
    }
}

route /comments {
    get {
        public;
        select bp.id, bp.* from BlogPost as bp
            order by bp.name asc
            limit 1000;

        if param "authorId" = $$ then
            inner join Person as p on bp.authorId = p.id
            where p.id = $$;

        default-filter-sort;
    }
    post {
        insert BlogPost;
    }
}

route /comments/#CommentId {
    get {
        public;
        select c.* from Comment as c where c.id = $1;
    }
    put {
        update Comment identified by $1;
    }
    delete {
        delete from Comment as c where c.id = $1;
    }
}
```

## Defining database entities

As shown above, Persistent entities are defined in *entity {}* blocks. The
entity {}-block has the following structure where square brackets []  denote an optional
value and the asterisk * means that the element can be repeated:

```
entity EntityName {
    [instance of ClassName [, ClassName]*;]

    [fieldName [Maybe] FieldType [default defaultValue] [check functionName]*;]*
    
    [unique UniqueName fieldName [, fieldName]*;]*

    [deriving ClassName [, ClassName]*;]

    [check functionName [, functionName]*;]
}
```

An entity can be an instance of a class defined before or after the entity in the DSL files.

Field names must begin with a lower-case letter. Single-quotes can be used to
avoid clashes with reserved words of the DSL.

Built-in values for FieldType are Word32, Word64, Int32, Int64, Text, Bool,
Double, TimeOfDay, Day, UTCTime, ZonedTime. If the FieldType ends in "Id",
then the prefix must be a valid entity or class name.

A field with an "entity class" type is replaced with a number of fields
referencing the Id field in each entity implementing the class. For this
reason, such a field must have Maybe-qualifier.
 
The default value is passed to Persistent as such and taken into account in the
database server. Examples of valid values are: "string", 1.3, and 4.

Defining a field check-function adds a function to the type class
*ModuleName*Validation which must be implemented by the Yesod master site using
the generated subsite. Field check function is executed before modifying
entities in the database. If the check function returns False, the transaction
is aborted and an error message is sent to the callee.

Unique-statement names are prefixed with "Unique" in the resulting Persistent
models-definition and !force-flag is added to allow using Maybe fields in
unique definitions (we assume you know what you're doing).

Deriving-statement can be used to tell Persistent to derive instances for
built-in type classes, such as Typeable (required by Yesod.Auth's User entity).

Entity-wise check-functions are similar to field check-functions but take the
entity as a parameter instead of a single field.

## Enums

Enumerated values are defined with *enum* keyword, for example:

```
enum TrafficLight = Red | Green | Blue;
```

The definition above is translated to:
```
data TrafficLight = Red | Green | Blue deriving (Show, Read, Eq);
derivePersistField "TrafficLight"
deriveJSON id ''TrafficLight
```
## Entity classes

An entity class is defined in an *class {}*-block and is used for two purposes.
The first is to factor commonly used fields along with check functions and
unique definitions into one place, and the second is to help in implementing
polymorphic relations among entities.

The class {}-block has the following structure where square brackets [] denote an
optional value and []* means that the element can be repeated.

```
class ClassName {
    [fieldName [Maybe] FieldType [default defaultValue] [check functionName]*;]*
    
    [unique UniqueName fieldName [, fieldName]*]*;
}
```

The fields and unique definitions are copied to every entity that is an
instance of the entity class. The unique definitions are prefixed with the
entity name.

Also, each field whose type is of the form Maybe *ClassName*Id is replaced by a
number of fields, one for each entity that is an instance of the entity class.

## Routes and handlers

Routes are defined in *route {}* blocks. Only parameters of the form #EntityId
are supported in route paths. Square brackets [] denote an optional value, []* means that the element can be repeated, and the pipe character | indicates alternatives.

```
route /pathPiece[/pathPiece]* {
    [get {
        [public;]
        select [entityAlias.[fieldName | *] [as outputName]]*
               from EntityName as entityAlias
               [[inner join | cross join | left outer join 
                 | right outer join | full outer join] 
                 EntityName as entityAlias 
                 [on entityAlias.field binOp entityAlias.field]]*
               [where expr]
               [order by [entityAlias.fieldName [asc | desc]]*]
               [limit N [offset M]];

        [default-filter-sort;]
        [if param "paramName" = $$ then
            [inner join EntityName as entityAlias on entityAlias.field binOp entityAlias.field]*
            [where expr];]*
    }]
    [put | post | delete {
        [public;]
        [update EntityName identified by inputValue 
           [with { 
               [fieldName = inputRef]
               [, fieldName = inputRef]*  
           }];]*
        [insert EntityName  
           [from { 
               [fieldName = inputRef]
               [, fieldName = inputRef]* 
           }];]*
        [delete from EntityName as entityAlias [where expr];]*
    }]*
}
``` 
where *pathPiece* is either a string constant or an entity key (#EntityId),
and *inputRef* is one of the following:
 * $*i*, reference to the *i*th parameter in the route path 
 * $authId, reference to the return value of *requireAuthId*
 * *attrName*, reference to the attribute in the JSON object parsed from request body
 * constant value (string, integer, or float),
and *expr* allows following SQL expressions (BNF-style grammar):
```
expr: (expr) and (expr)
    | (expr) or (expr)
    | not (expr)
    | valExpr (= | <> | < | > | <= | >= | like | ilike) valExpr
    | entityAlias.field (in | not in) ($i | $$)

valExpr: "string-constant"
       | int-constant
       | float-constant
       | entityAlias.field
       | valExpr || valExpr
       | inputValue

inputValue: $i
          | authId
          | $$
```
where:
 * *$i* refers to the *i*th parameter in the route path
 * *authId* refers to the return value of requireAuthId
 * *$$* refers to the named parameter in the query string

If *public;* is present, then handler can be accessed without authenticating, 
otherwise, requireAuthId is used to authenticate requests.

A GET handler must have a select-query and returns a JSON object with two
attributes *totalCount* and *results*. The attribute *totalCount* has the value
returned by SELECT COUNT without OFFSET- and LIMIT-parameters. The attribute
*results* is a JSON array where each element is a JSON object corresponding to
a row returned by the SELECT query. 

If *default-filter-sort;* is present, then additional ORDER BY, OFFSET, LIMIT,
and WHERE are added based on query string parameters. 

Additional joins and where expressions can be conditionally added based on the
query string parameters by using *if param "paramName" = $$ then* statements.

## Using the generated subsite

In order to use the generated subsite in a scaffolded Yesod site, it suffices to do the following steps:
 * add the generated Haskell modules Handler.MyModule, Handler.MyModule.Enums, and Handler.MyModule.Internal to the .cabal-file, and
 * import Handler.MyModule in Application.hs, and
 * define the instance MyModuleValidation App that implements field and entity check functions, and
 * add migration code 
```haskell
runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateMyModule) p)
        (messageLoggerSource foundation logger)
```
 * add route to the subsite to config/routes
```
/myModule MyModuleR MyModule getMyModule
```
 
## Generated files

Due to the GHC stage restriction considering template Haskell, 
the code generator generates three files that constitute a Yesod subsite: Handler/ModuleName.hs, Handler/ModuleName/Enums.hs, and Handler/ModuleName/Internal.hs.
For the example above, the result is following:

```haskell
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.MyModule 
    ( module Handler.MyModule.Internal ) where
import Handler.MyModule.Internal
import Yesod.Core
import Yesod.Auth
import Yesod.Persist
import Database.Esqueleto
import Prelude
type MyModuleRoute = Route MyModule
 
instance (YesodAuthPersist master,
          MyModuleValidation master,
          YesodPersistBackend master ~ SqlPersistT) => YesodSubDispatch MyModule (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesMyModule)
```

and

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Handler.MyModule.Enums where
import Database.Persist.TH
import Data.Aeson.TH
data CommentState = SpamComment | AcceptedComment | PendingComment deriving (Show, Read, Eq)
derivePersistField "CommentState"
deriveJSON id ''CommentState
```

and

```haskell
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
module Handler.MyModule.Internal where
import Handler.MyModule.Enums
import Prelude
import Database.Esqueleto
import Database.Esqueleto.Internal.Sql (unsafeSqlBinOp)
import qualified Database.Persist as P
import Database.Persist.TH
import Yesod.Auth (requireAuthId, YesodAuth, AuthId, YesodAuthPersist)
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
import Network.HTTP.Types (status200, status400, status404)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Control.Applicative ((<$>), (<*>))  
import qualified Data.HashMap.Lazy as HML

data FilterJsonMsg = FilterJsonMsg {
    filterJsonMsg_type :: Text,
    filterJsonMsg_value :: Text,
    filterJsonMsg_field :: Text,
    filterJsonMsg_property :: Text,
    filterJsonMsg_comparison :: Text
} 
instance FromJSON FilterJsonMsg where
    parseJSON (A.Object v) = FilterJsonMsg <$>
        v .:? "type" .!= "string" <*> 
        v .: "value" <*>
        v .:? "field" .!= "" <*>
        v .:? "property" .!= "" <*>
        v .:? "comparison" .!= "eq"
    parseJSON _ = mzero

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

instance PathPiece a => PathPiece [a] where
    fromPathPiece s = do
        parts <- safeRead s
        values <- mapM fromPathPiece parts
        return values
    toPathPiece values = T.concat [ 
            T.pack "[",
            T.intercalate (T.pack ",") (map toPathPiece values),
            T.pack "]" 
        ]
            

getDefaultFilter maybeGetParam defaultFilterJson p = do
    f <- maybe maybeGetParam Just getFilter
    fromPathPiece f
    where 
        getFilter = do            
            j <- defaultFilterJson
            v <- DL.find (\fjm -> filterJsonMsg_property fjm == p) j
            return (filterJsonMsg_value v)
share [mkPersist sqlOnlySettings, mkMigrate "migrateMyModule" ] [persistLowerCase|
Person json
    age Int32  
    name Text  
    UniquePersonName name !force
BlogPost json
    authorId PersonId  
    name Text  
    UniqueBlogPostName name !force
Comment json
    blogPostId BlogPostId  
    authorId PersonId  
    comment Text  
    time UTCTime  
    commentState CommentState  
|]
class Named a where
    namedName :: a -> Text
instance Named Person where
    namedName = personName
instance Named BlogPost where
    namedName = blogPostName
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
                 MyModuleValidation master) 
             => a -> HandlerT master IO [Text]

class Yesod master => MyModuleValidation master where
    nonEmpty :: (YesodPersist master) => Text -> HandlerT master IO Bool
    maxFivePendingComments :: (P.PersistMonadBackend (b (HandlerT master IO)) ~ P.PersistEntityBackend Comment, 
                b ~ YesodPersistBackend master,
                P.PersistQuery (b (HandlerT master IO)),
                P.PersistUnique (b (HandlerT master IO)),
                YesodPersist master)
            => Comment -> HandlerT master IO Bool
instance Validatable Person where
    validate v = do
        results <- sequence [
                checkResult "Person.name nonEmpty" (nonEmpty $ personName v)            ]
        return $ catMaybes results
instance Validatable BlogPost where
    validate v = do
        results <- sequence [
                checkResult "BlogPost.name nonEmpty" (nonEmpty $ blogPostName v)            ]
        return $ catMaybes results
instance Validatable Comment where
    validate v = do
        results <- sequence [
                checkResult "Comment maxFivePendingComments" $ maxFivePendingComments v            ]
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
getPersonsR :: forall master. (MyModuleValidation master, 
    YesodAuthPersist master,
    YesodPersistBackend master ~ SqlPersistT)
    => HandlerT MyModule (HandlerT master IO) A.Value
getPersonsR  = do
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
        let pId' = p ^. PersonId

        _ <- if limitOffsetOrder
            then do 
                offset 0
                limit 1000
                case defaultSortJson of 
                    Just xs -> mapM_ (\sjm -> case sortJsonMsg_property sjm of
                            "age" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (p ^. PersonAge) ] 
                                "DESC" -> orderBy [ desc (p ^. PersonAge) ] 
                                _      -> return ()
                            "name" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (p ^. PersonName) ] 
                                "DESC" -> orderBy [ desc (p ^. PersonName) ] 
                                _      -> return ()
                
                            _ -> return ()
                        ) xs
                    Nothing -> orderBy [ asc (p ^. PersonName) ]

                case defaultOffset of
                    Just o -> offset o
                    Nothing -> return ()
                case defaultLimit of
                    Just l -> limit (min 10000 l)
                    Nothing -> return ()
                 
            else return ()
        case defaultFilterJson of 
            Just xs -> mapM_ (\fjm -> case filterJsonMsg_field fjm of
                "age" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (p ^. PersonAge) (val v) 
                    _        -> return ()
                "name" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (p ^. PersonName) (val v) 
                    _        -> return ()

                _ -> return ()
                ) xs
            Nothing -> return ()  
        return (p ^. PersonId, p ^. PersonAge, p ^. PersonName)
    count <- lift $ runDB $ select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- lift $ runDB $ select $ baseQuery True
    return $ A.object [
        "totalCount" .= (T.pack $ (\(Database.Esqueleto.Value v) -> show (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3)) -> A.object [
                    "id" .= toJSON f1,
                    "age" .= toJSON f2,
                    "name" .= toJSON f3                                    
                    ]
                _ -> A.object []
            ) results)
       ]
postPersonsR :: forall master. (MyModuleValidation master, 
    YesodAuthPersist master,
    YesodPersistBackend master ~ SqlPersistT)
    => HandlerT MyModule (HandlerT master IO) A.Value
postPersonsR  = do
    authId <- lift $ requireAuthId
    yReq <- getRequest
    let wReq = reqWaiRequest yReq
    bss <- liftIO $ runResourceT $ lazyConsume $ W.requestBody wReq
    jsonBody <- case AP.eitherResult $ AP.parse A.json (B.concat bss) of
         Left err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         Right o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    e1 <- case A.fromJSON jsonBody of
        A.Success e -> return e
        A.Error err -> sendResponseStatus status400 ("Could not decode an entity of type Person from JSON object in the request body : " ++ err )
    _ <- lift $ runDB $ do
        vErrors <- lift $ validate e1
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> P.insert (e1 :: Person)
    return $ A.Null
getPersonPersonIdR :: forall master. (MyModuleValidation master, 
    YesodAuthPersist master,
    YesodPersistBackend master ~ SqlPersistT)
    => PersonId -> HandlerT MyModule (HandlerT master IO) A.Value
getPersonPersonIdR p1 = do
    authId <- lift $ requireAuthId
    let baseQuery limitOffsetOrder = from $ \(p ) -> do
        let pId' = p ^. PersonId
        where_ ((p ^. PersonId) ==. ((val p1)))

        _ <- if limitOffsetOrder
            then do 
                offset 0
                limit 10000

                 
            else return ()
        return (p ^. PersonAge, p ^. PersonName)
    count <- lift $ runDB $ select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- lift $ runDB $ select $ baseQuery True
    return $ A.object [
        "totalCount" .= (T.pack $ (\(Database.Esqueleto.Value v) -> show (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2)) -> A.object [
                    "age" .= toJSON f1,
                    "name" .= toJSON f2                                    
                    ]
                _ -> A.object []
            ) results)
       ]
putPersonPersonIdR :: forall master. (MyModuleValidation master, 
    YesodAuthPersist master,
    YesodPersistBackend master ~ SqlPersistT)
    => PersonId -> HandlerT MyModule (HandlerT master IO) A.Value
putPersonPersonIdR p1 = do
    authId <- lift $ requireAuthId
    yReq <- getRequest
    let wReq = reqWaiRequest yReq
    bss <- liftIO $ runResourceT $ lazyConsume $ W.requestBody wReq
    jsonBody <- case AP.eitherResult $ AP.parse A.json (B.concat bss) of
         Left err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         Right o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    e1 <- case A.fromJSON jsonBody of
        A.Success e -> return e
        A.Error err -> sendResponseStatus status400 ("Could not decode an entity of type Person from JSON object in the request body : " ++ err )
    _ <- lift $ runDB $ do
       vErrors <- lift $ validate e1
       case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> P.repsert p1 (e1 :: Person)
    return $ A.Null
deletePersonPersonIdR :: forall master. (MyModuleValidation master, 
    YesodAuthPersist master,
    YesodPersistBackend master ~ SqlPersistT)
    => PersonId -> HandlerT MyModule (HandlerT master IO) A.Value
deletePersonPersonIdR p1 = do
    authId <- lift $ requireAuthId
    _ <- lift $ runDB $ do
        delete $ from $ (\p -> where_ $ (p ^. PersonId) ==. ((val p1)))
    return $ A.Null
getBlogpostsR :: forall master. (MyModuleValidation master, 
    YesodAuthPersist master,
    YesodPersistBackend master ~ SqlPersistT)
    => HandlerT MyModule (HandlerT master IO) A.Value
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
        on ((p ^. PersonId) ==. (bp ^. BlogPostAuthorId))
        let bpId' = bp ^. BlogPostId

        _ <- if limitOffsetOrder
            then do 
                offset 0
                limit 1000
                case defaultSortJson of 
                    Just xs -> mapM_ (\sjm -> case sortJsonMsg_property sjm of
                            "authorId" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (bp ^. BlogPostAuthorId) ] 
                                "DESC" -> orderBy [ desc (bp ^. BlogPostAuthorId) ] 
                                _      -> return ()
                            "name" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (bp ^. BlogPostName) ] 
                                "DESC" -> orderBy [ desc (bp ^. BlogPostName) ] 
                                _      -> return ()
                            "authorName" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (p ^. PersonName) ] 
                                "DESC" -> orderBy [ desc (p ^. PersonName) ] 
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
            Just xs -> mapM_ (\fjm -> case filterJsonMsg_field fjm of
                "age" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (p ^. PersonAge) (val v) 
                    _        -> return ()
                "name" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (p ^. PersonName) (val v) 
                    _        -> return ()
                "authorId" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (bp ^. BlogPostAuthorId) (val v) 
                    _        -> return ()
                "name" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (bp ^. BlogPostName) (val v) 
                    _        -> return ()

                _ -> return ()
                ) xs
            Nothing -> return ()  
        case getDefaultFilter filterParam_blogPostName defaultFilterJson "blogPostName" of
            Just localParam -> do 

                where_ $ (bp ^. BlogPostName) `like` (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%"))))
            Nothing -> return ()
        return (bp ^. BlogPostId, bp ^. BlogPostAuthorId, bp ^. BlogPostName, p ^. PersonName)
    count <- lift $ runDB $ select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- lift $ runDB $ select $ baseQuery True
    return $ A.object [
        "totalCount" .= (T.pack $ (\(Database.Esqueleto.Value v) -> show (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3), (Database.Esqueleto.Value f4)) -> A.object [
                    "id" .= toJSON f1,
                    "authorId" .= toJSON f2,
                    "name" .= toJSON f3,
                    "authorName" .= toJSON f4                                    
                    ]
                _ -> A.object []
            ) results)
       ]
postBlogpostsR :: forall master. (MyModuleValidation master, 
    YesodAuthPersist master,
    YesodPersistBackend master ~ SqlPersistT)
    => HandlerT MyModule (HandlerT master IO) A.Value
postBlogpostsR  = do
    authId <- lift $ requireAuthId
    yReq <- getRequest
    let wReq = reqWaiRequest yReq
    bss <- liftIO $ runResourceT $ lazyConsume $ W.requestBody wReq
    jsonBody <- case AP.eitherResult $ AP.parse A.json (B.concat bss) of
         Left err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         Right o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    e1 <- case A.fromJSON jsonBody of
        A.Success e -> return e
        A.Error err -> sendResponseStatus status400 ("Could not decode an entity of type BlogPost from JSON object in the request body : " ++ err )
    _ <- lift $ runDB $ do
        vErrors <- lift $ validate e1
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> P.insert (e1 :: BlogPost)
    return $ A.Null
getBlogpostsBlogPostIdR :: forall master. (MyModuleValidation master, 
    YesodAuthPersist master,
    YesodPersistBackend master ~ SqlPersistT)
    => BlogPostId -> HandlerT MyModule (HandlerT master IO) A.Value
getBlogpostsBlogPostIdR p1 = do
    let baseQuery limitOffsetOrder = from $ \(bp  `InnerJoin` p) -> do
        on ((p ^. PersonId) ==. (bp ^. BlogPostAuthorId))
        let bpId' = bp ^. BlogPostId
        where_ ((bp ^. BlogPostId) ==. ((val p1)))

        _ <- if limitOffsetOrder
            then do 
                offset 0
                limit 10000

                 
            else return ()
        return (bp ^. BlogPostAuthorId, bp ^. BlogPostName, p ^. PersonName)
    count <- lift $ runDB $ select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- lift $ runDB $ select $ baseQuery True
    return $ A.object [
        "totalCount" .= (T.pack $ (\(Database.Esqueleto.Value v) -> show (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3)) -> A.object [
                    "authorId" .= toJSON f1,
                    "name" .= toJSON f2,
                    "authorName" .= toJSON f3                                    
                    ]
                _ -> A.object []
            ) results)
       ]
putBlogpostsBlogPostIdR :: forall master. (MyModuleValidation master, 
    YesodAuthPersist master,
    YesodPersistBackend master ~ SqlPersistT)
    => BlogPostId -> HandlerT MyModule (HandlerT master IO) A.Value
putBlogpostsBlogPostIdR p1 = do
    authId <- lift $ requireAuthId
    yReq <- getRequest
    let wReq = reqWaiRequest yReq
    bss <- liftIO $ runResourceT $ lazyConsume $ W.requestBody wReq
    jsonBody <- case AP.eitherResult $ AP.parse A.json (B.concat bss) of
         Left err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         Right o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    e1 <- case A.fromJSON jsonBody of
        A.Success e -> return e
        A.Error err -> sendResponseStatus status400 ("Could not decode an entity of type BlogPost from JSON object in the request body : " ++ err )
    _ <- lift $ runDB $ do
       vErrors <- lift $ validate e1
       case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> P.repsert p1 (e1 :: BlogPost)
    return $ A.Null
deleteBlogpostsBlogPostIdR :: forall master. (MyModuleValidation master, 
    YesodAuthPersist master,
    YesodPersistBackend master ~ SqlPersistT)
    => BlogPostId -> HandlerT MyModule (HandlerT master IO) A.Value
deleteBlogpostsBlogPostIdR p1 = do
    authId <- lift $ requireAuthId
    _ <- lift $ runDB $ do
        delete $ from $ (\bp -> where_ $ (bp ^. BlogPostId) ==. ((val p1)))
    return $ A.Null
getCommentsR :: forall master. (MyModuleValidation master, 
    YesodAuthPersist master,
    YesodPersistBackend master ~ SqlPersistT)
    => HandlerT MyModule (HandlerT master IO) A.Value
getCommentsR  = do
    filterParam_authorId <- lookupGetParam "authorId"
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
                                "ASC"  -> orderBy [ asc (bp ^. BlogPostAuthorId) ] 
                                "DESC" -> orderBy [ desc (bp ^. BlogPostAuthorId) ] 
                                _      -> return ()
                            "name" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (bp ^. BlogPostName) ] 
                                "DESC" -> orderBy [ desc (bp ^. BlogPostName) ] 
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
            Just xs -> mapM_ (\fjm -> case filterJsonMsg_field fjm of
                "authorId" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (bp ^. BlogPostAuthorId) (val v) 
                    _        -> return ()
                "name" -> case (fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v) -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (bp ^. BlogPostName) (val v) 
                    _        -> return ()

                _ -> return ()
                ) xs
            Nothing -> return ()  
        case getDefaultFilter filterParam_authorId defaultFilterJson "authorId" of
            Just localParam -> from $ \(p) -> do
 
                where_ ((bp ^. BlogPostAuthorId) ==. (p ^. PersonId))

                where_ $ (p ^. PersonId) ==. ((val localParam))
            Nothing -> return ()
        return (bp ^. BlogPostId, bp ^. BlogPostAuthorId, bp ^. BlogPostName)
    count <- lift $ runDB $ select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- lift $ runDB $ select $ baseQuery True
    return $ A.object [
        "totalCount" .= (T.pack $ (\(Database.Esqueleto.Value v) -> show (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3)) -> A.object [
                    "id" .= toJSON f1,
                    "authorId" .= toJSON f2,
                    "name" .= toJSON f3                                    
                    ]
                _ -> A.object []
            ) results)
       ]
postCommentsR :: forall master. (MyModuleValidation master, 
    YesodAuthPersist master,
    YesodPersistBackend master ~ SqlPersistT)
    => HandlerT MyModule (HandlerT master IO) A.Value
postCommentsR  = do
    authId <- lift $ requireAuthId
    yReq <- getRequest
    let wReq = reqWaiRequest yReq
    bss <- liftIO $ runResourceT $ lazyConsume $ W.requestBody wReq
    jsonBody <- case AP.eitherResult $ AP.parse A.json (B.concat bss) of
         Left err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         Right o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    e1 <- case A.fromJSON jsonBody of
        A.Success e -> return e
        A.Error err -> sendResponseStatus status400 ("Could not decode an entity of type BlogPost from JSON object in the request body : " ++ err )
    _ <- lift $ runDB $ do
        vErrors <- lift $ validate e1
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> P.insert (e1 :: BlogPost)
    return $ A.Null
getCommentsCommentIdR :: forall master. (MyModuleValidation master, 
    YesodAuthPersist master,
    YesodPersistBackend master ~ SqlPersistT)
    => CommentId -> HandlerT MyModule (HandlerT master IO) A.Value
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
putCommentsCommentIdR :: forall master. (MyModuleValidation master, 
    YesodAuthPersist master,
    YesodPersistBackend master ~ SqlPersistT)
    => CommentId -> HandlerT MyModule (HandlerT master IO) A.Value
putCommentsCommentIdR p1 = do
    authId <- lift $ requireAuthId
    yReq <- getRequest
    let wReq = reqWaiRequest yReq
    bss <- liftIO $ runResourceT $ lazyConsume $ W.requestBody wReq
    jsonBody <- case AP.eitherResult $ AP.parse A.json (B.concat bss) of
         Left err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         Right o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    e1 <- case A.fromJSON jsonBody of
        A.Success e -> return e
        A.Error err -> sendResponseStatus status400 ("Could not decode an entity of type Comment from JSON object in the request body : " ++ err )
    _ <- lift $ runDB $ do
       vErrors <- lift $ validate e1
       case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> P.repsert p1 (e1 :: Comment)
    return $ A.Null
deleteCommentsCommentIdR :: forall master. (MyModuleValidation master, 
    YesodAuthPersist master,
    YesodPersistBackend master ~ SqlPersistT)
    => CommentId -> HandlerT MyModule (HandlerT master IO) A.Value
deleteCommentsCommentIdR p1 = do
    authId <- lift $ requireAuthId
    _ <- lift $ runDB $ do
        delete $ from $ (\c -> where_ $ (c ^. CommentId) ==. ((val p1)))
    return $ A.Null

data MyModule = MyModule

getMyModule :: a -> MyModule
getMyModule = const MyModule

mkYesodSubData "MyModule" [parseRoutes|
/persons        PersonsR      GET POST
/person/#PersonId        PersonPersonIdR      GET PUT DELETE
/blogposts        BlogpostsR      GET POST
/blogposts/#BlogPostId        BlogpostsBlogPostIdR      GET PUT DELETE
/comments        CommentsR      GET POST
/comments/#CommentId        CommentsCommentIdR      GET PUT DELETE
|]
```
