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

## How to get yesod-dsl?

```
cabal install yesod-dsl
```

Note that the current version is experimental and all of the DSL syntax is
probably not handled correctly.

## DSL syntax

The syntax for the input of yesod-dsl is illustrated by the following complete
example: 
```
-- single-line comments with double-hyphens
module Example;

import "versioned.ydsl";

class Named {
    name Text check nonEmpty;
    unique Name name;
}

entity User {
    instance of Named, Versioned;
    firstName Text;
    lastName Text;
    age      Maybe Int32;
    deriving Typeable;
}

entity Group {
    instance of Named;
}

entity BlogPost {
    instance of Named;
    authorId UserId;
    content Text;
    time UTCTime;
}
enum CommentState = Pending | Accepted | Spam;

entity Comment {
    blogPostId BlogPostId;
    authorId UserId;
    comment Text;
    time UTCTime;
    commentState CommentState;
    check maxTwoPendingComments;
}

route /users {
    get {
        select p.id, p.* from User as p
            order by p.name asc
            limit 1000;
        default-filter-sort;
    }
 
}

route /users/#UserId {
    put {
        require User as u where u.id = $1 and $1 = $auth.id;
        update User identified by $1 with {
            firstName = $request.firstName,
            lastName = $request.lastName,
            age = $request.age
        };
    }
}

route /blogposts {
    get {
        public;
        select bp.id, bp.*, 
               p.firstName as authorFirstName, 
               p.lastName as authorLastName 
            from BlogPost as bp
            inner join User as p on p.id = bp.authorId
            order by bp.name asc
            limit 1000;

        if param "blogPostName" = $$ then
            where bp.name ilike "%" || $$ || "%"
                  or p.firstName ilike "%" || $$ || "%"
                  or p.lastName ilike "%" || $$ || "%";
        default-filter-sort;
    }
    post {
        insert BlogPost from {
            authorId = $auth.id,
            name = $request.name,
            content = $request.content,
            time = now()
        };
    }
}

route /blogposts/#BlogPostId {
    put {
        update BlogPost identified by $1;
    }
}

route /comments {
    get {
        public;
        select bp.id, bp.* from BlogPost as bp
            order by bp.name asc
            limit 1000;

        if param "authorId" = $$ then
            inner join User as p on bp.authorId = p.id
            where p.id = $$;

        if param "query" = $$ then
            where bp.content ilike "%" || $$ || "%";

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
data TrafficLight = TrafficLightBlue | TrafficLightGreen | TrafficLightRed deriving (Eq)

instance Read TrafficLight where
    readsPrec _ ('B':'l':'u':'e':xs) = [ (TrafficLightBlue, xs) ]
    readsPrec _ ('G':'r':'e':'e':'n':xs) = [ (TrafficLightGreen, xs) ]
    readsPrec _ ('R':'e':'d':xs) = [ (TrafficLightRed, xs) ]
    readsPrec _ _ = [ ]

instance Show TrafficLight where
    show TrafficLightBlue = "Blue"
    show TrafficLightGreen = "Green"
    show TrafficLightRed = "Red"


derivePersistField "TrafficLight"
instance A.FromJSON TrafficLight where
    parseJSON = A.withText "TrafficLight" (\v -> case v of
        "Blue" -> return TrafficLightBlue
        "Green" -> return TrafficLightGreen
        "Red" -> return TrafficLightRed
        _ -> mzero)

instance A.ToJSON TrafficLight where
    toJSON TrafficLightBlue = A.String "Blue"
    toJSON TrafficLightGreen = A.String "Green"
    toJSON TrafficLightRed = A.String "Red"


instance PathPiece TrafficLight where
    fromPathPiece "Blue" = Just TrafficLightBlue
    fromPathPiece "Green" = Just TrafficLightGreen
    fromPathPiece "Red" = Just TrafficLightRed

    fromPathPiece _ = Nothing
    toPathPiece TrafficLightBlue = "Blue"
    toPathPiece TrafficLightGreen = "Green"
    toPathPiece TrafficLightRed = "Red"
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
        [require EntityName as entityAlias
            [[inner join | left outer join] 
                     EntityName as entityAlias 
                     [on entityAlias.field binOp entityAlias.field]]*
                   [where expr];]*
        
        select [entityAlias.[fieldName | *] [as outputName]]*
               from EntityName as entityAlias
               [[inner join | left outer join] 
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
        [require EntityName as entityAlias
            [[inner join | left outer join] 
                     EntityName as entityAlias 
                     [on entityAlias.field binOp entityAlias.field]]*
                   [where expr];]*
        
        [variableName <- get EntityName identified by inputValue;]*
        [update EntityName identified by inputValue 
           [with { 
               [fieldName = inputValue]
               [, fieldName = inputValue]*  
           }];]*
        [[variableName <- ] insert EntityName  
           [from { 
               [fieldName = inputValue]
               [, fieldName = inputValue]* 
           }];]*
        [delete from EntityName as entityAlias [where expr];]*
        [return { [fieldName = outputValue]
                  [, fieldName = outputValue]* };]
    }]*
}
``` 
where *pathPiece* is either a string constant or an entity key (#EntityId),
and *expr* allows following SQL expressions (BNF-style grammar):
```
expr: (expr) and (expr)
    | (expr) or (expr)
    | not (expr)
    | valExpr (= | <> | < | > | <= | >= | like | ilike) valExpr
    | entityAlias.field (in | not in) ($i | $$ | sub_select)

valExpr: "string-constant"
       | int-constant
       | float-constant
       | (True | False)
       | Nothing
       | entityAlias.field
       | valExpr || valExpr
       | inputValue
       | enumName.enumValue

inputValue: $i
          | $auth.id
          | $auth.field
          | $$
          | $request.field
          | now()
          | variableName
          | variableName.field

outputValue: variableName

sub_select: (select entityAlias.fieldName 
             from EntityName as entityAlias
             [[inner join | left outer join] 
               EntityName as entityAlias 
               [on entityAlias.field binOp entityAlias.field]]*
             [where expr])
```
where:
 * $i refers to the *i*th parameter in the route path
 * *$auth.id* refers to the return value of requireAuthId
 * *$auth.field$ refers to the field of the entity User returned by requireAuth
 * *$$* refers to the named parameter in the query string
 * request refers to the JSON object parsed from the request body

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
 * add the following packages to build-depends: 
    ```
    esqueleto 
    , yesod-persistent              
    , unordered-containers 
    , blaze-builder
    , http-types   
    , wai
    , resourcet
    , attoparsec
    , time
    , path-pieces
    ```
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

Due to the GHC stage restriction, 
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
module Handler.Example 
    ( module Handler.Example.Enums, module Handler.Example.Internal ) where
import Handler.Example.Enums
import Handler.Example.Internal
import Yesod.Core
import Yesod.Auth
import Yesod.Persist
import Database.Esqueleto
import Prelude
type ExampleRoute = Route Example
 
instance (YesodAuthPersist master,
          ExampleValidation master,
          KeyEntity (AuthId master) ~ User,
          YesodPersistBackend master ~ SqlPersistT) => YesodSubDispatch Example (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesExample)
```

and

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Handler.Example.Enums where
import Database.Persist.TH
import qualified Data.Aeson as A
import Prelude
import Web.PathPieces
import Control.Monad (mzero)
data CommentState = CommentStateSpam | CommentStateAccepted | CommentStatePending deriving (Eq)

instance Read CommentState where
    readsPrec _ ('S':'p':'a':'m':xs) = [ (CommentStateSpam, xs) ]
    readsPrec _ ('A':'c':'c':'e':'p':'t':'e':'d':xs) = [ (CommentStateAccepted, xs) ]
    readsPrec _ ('P':'e':'n':'d':'i':'n':'g':xs) = [ (CommentStatePending, xs) ]
    readsPrec _ _ = [ ]

instance Show CommentState where
    show CommentStateSpam = "Spam"
    show CommentStateAccepted = "Accepted"
    show CommentStatePending = "Pending"

    
derivePersistField "CommentState"

instance A.FromJSON CommentState where
    parseJSON = A.withText "CommentState" (\v -> case v of
        "Spam" -> return CommentStateSpam
        "Accepted" -> return CommentStateAccepted
        "Pending" -> return CommentStatePending
        _ -> mzero)

instance A.ToJSON CommentState where
    toJSON CommentStateSpam = A.String "Spam"
    toJSON CommentStateAccepted = A.String "Accepted"
    toJSON CommentStatePending = A.String "Pending"


instance PathPiece CommentState where
    fromPathPiece "Spam" = Just CommentStateSpam
    fromPathPiece "Accepted" = Just CommentStateAccepted
    fromPathPiece "Pending" = Just CommentStatePending

    fromPathPiece _ = Nothing
    toPathPiece CommentStateSpam = "Spam"
    toPathPiece CommentStateAccepted = "Accepted"
    toPathPiece CommentStatePending = "Pending"
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
```
