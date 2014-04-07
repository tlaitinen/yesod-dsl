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

    [fieldName [Maybe] FieldType [default defaultValue] [check functionName]* [internal];]*
    
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

Fields can be marked as internal by adding the keyword "internal" in the end of
a field definition. Internal fields cannot be returned in SQL SELECT queries defined in route handlers.

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
    [fieldName [Maybe] FieldType [default defaultValue] [check functionName]* [internal];]*
    
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
        
        select [[entityAlias.[fieldName | *] | (expr)] [as outputName]]*
               from EntityName as entityAlias
               [[inner join | left outer join] 
                 EntityName as entityAlias 
                 [on entityAlias.field binOp entityAlias.field]]*
               [where expr]
               [order by [entityAlias.fieldName [asc | desc]]*]
               [limit N [offset M]];

        [default-filter-sort;]
        [if param "paramName" = [$$ | _] then
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
        [for paramName in inputValue {
            -- require, get, update, insert, delete, etc.
         };];
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
    | extract (subField from entityAlias.field)
     
valExpr: "string-constant"
       | int-constant
       | float-constant
       | (True | False)
       | Nothing
       | entityAlias.field
       | valExpr || valExpr
       | valExpr + valExpr
       | valExpr - valExpr
       | valExpr * valExpr
       | valExpr / valExpr
       | inputValue
       | enumName.enumValue
       | defineName([param[, param]*])

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

subField: century | day | decade | dow | doy | epoch | hour | isodow
        | microseconds | millennium | milliseconds | minute | month
        | quarter | second | timezone | timezone_hour | timezone_minute
        | week | year
```
where:
 * $i refers to the *i*th parameter in the route path
 * *$auth.id* refers to the return value of requireAuthId
 * *$auth.field* refers to the field of the entity User returned by requireAuth
 * *$$* refers to the named parameter in the query string
 * *_* is a value for a parameter that is required but not used
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

## Define-statement (Commonly used sub-queries)

Commonly used sub-queries, e.g. filtering the result set according to a user/user group-specific access control list, may be factored out using *define* statement.

The example below illustrates the use of *define* statement. The syntax is
identical to inline sub-queries used in route handlers with the exception of
allowing field name parameters in curly braces { }.
```
define hasReadPerm(field) =
    select ugc.{field} from UserGroupContent as ugc
    where ugc.userGroupId in
        (select ug.id from UserGroup as ug
         inner join UserGroupItem as ugi
         on ug.id = ugi.userGroupId
         where ugi.userId = $auth.id
         and ugi.deletedVersionId is Nothing));

class Restricted {
}
class Deletable {
    deletedVersionId Maybe VersionId;
}
entity UserGroup {
    instance of Named, Versioned, Deletable, Restricted;
    unique Group name;
}
entity UserGroupContent {
    instance of Deletable;

    userGroupId UserGroupId;
    contentId Maybe RestrictedId;
}
```

The example below illustrates how the defined sub-query can be used in
route handlers:
```
route /usergroups {
        get {
            -- returns UserGroup entities which the user is allowed to read
            select ug.id, ug.* from UserGroup as ug
                where ug.id in hasReadPerm(userGroupContentId)
                order by ug.name asc;
            default-filter-sort;
        }
}
```

After substituting *hasReadPerm(userGroupContentId)* the route handler
definition above is following:

```
route /usergroups {
        get {
            -- returns UserGroup entities which the user is allowed to read
            select ug.id, ug.* from UserGroup as ug
                where ug.id in 
                    (select ugc.userGroupContentId from UserGroupContent as ugc
                     where ugc.userGroupId in
                        (select ug.id from UserGroup as ug
                            inner join UserGroupItem as ugi
                            on ug.id = ugi.userGroupId
                            where ugi.userId = $auth.id
                            and ugi.deletedVersionId is Nothing));
                order by ug.name asc;
            default-filter-sort;
        }
}
```


## Using the generated subsite



In order to use the generated subsite in a scaffolded Yesod site, it suffices to do the following steps:
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

If the top-level .ydsl file has a corresponding .cabal file, it is updated
automatically using Cabal library's parser and pretty printer. Note that
comments and layout are not preserved when updating .cabal-file.

Otherwise, the following steps are required:
 * add the generated Haskell modules Handler.MyModule, Handler.MyModule.Route*, Handler.MyModule.Enums, Handler.MyModule.Routes, Handler.MyModule.Esqueleto, and Handler.MyModule.Internal to the .cabal-file, and
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
    , transformers
    , tagged
    ```

## Generated files

Due to the GHC stage restriction (and ghc's tendency to blow up when optimizing large Haskell modules), 
the code generator generates a number of Haskell modules that constitute a Yesod subsite: Handler/ModuleName.hs, Handler/ModuleName/Enums.hs, Handler/ModuleName/Routes.hs, Handler/ModuleName/Esqueleto.hs, Handler/ModuleName/Route*.hs, and Handler/ModuleName/Internal.hs.
For the example above, the result can be examined in the following links:
 * [Handler.Example](https://github.com/tlaitinen/yesod-dsl/blob/master/example/Handler/Example.hs)
 * [Handler.Example.Enums](https://github.com/tlaitinen/yesod-dsl/blob/master/example/Handler/Example/Enums.hs)
 * [Handler.Example.Esqueleto](https://github.com/tlaitinen/yesod-dsl/blob/master/example/Handler/Example/Esqueleto.hs)
 * [Handler.Example.Internal](https://github.com/tlaitinen/yesod-dsl/blob/master/example/Handler/Example/Internal.hs)
 * [Handler.Example.RouteBlogposts](https://github.com/tlaitinen/yesod-dsl/blob/master/example/Handler/Example/RouteBlogposts.hs)
 * [Handler.Example.RouteComments](https://github.com/tlaitinen/yesod-dsl/blob/master/example/Handler/Example/RouteComments.hs)
 * [Handler.Example.RouteCommentsComment](https://github.com/tlaitinen/yesod-dsl/blob/master/example/Handler/Example/RouteCommentsComment.hs)
 * [Handler.Example.RouteUsers](https://github.com/tlaitinen/yesod-dsl/blob/master/example/Handler/Example/RouteUsers.hs)
 * [Handler.Example.RouteUsersUser](https://github.com/tlaitinen/yesod-dsl/blob/master/example/Handler/Example/RouteUsersUser.hs)
 * [Handler.Example.Routes](https://github.com/tlaitinen/yesod-dsl/blob/master/example/Handler/Example/Routes.hs)

