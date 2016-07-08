# yesod-dsl

A domain specific language and a code generator to create RESTful
JSON-only web services for managing an RDBMS with [Yesod web
framework](http://www.yesodweb.com/),
[Persistent](http://www.yesodweb.com/book/persistent), and
[Esqueleto](http://hackage.haskell.org/package/esqueleto).

## Features
 * boilerplate code for entity validation, and filtering and sorting the results 
 * supported field types : Word32, Word64, Int, Int32, Int64, Text, Bool, Double, TimeOfDay, Day, UTCTime, ZonedTime, Checkmark
 * support code for implementing polymorphic relations and accessing common fields
 * generates Haskell and [PureScript](http://purescript.org) (client-side) code to access the web service 

## License
 * The code generator is distributed under the terms of [Simplified BSD license](LICENSE)

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

Note that the current version is experimental and all of the DSL syntax may not
be handled correctly. However, if the generated code compiles, it probably does
the right thing. If it does not compile, please post an issue with a minimal
example reproducing the issue.

## Status

This project is not actively developed anymore because I'm focusing my efforts
on [servant](https://github.com/haskell-servant/servant) instead of
[yesod](http://www.yesodweb.com/). Servant is more suitable for developing an
API server than Yesod, requires less boilerplate and can generate client
code, so it renders this project largely obsolete.

## DSL syntax

The syntax for the input of yesod-dsl is illustrated by the following complete
example: 
```
-- single-line comments with double-hyphens
module Example;

import "versioned.ydsl";
import Handler.Utils (nonEmpty, maxTwoPendingComments);

class Named {
    name Text check nonEmpty;
    unique Name name;
}

entity User {
    instance of Named;
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
entity EntityName [sql "table name"] {
    [instance of ClassName [, ClassName]*;]

    [fieldName [Maybe] FieldType [default defaultValue] [sql "table-column-name"] [json "json-field-name"] [check functionName]* [internal];]*
    
    [unique UniqueName fieldName [, fieldName]*;]*

    [deriving ClassName [, ClassName]*;]

    [check functionName [, functionName]*;]
}
```

An entity can be an instance of a class defined before or after the entity in the DSL files.

Field names must begin with a lower-case letter. Single-quotes can be used to
avoid clashes with reserved words of the DSL.

Built-in values for FieldType are Word32, Word64, Int32, Int64, Text, Bool,
Double, TimeOfDay, Day, UTCTime, ZonedTime, and Checkmark. If the FieldType
ends in "Id", then the prefix must be a valid entity or class name.

A field with an "entity class" type is replaced with a number of fields
referencing the Id field in each entity implementing the class. For this
reason, such a field must have Maybe-qualifier.
 
The default value is passed to Persistent as such and taken into account in the
database server. Examples of valid values are: "string", 1.3, and 4.

Defining a field check-function adds a function to the type class
*ModuleName*Interface which must be implemented by the Yesod master site using
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

 * A field whose type is of the form Maybe *ClassName*Id is replaced by a
number of fields, one for each entity that is an instance of the entity class.
 * A field whose type is ClassInstanceId is replaced by a field referencing the entity currently instantiating the class, e.g. 
   ```class HasParent { parentId ClassInstanceId; } entity Person { instance of HasParent; }``` results in Person entity having the field ```parentId PersonId; ```



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
           [from [variableName] { 
               [fieldName = inputValue]
               [, fieldName = inputValue]* 
           }];]*
        [delete from EntityName as entityAlias [where expr];]*
        [for paramName in inputValue {
            -- require, get, update, insert, delete, etc.
         };];
        [externalFunctionName [inputValue ]*;]*
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
    | exists (sub_select)
     
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
       | random
       | floor(valExpr)
       | ceiling(valExpr)
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
          | CheckmarkActive
          | CheckmarkInactive

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

## Using the generated subsite


In order to use the generated subsite in a scaffolded Yesod site, it suffices to do the following steps:
 * import Handler.MyModule in Application.hs, and
 * define the instance MyModuleInterface App that implements field and entity check functions, and
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

## Examples

See [yesod-dsl wiki](https://github.com/tlaitinen/yesod-dsl/wiki) for examples.



