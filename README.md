# yesod-dsl

A domain specific language and a code generator to create RESTful
JSON-only web services for managing an SQL database with [Yesod web
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
    time DateTime;
    commentState CommentState;
    check maxFivePendingComments;
}

resource /persons {
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

resource /person/#PersonId {
    get {
        select p.* from Person as p where p.id = $1;
    }
    put {
        replace Person identified by $1;
    }
    delete {
        delete from Person as p where p.id = $1;
    }
}

resource /blogposts {
    get {
        public;
        select bp.id, bp.*, p.name as authorName 
            from BlogPost as bp
            inner join Person as p on p.id = bp.authorId
            order by bp.name
            limit 1000;
        default-filter-sort;
    }
    post {
        insert BlogPost;
    }
}

resource /blogposts/#BlogPostId {
    get {
        public;
        select bp.*, p.name as authorName from BlogPost as bp 
            inner join Person as p on p.id = bp.authorId
            where bp.id = $1;
    }
    put {
        replace BlogPost identified by $1;
    }
    delete {
        delete from BlogPost as bp where bp.id = $1;
    }
}

resource /comments {
    get {
        public;
        select bp.id, bp.* from BlogPost as bp
            order by bp.name
            limit 1000;
        default-filter-sort;
    }
    post {
        insert BlogPost;
    }
}

resource /comments/#CommentId {
    get {
        public;
        select c.* from Comment as c where c.id = $1;
    }
    put {
        replace Comment identified by $1;
    }
    delete {
        delete from Comment as c where c.id = $1;
    }
}
```

## Defining database entities

As shown above, Persistent entities are defined in *entity {}* blocks. The
entity {}-block has the following structure where brackets denote an optional
value and "..." mean repeated syntactic element: 

'''
entity EntityName {
    instance of Class1, ..., ClassN;

    field1Name [Maybe] Field1Type [default defaultValue] [check functionName];
    ...
    fieldNName [Maybe] FieldNType [default defaultValue] [check functionName];
    

    unique Unique1Name unique1field1 ... unique1fieldN;
    ...
    unique UniqueNName uniqueNfield1 ... uniqueNfieldN;

    deriving Class1;
    ...
    deriving ClassN;

    check entityCheck1FunctionName;
    check ...
    check entityCheckNFunctionName;
}
'''

An entity can be an instance of a class defined before or after the entity in the DSL files.

Field names must begin with a lower-case letter. Single-quotes can be used to
avoid clashes with reserved words of the DSL.

Built-in values for FieldType are Word32, Word64, Int32, Int64, Text, Bool,
Double, TimeOfDay, Day, UTCTime, ZonedTime. If the FieldType ends in "Id",
then the prefix must be a valid entity or class name.

A field with an "entity class" type is replaced with a number of fields
referencing the Id field in each entity implementing the class. For this
reason, such a field must have Maybe-qualified.
 
The default value is passed to Persistent as such and taken into account in the
database server. Examples of valid values are: "string", 1.3, and 4.

Defining a field check-function adds a function to the type class
ValidationFunctions which must be implemented by the Yesod master site using
the generated subsite. Field check function is executed before modifying
entities in the databas. If the check function returns False, the transaction
is aborted and an error message is sent to the callee.

Unique-statement names are prefixed with "Unique" in the resulting Persistent
models-definition and !force-flag is added to allow using Maybe fields in
unique definitions (we assume you know what you're doing).

Deriving-statement can be used to tell Persistent to derive instances for
built-in type classes, such as Typeable (required by Yesod.Auth's User entity).

Entity-wise check-functions are similar to field check-functions but take the
entity as a parameter instead of a single field.

## Enums

## Field and entity validation checks

## Unique definitions

## Entity classes

## Splitting definitions to multiple files





