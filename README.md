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

The syntax for the input of yesod-dsl is illustrated by the following example: 

```
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

As shown above, Persistent entities are defined in 
'''entity {}''' blocks.


## Enums

## Field and entity validation checks

## Unique definitions

## Entity classes

## Splitting definitions to multiple files





