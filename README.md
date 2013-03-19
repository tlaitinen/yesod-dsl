# enterdsl 

A domain specific language and a code generator desined to create RESTful
services for managing a database. 

The generated server-side Haskell code implements a web service that provides:
 * data versioning
 * data validation
 * per-user-limited-database-views
 * per-document and per-field user rights management

## Building blocks
 * Yesod web framework
 * A database backend supported by [Persistent](http://hackage.haskell.org/package/yesod-persistent)

## Status
 * work in progress

## License
 * This library is distributed under the terms of [Simplified BSD license](enterdsl/blob/master/LICENSE)

## Quick start

### Step 1: get the source code and compile

    git clone git://github.com/tlaitinen/enterdsl.git
    cd enterdsl/database
    make

### Step 2: Create scaffolded Yesod site

    yesod init

### Step 3: write database definition .dbdef-file
```
-- .dbdef-file can include other .dbdef-files for increased 
-- reusability
import "mymodule.dbdef"     


```

### Step 4: run code generator

    $ dbdef-gen main.dbdef


