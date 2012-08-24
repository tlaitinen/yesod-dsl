# enterdsl - Enterprise DSL

A domain specific language and a code generator desined to create RESTful
web-based administration systems for managing a MongoDB database. 

The generated server-side Haskell code implements a JSON web service that provides:
 * data versioning
 * data validation
 * per-user-limited-database-views
 * per-document and per-field user rights management
 * file storage and retrieval in GridFS

The generated client-side Javascript code implements a reactive user interface that provides:
 * three layer navigation structure (pages, sub-pages, tabs)
 * displaying only pages and fields that the user can see
 * displaying db-views in tables whose rows can be ordered, selected and searched using different filters
 * database-document editors with fast field validation
 * multiple file upload
 * internationalization support

## Building blocks
The generated code uses:
 * Yesod web framework
 * MongoDB database
 * EmberJS framework

## Status
 * work in progress
 * code generator writes Yesod-compatible MongoDB wrappers

## License
 * This library is distributed under the terms of [Simplified BSD license](enterdsl/blob/master/LICENSE)

## Quick start

### Step 1: get the source code 

    git clone git://github.com/tlaitinen/enterdsl.git

### Step 2: Create scaffolded Yesod site

    yesod init

### Step 3: write database definition .dbdef-file

    -- .dbdef-file can include other .dbdef-files for increased 
    -- reusability
    import "mymodule.dbdef"     

    -- 'document' keyword is used to define a data container
    document User {
        -- Field name in lower case comes first and after that field type
        -- which can be one of (Word32, Word64, Int32, Int64, Text, Bool,
        --                      Double, Date, Time, DateTime, ZonedTime)
        -- or another document.
        userName Text;
        password Text;

        -- Other documents can be embedded within a documents.
        -- Square brackets denote a list.
        messages [Message];

    }

    document Message {
        -- 'ref' keyword is used to indicate a reference to another document.
        -- Note that embedded documents cannot be referenced.
        from ref User;
        to ref User;
        subject Text;
        body Text;
        time DateTime;
    }

