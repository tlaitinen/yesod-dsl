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

### Step 1: get the source code and compile

    git clone git://github.com/tlaitinen/enterdsl.git
    cd enterdsl/database
    make

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

    -- 'interface' keyword is used to define an access pattern 
    -- to multiple documents. Each document that 'implements' the
    -- interface inherits all the fields defined in the interface.
    interface Named {
        -- this is an interface for all documents that are guaranteed
        -- to have a name. 
        
        -- 'check' keyword is used to define custom
        -- field validation checks. In this case, the function 'nonEmpty'
        -- can return an error message if the supplied value is an empty string.
        name Text check nonEmpty;
    }

    interface Playable {
        -- this is an interface for all documents that can be "played"

        -- no fields here, just for referencing
    }

    document AudioFile {
        -- A document can implement a number of interfaces.
        -- 'implements' keywords must be placed in the beginning of the
        -- document declaration.

        implements Named;
        implements Playable;

        path Text;

        -- AudioFile embeds another document 'File'
        file File;
    }


    document File {
        implements Named;
        size Word64;
        chunks [ref Chunk];
    }

    document Chunk {
        num Word64;
        bytes Text;
    }

    document HTTPStream {
        implements Named;
        implements Playable;

        url Text;
    }

    document PlayList {
        implements Named;

        -- PlayList is a list of Playables (audio files and HTTP streams).
        playables [ref Playable];

        -- Note that PlayList cannot implement the interface 'Playable'
        -- because that would cause module import cycle.
    }

### Step 4: run code generator

    $ dbdef-gen main.dbdef
    Updating    Model/Common.hs
    Updating    Model/PlayList.hs
    Updating    Model/HTTPStream.hs
    Updating    Model/Chunk.hs
    Updating    Model/File.hs
    Updating    Model/AudioFile.hs
    Updating    Model/Message.hs
    Updating    Model/User.hs
    Updating    Model/Playable.hs
    Updating    Model/PlayableInst.hs
    Updating    Model/PlayableInstRef.hs
    Updating    Model/Named.hs
    Updating    Model/NamedInst.hs
    Updating    Model/NamedInstRef.hs

The generator writes Haskell modules for each document and each interface.

For example, Model/File.hs is as follows:
```haskell
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module Model.File where 
import Database.Persist
import Database.Persist.MongoDB
import Database.Persist.TH
import Language.Haskell.TH.Syntax
import qualified Model.Validation as V
import Model.Common
import Model.Chunk (Chunk, ChunkGeneric, ChunkId)
import qualified Model.Named as Named
share [mkPersist MkPersistSettings { mpsBackend = ConT ''Action }] [persist|
File
    chunks [ChunkId] 
    size Word64 
    name Text 
|]
chunks = fileChunks
size = fileSize
name = fileName
s_chunks ::  [ChunkId] -> File -> Either String File
s_chunks v d 
    | otherwise = Right $ d { fileChunks = v } 

s_size ::  Word64 -> File -> Either String File
s_size v d 
    | otherwise = Right $ d { fileSize = v } 

s_name ::  Text -> File -> Either String File
s_name v d 
    | isJust $ V.nonEmpty v = Left $ fromJust $ V.nonEmpty v
    | otherwise = Right $ d { fileName = v } 

instance Named.Named File where
    name = Model.File.name
```        
