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
```

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
s_chunks ::  [ChunkId] -> File -> File
s_chunks v d = d { fileChunks = v }

s_size ::  Word64 -> File -> File
s_size v d = d { fileSize = v }

s_name ::  Text -> File -> File
s_name v d = d { fileName = v }

instance Named.Named File where
    name = Model.File.name
    s_name = Model.File.s_name
instance Validatable File where 
    validate d = catMaybes [            if not $ V.nonEmpty $ name d then Just "File.name nonEmpty" else Nothing    ]
```        

References to instances of an interface are implemented using an auxiliary data type. For example, a reference to a document that implements the interface "Playable" is represented by PlayableInstRef:
```haskell
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module Model.PlayableInstRef where 
import Database.Persist
import Database.Persist.MongoDB
import Database.Persist.TH
import Language.Haskell.TH.Syntax
import qualified Model.Validation as V
import Model.Common
import qualified Model.Playable as Playable
import Model.HTTPStream (HTTPStreamId, HTTPStreamGeneric)
import Model.AudioFile (AudioFileId, AudioFileGeneric)
share [mkPersist MkPersistSettings { mpsBackend = ConT ''Action }] [persist|
PlayableInstRef
    hTTPStreamId HTTPStreamId Maybe 
    audioFileId AudioFileId Maybe 
|]
hTTPStreamId = playableInstRefHTTPStreamId
audioFileId = playableInstRefAudioFileId
s_hTTPStreamId ::  Maybe HTTPStreamId -> PlayableInstRef -> PlayableInstRef
s_hTTPStreamId v d = d { playableInstRefHTTPStreamId = v }

s_audioFileId ::  Maybe AudioFileId -> PlayableInstRef -> PlayableInstRef
s_audioFileId v d = d { playableInstRefAudioFileId = v }
```

Instances of an interface can also be embedded within another document. For example, an embedded instance of the "Named" interface is represented by NamedInst:
```haskell
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module Model.NamedInst where 
import Database.Persist
import Database.Persist.MongoDB
import Database.Persist.TH
import Language.Haskell.TH.Syntax
import qualified Model.Validation as V
import Model.Common
import qualified Model.Named as Named
import Model.PlayList (PlayList)
import Model.HTTPStream (HTTPStream)
import Model.File (File)
import Model.AudioFile (AudioFile)
import qualified Model.PlayList
import qualified Model.HTTPStream
import qualified Model.File
import qualified Model.AudioFile
share [mkPersist MkPersistSettings { mpsBackend = ConT ''Action }] [persist|
NamedInst
    playList PlayList Maybe 
    hTTPStream HTTPStream Maybe 
    file File Maybe 
    audioFile AudioFile Maybe 
|]
playList = namedInstPlayList
hTTPStream = namedInstHTTPStream
file = namedInstFile
audioFile = namedInstAudioFile
s_playList ::  Maybe PlayList -> NamedInst -> NamedInst
s_playList v d = d { namedInstPlayList = v }

s_hTTPStream ::  Maybe HTTPStream -> NamedInst -> NamedInst
s_hTTPStream v d = d { namedInstHTTPStream = v }

s_file ::  Maybe File -> NamedInst -> NamedInst
s_file v d = d { namedInstFile = v }

s_audioFile ::  Maybe AudioFile -> NamedInst -> NamedInst
s_audioFile v d = d { namedInstAudioFile = v }

instance Named.Named NamedInst where
    name d
        | isJust (playList d) = Model.PlayList.name$ fromJust (playList d)
        | isJust (hTTPStream d) = Model.HTTPStream.name$ fromJust (hTTPStream d)
        | isJust (file d) = Model.File.name$ fromJust (file d)
        | isJust (audioFile d) = Model.AudioFile.name$ fromJust (audioFile d)
        | otherwise = throw NoInstance
    s_name v d
        | isJust (playList d) = s_playList (Just $ Model.PlayList.s_name v $ fromJust (playList d)) d
        | isJust (hTTPStream d) = s_hTTPStream (Just $ Model.HTTPStream.s_name v $ fromJust (hTTPStream d)) d
        | isJust (file d) = s_file (Just $ Model.File.s_name v $ fromJust (file d)) d
        | isJust (audioFile d) = s_audioFile (Just $ Model.AudioFile.s_name v $ fromJust (audioFile d)) d
        | otherwise = throw NoInstance
instance Validatable NamedInst where
    validate d
        | isJust (playList d) =  validate $ fromJust (playList d)
        | isJust (hTTPStream d) =  validate $ fromJust (hTTPStream d)
        | isJust (file d) =  validate $ fromJust (file d)
        | isJust (audioFile d) =  validate $ fromJust (audioFile d)
        | otherwise = throw NoInstance
```
