# yesod-generate-rest

A domain specific language and a code generator desined to create RESTful
services for managing a database with [Yesod web framework](http://www.yesodweb.com/)
and [Persistent](http://www.yesodweb.com/book/persistent).

## Features (parentheses if not yet implemented)
 * splitting database definitions into multiple files
 * generates support code for implementing polymorphic relations and accessing common fields
 * generates boilerplate code for entity validation
 * supports following field types : Word32, Word64, Int32, Int64, Text, Bool, Double, TimeOfDay, Day, UTCTime, ZonedTime
 * (generates RESTful web service for managing entities)
 * (triggers for web service events: beforeCreate, afterCreate, beforeModify, afterModify, etc.)

## License
 * The code generator is distributed under the terms of [Simplified BSD license](LICENSE)

## Quick start

### Step 1: get the source code and compile

    git clone git://github.com/tlaitinen/yesod-generate-rest.git
    cd yesod-generate-rest
    make

### Step 2: Create scaffolded Yesod site

    yesod init

### Step 3: write database definition .dbdef-file
```
-- .dbdef-file can include other .dbdef-files for increased 
-- reusability
import "module.dbdef";

-- class defines a set of fields that can be inherited by an entity
class Named {
    -- a field can have a number of validation functions 
    -- code generator assumes that nonempty :: Text -> Bool 
    -- is found at Model/ValidationFunctions.hs
    name Text check nonempty;
}

class Versioned {
    version Maybe Int64; 
}

-- User-entity is an instance of the classes Named and Versioned
entity User : Named, Versioned {
    password Text;
    salt Text;
    language Text;
    timezone Text;
}

entity Note : Named, Versioned {
    owner   User;
    body    Text;
    created DateTime;
}

entity File : Named {
    owner User;
    path Text;

    unique OwnerName owner name;
}

entity ChangeRecord {
    field    Text;
    oldValue Text;
    newValue Text;
    time     DateTime;
    version  Int64;
    -- a polymorphic relation which will be expanded to a number of fields
    -- pointing a each possible entity that is an instance of Versioned
    'entity' Maybe Versioned;
}
```

### Step 4: run code generator

    $ yesod-generate-rest main.dbdef

At the moment, the code generator writes config/generated-models, Model/Validation.hs,
   and Model/Classes.hs that have the following contents.


#### config/generated-models
```
ChangeRecord
    entityNote NoteId Maybe 
    entityUser UserId Maybe 
    version Int64 
    time UTCTime 
    newValue Text 
    oldValue Text 
    field Text 

File
    path Text 
    owner UserId 
    name Text 
    UniqueOwnerName owner name

Note
    created UTCTime 
    body Text 
    owner UserId 
    version Int64 Maybe 
    name Text 

User
    timezone Text 
    language Text 
    salt Text 
    password Text 
    version Int64 Maybe 
    name Text 
```

#### Model/Validation.hs
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Model.Validation (Validatable(..)) where
import Data.Text
import qualified Model.ValidationFunctions as V
import Import
class Validatable a where
    validate :: a -> [Text]
instance Validatable ChangeRecord where 
    validate d = catMaybes [
    ]
    
instance Validatable File where 
    validate d = catMaybes [
    if (not . V.nonempty) $name d then Just "File.name nonempty" else Nothing
    ]
    
instance Validatable Note where 
    validate d = catMaybes [
    if (not . V.nonempty) $name d then Just "Note.name nonempty" else Nothing
    ]
    
instance Validatable User where 
    validate d = catMaybes [
    if (not . V.nonempty) $name d then Just "User.name nonempty" else Nothing
    ]
```    

#### Model/Classes.hs
```haskell
module Model.Classes where
import Import
class Versioned a where
    versionedVersion :: a -> Maybe Int64

instance Versioned Note where 
    versionedVersion = noteVersion

instance Versioned User where 
    versionedVersion = userVersion

class Named a where
    namedName :: a -> Text

instance Named File where 
    namedName = fileName

instance Named Note where 
    namedName = noteName

instance Named User where 
    namedName = userName
```
