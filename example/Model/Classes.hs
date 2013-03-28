module Model.Classes where
import Import
import Data.Int
import Data.Word
import Data.Time
class Versioned a where
    versionedVersion :: a -> Int64

instance Versioned Note where 
    versionedVersion = noteVersion

