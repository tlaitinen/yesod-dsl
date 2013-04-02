module Model.TimeJson where
import Data.Time
import Data.Aeson
import Prelude
import Control.Monad
instance ToJSON Day where
    toJSON = toJSON . show

instance FromJSON Day where
    parseJSON x = do
        s <- parseJSON x
        case reads s of
            (d, _):_ -> return d
            [] -> mzero 

