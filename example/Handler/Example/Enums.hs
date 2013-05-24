{-# LANGUAGE TemplateHaskell #-}
module Handler.Example.Enums where
import Database.Persist.TH
import Data.Aeson.TH
import Prelude
data CommentState = SpamComment | AcceptedComment | PendingComment deriving (Show, Read, Eq)
derivePersistField "CommentState"
deriveJSON id ''CommentState
