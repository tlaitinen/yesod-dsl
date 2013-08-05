{-# LANGUAGE TemplateHaskell #-}
module Handler.Example.Enums where
import Database.Persist.TH
import qualified Data.Aeson as A
import Prelude
import Web.PathPieces
import Control.Monad (mzero)
data CommentState = CommentStateSpam | CommentStateAccepted | CommentStatePending deriving (Eq)

instance Read CommentState where
    readsPrec _ ('S':'p':'a':'m':xs) = [ (CommentStateSpam, xs) ]
    readsPrec _ ('A':'c':'c':'e':'p':'t':'e':'d':xs) = [ (CommentStateAccepted, xs) ]
    readsPrec _ ('P':'e':'n':'d':'i':'n':'g':xs) = [ (CommentStatePending, xs) ]
    readsPrec _ _ = [ ]

instance Show CommentState where
    show CommentStateSpam = "Spam"
    show CommentStateAccepted = "Accepted"
    show CommentStatePending = "Pending"

    
derivePersistField "CommentState"

instance A.FromJSON CommentState where
    parseJSON = A.withText "CommentState" (\v -> case v of
        "Spam" -> return CommentStateSpam
        "Accepted" -> return CommentStateAccepted
        "Pending" -> return CommentStatePending
        _ -> mzero)

instance A.ToJSON CommentState where
    toJSON CommentStateSpam = A.String "Spam"
    toJSON CommentStateAccepted = A.String "Accepted"
    toJSON CommentStatePending = A.String "Pending"


instance PathPiece CommentState where
    fromPathPiece "Spam" = Just CommentStateSpam
    fromPathPiece "Accepted" = Just CommentStateAccepted
    fromPathPiece "Pending" = Just CommentStatePending

    fromPathPiece _ = Nothing
    toPathPiece CommentStateSpam = "Spam"
    toPathPiece CommentStateAccepted = "Accepted"
    toPathPiece CommentStatePending = "Pending"



