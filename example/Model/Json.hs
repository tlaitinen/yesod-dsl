{-# LANGUAGE FlexibleInstances #-}
module Model.Json where
import Import
import Data.Aeson
import qualified Data.HashMap.Lazy as HML
instance ToJSON (Entity ChangeRecord) where
    toJSON (Entity k v) = case toJSON v of
        Object o -> Object $ HML.insert "id" (toJSON k) o
        _ -> error "unexpected JS encode error"
instance ToJSON (Entity Note) where
    toJSON (Entity k v) = case toJSON v of
        Object o -> Object $ HML.insert "id" (toJSON k) o
        _ -> error "unexpected JS encode error"
instance ToJSON (Entity Person) where
    toJSON (Entity k v) = case toJSON v of
        Object o -> Object $ HML.insert "id" (toJSON k) o
        _ -> error "unexpected JS encode error"
