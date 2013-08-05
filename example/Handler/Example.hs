{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Handler.Example 
    ( module Handler.Example.Enums, module Handler.Example.Internal ) where
import Handler.Example.Enums
import Handler.Example.Internal
import Yesod.Core
import Yesod.Auth
import Yesod.Persist
import Database.Esqueleto
import Prelude
type ExampleRoute = Route Example
 
instance (YesodAuthPersist master,
          ExampleValidation master,
          KeyEntity (AuthId master) ~ User,
          YesodPersistBackend master ~ SqlPersistT) => YesodSubDispatch Example (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesExample)
