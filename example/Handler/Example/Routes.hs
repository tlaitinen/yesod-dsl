{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Handler.Example.Routes where
import Prelude (const)
import Handler.Example.Enums
import Handler.Example.Esqueleto
import Handler.Example.Internal
import Handler.Example.RouteUsers
import Handler.Example.RouteUsersUser
import Handler.Example.RouteBlogposts
import Handler.Example.RouteBlogpostsBlogPost
import Handler.Example.RouteComments
import Handler.Example.RouteCommentsComment

import Yesod.Auth (requireAuth, requireAuthId, YesodAuth, AuthId, YesodAuthPersist)
import Yesod.Core
import Yesod.Persist (runDB, YesodPersist, YesodPersistBackend)


getExample :: a -> Example
getExample = const Example

mkYesodSubData "Example" [parseRoutes|
/users        UsersR      GET
/users/#UserId        UsersUserIdR      PUT
/blogposts        BlogpostsR      GET POST
/blogposts/#BlogPostId        BlogpostsBlogPostIdR      PUT
/comments        CommentsR      GET POST
/comments/#CommentId        CommentsCommentIdR      GET PUT DELETE
|]

