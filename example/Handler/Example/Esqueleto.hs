{-# LANGUAGE ConstraintKinds
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , MultiParamTypeClasses
           , OverloadedStrings
           , UndecidableInstances
 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}           
module Handler.Example.Esqueleto where
import Prelude
import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Arrow ((***), first)
import Control.Exception (throw, throwIO)
import Control.Monad ((>=>), ap, void, MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResourceBase)
import Data.Int (Int64)
import Data.List (intersperse)
import Data.Monoid (Monoid(..), (<>))
import Data.Proxy (Proxy(..))
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Trans.Writer as W
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLBI

import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql

uncommas :: [TLB.Builder] -> TLB.Builder
uncommas = mconcat . intersperse ", " . filter (/= mempty)

uncommas' :: Monoid a => [(TLB.Builder, a)] -> (TLB.Builder, a)
uncommas' = (uncommas *** mconcat) . unzip


