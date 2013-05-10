module GenMonad where

import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Writer

type GenMonad env err doc r = ReaderT env (ErrorT err (WriterT doc IO)) r
