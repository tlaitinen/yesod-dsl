module Result (Result,
               emptyResult,
               addToResult, 
               getResult) where

import qualified Data.Map as Map
import Control.Monad.Writer

type FileName = String
type Result = [(FileName, String)]

emptyResult :: Result
emptyResult = []

addToResult :: FileName -> String -> Writer Result ()
addToResult f c = tell [(f,c)]

getResult :: Result -> [(FileName, String)]
getResult = Map.toList . (Map.fromListWith f)
    where f l r = l ++ r
