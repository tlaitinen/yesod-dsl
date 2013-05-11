module Validation where

import AST
import Data.List
import ValidationNames
import ValidationRefs

validate :: Module -> String
validate m = nameErrors (names m) ++ refErrors (refTargets m) (refs m)
