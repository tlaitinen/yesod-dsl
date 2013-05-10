module Validation where

import AST
import Data.List
import ValidationNames

validate :: Module -> String
validate = duplicateNameErrors . findNames
