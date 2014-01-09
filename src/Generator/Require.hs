{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Generator.Require where
import AST
import Text.Shakespeare.Text hiding (toText)
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Data.Char
import Generator.Common
import Generator.Esqueleto
import Generator.Models
requireStmts :: Module -> [HandlerParam] -> String
requireStmts m ps = concatMap f $ zip [1..] ps
    where f (requireId,(Require sq)) = T.unpack $(codegenFile "codegen/require-select-query.cg")
            where 
                (limit, offset) = sqLimitOffset sq
                ctx = Context {
                      ctxNames = sqAliases sq,
                      ctxModule = m,
                      ctxHandlerParams = []
                }
                (selectEntity, selectVar) = sqFrom sq
                maybeWhere = case sqWhere sq of
                    Just expr -> T.unpack $(codegenFile "codegen/where-expr.cg")
                    Nothing -> ""
          f _ = ""

