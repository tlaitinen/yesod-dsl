{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator.Require where
import YesodDsl.AST
import Text.Shakespeare.Text hiding (toText)
import qualified Data.Text as T
import YesodDsl.Generator.Common
import YesodDsl.Generator.Esqueleto
import Control.Monad.Reader
requireStmts :: [Stmt] -> String
requireStmts ps = concatMap f $ zip ([1..] :: [Int]) ps
    where 
        f (requireId,(Require sq)) = runReader 
            (do
                mw <- case sqWhere sq of
                    Just expr -> do
                        e <- hsBoolExpr expr 
                        return $ "where_ (" ++ e ++ ")\n"
                    Nothing -> return ""
                jes <- liftM concat $ mapM mapJoinExpr (reverse $ sqJoins sq)

                return $ let maybeWhere = mw 
                             (selectEntity, selectVar) = sqFrom sq
                             joinExprs  = jes in 
                    T.unpack $(codegenFile "codegen/require-select-query.cg"))
            (emptyContext { ctxNames = sqAliases sq })
        f _ = "" 
