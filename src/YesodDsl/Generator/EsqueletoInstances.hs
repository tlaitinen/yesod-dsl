{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module YesodDsl.Generator.EsqueletoInstances (esqueletoInstances) where
import YesodDsl.AST
import YesodDsl.Generator.Common
import qualified Data.Text as T
import Text.Shakespeare.Text hiding (toText)
import Data.List
import YesodDsl.Generator.Esqueleto
import Control.Monad.State

maxInstances :: Module -> Int
maxInstances m = maximum $ map sqFieldNumber
                      $ filter isSelectQuery [ hp | r <- modRoutes m, 
                                               h <- routeHandlers r,
                                               hp <- handlerParams h]
    where isSelectQuery (Select _) = True
          isSelectQuery _ = False
          sqFieldNumber (Select sq) = let
              ctx = (emptyContext m) {
                  ctxNames = sqAliases sq
              }
              in evalState ((liftM concat $ mapM selectFieldExprs (sqFields sq))
                        >>= \fes -> return $ length fes) ctx
          sqFieldNumber _ = 0

genInstance :: Int -> String
genInstance fnum = T.unpack $(codegenFile "codegen/sqlselect-instance.cg")
    where nums = [1..fnum]
          ifield n = "i" ++ show n
          ofield n = "o" ++ show n
          sqlSelectTypeLhs n = "SqlSelect " ++ ifield n ++ " " ++ ofield n
          sqlSelectCols n = "sqlSelectCols esc " ++ ifield n
          pairs xs = intercalate ", " $ pairs' xs
          pairs' :: [String] -> [String]
          pairs' (x1:x2:xs) = ("(" ++ x1 ++ "," ++ x2 ++ ")"):pairs' xs
          pairs' (x:_) = [x]                                       
          pairs' _ = []
          

esqueletoInstances :: Module -> String
esqueletoInstances m = concatMap genInstance [17.. (maxInstances m)]


