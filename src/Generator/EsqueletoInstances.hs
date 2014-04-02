{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.EsqueletoInstances (esqueletoInstances) where
import AST
import Generator.Common
import qualified Data.Text as T
import Text.Shakespeare.Text hiding (toText)
import Data.List
import Generator.Esqueleto

maxInstances :: Module -> Int
maxInstances m = maximum $ map sqFieldNumber
                      $ filter isSelectQuery [ hp | r <- modRoutes m, 
                                               h <- routeHandlers r,
                                               hp <- handlerParams h]
    where isSelectQuery (Select _) = True
          isSelectQuery _ = False
          sqFieldNumber (Select sq) = let
              ctx = Context {
                  ctxNames = sqAliases sq,
                  ctxModule = m,
                  ctxHandlerParams = []
              }
              in length $ concatMap (selectFieldExprs m ctx) (sqFields sq)

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


