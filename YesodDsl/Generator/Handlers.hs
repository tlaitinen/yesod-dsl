{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module YesodDsl.Generator.Handlers where
import YesodDsl.AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import Data.String.Utils (rstrip)
import qualified Data.Map as Map

import YesodDsl.Generator.GetHandler
import YesodDsl.Generator.UpdateHandlers
import YesodDsl.Generator.Routes
import Control.Monad.Reader
import YesodDsl.Generator.Esqueleto

hsRouteParams :: [PathPiece] -> String
hsRouteParams ps = intercalate " " [("p" ++ show x) | 
                                    x <- [1..length (filter hasType ps)]]
    where hasType (PathId _ _) = True
          hasType _ = False

hsHandlerMethod :: HandlerType -> String          
hsHandlerMethod GetHandler    = "get"
hsHandlerMethod PutHandler    = "put"
hsHandlerMethod PostHandler   = "post"
hsHandlerMethod DeleteHandler = "delete"

handler :: Module -> Route -> Handler -> Reader Context String
handler m r (Handler _ ht ps) = do
    result <- liftM concat $ sequence [
            return $ T.unpack $(codegenFile "codegen/handler-header.cg"),
            return $ (if Public `elem` ps 
                    then "" 
                    else (T.unpack $(codegenFile "codegen/handler-requireauth.cg"))),
            case ht of
                    GetHandler -> getHandler ps
                    _ -> updateHandler ps
        ]
    return result 


