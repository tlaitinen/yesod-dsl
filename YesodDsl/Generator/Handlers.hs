{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module YesodDsl.Generator.Handlers where
import YesodDsl.AST
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)

import YesodDsl.Generator.GetHandler
import YesodDsl.Generator.UpdateHandlers
import YesodDsl.Generator.Routes

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

handler :: Module -> Route -> Handler ->String
handler m r (Handler _ ht ps) = concat $ [
        T.unpack $(codegenFile "codegen/handler-header.cg"),
        if Public `elem` ps 
                then "" 
                else (T.unpack $(codegenFile "codegen/handler-requireauth.cg")),
        case ht of
            GetHandler -> getHandler ps
            _ -> updateHandler ps
    ]


