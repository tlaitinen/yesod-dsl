{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Handlers where
import AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import Data.String.Utils (rstrip)

import Generator.GetHandler
import Generator.UpdateHandlers
import Generator.Routes
hsRouteParams :: [PathPiece] -> String
hsRouteParams ps = intercalate " " [("p" ++ show x) | 
                                    x <- [1..length (filter hasType ps)]]
    where hasType (PathId _) = True
          hasType _ = False

hsHandlerMethod :: HandlerType -> String          
hsHandlerMethod GetHandler    = "get"
hsHandlerMethod PutHandler    = "put"
hsHandlerMethod PostHandler   = "post"
hsHandlerMethod DeleteHandler = "delete"

handler :: Module -> Resource -> Handler -> String
handler m r (Handler ht ps) = T.unpack $(codegenFile "codegen/handler-header.cg")
    ++ if Public `elem` ps 
            then "" 
            else (T.unpack $(codegenFile "codegen/handler-requireauth.cg"))
    ++ (case ht of
            GetHandler -> getHandler m r ps
            PutHandler -> updateHandler m r ps
            PostHandler -> updateHandler m r ps
            DeleteHandler -> deleteHandler m r ps)


