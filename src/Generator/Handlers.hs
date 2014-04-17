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
import qualified Data.Map as Map

import Generator.GetHandler
import Generator.UpdateHandlers
import Generator.Routes
import Control.Monad.State
import Generator.Esqueleto

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

handler :: Handler -> State Context String
handler (Handler _ ht ps) = do
    ctx <- get 
    put $ ctx { ctxHandlerParams = ps, ctxTypes = Map.empty }
    m <- gets ctxModule
    r <- gets ctxRoute >>= return . fromJust
 
    result <- liftM concat $ sequence [
            return $ T.unpack $(codegenFile "codegen/handler-header.cg"),
            return $ (if Public `elem` ps 
                    then "" 
                    else (T.unpack $(codegenFile "codegen/handler-requireauth.cg"))),
            case ht of
                    GetHandler -> getHandler
                    PutHandler -> updateHandler
                    PostHandler -> updateHandler
                    DeleteHandler -> updateHandler
        ]
    return result 


