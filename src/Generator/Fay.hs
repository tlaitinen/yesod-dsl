{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Generator.Fay (fay) where
import AST
import Text.Shakespeare.Text hiding (toText)
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Data.Char
import Generator.Common


trPathPiece :: PathPiece -> String
trPathPiece pp = case pp of
    PathText pt -> "PathText " ++ show pt
    PathId pi   -> "PathId " ++ show pi

trHandlerType :: HandlerType -> String
trHandlerType ht = case ht of
    PutHandler -> "PutHandler"
    GetHandler -> "GetHandler"
    PostHandler -> "PostHandler"
    DeleteHandler -> "DeleteHandler"

trHandler :: Handler -> String
trHandler h = T.unpack $(codegenFile "codegen/fay-handler.cg")

trField :: Field -> String
trField f = T.unpack $(codegenFile "codegen/fay-field.cg")

trEntity :: Entity -> String
trEntity e = T.unpack $(codegenFile "codegen/fay-entity.cg")

trClass :: Class -> String
trClass c = T.unpack $(codegenFile "codegen/fay-class.cg")

trEnum  :: EnumType -> String
trEnum e = T.unpack $(codegenFile "codegen/fay-enum.cg")

trRoute:: Route -> String
trRoute r = T.unpack $(codegenFile "codegen/fay-route.cg")

fay :: Module -> String
fay m = T.unpack $(codegenFile "codegen/fay-header.cg")
    


