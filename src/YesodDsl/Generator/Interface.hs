{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator.Interface where
import YesodDsl.AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import Data.String.Utils (rstrip)
import YesodDsl.Generator.Models
import YesodDsl.Generator.Common
import YesodDsl.Generator.Esqueleto

   

lookupFieldType :: Module -> EntityName -> FieldName -> String
lookupFieldType m en fn = hsFieldType (fromJust $ lookupField m en fn)

handlerCall :: (FunctionName, [TypeName]) -> String
handlerCall (fn,ptns) = T.unpack $(codegenFile "codegen/call-type-signature.cg")
    where paramTypes = concatMap (++" -> ") ptns

interface :: Module -> [Context] -> String
interface m ctxs= T.unpack $(codegenFile "codegen/interface-header.cg")
             ++ (concatMap handlerCall $ concatMap ctxCalls ctxs)


