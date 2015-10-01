{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator.PureScript (moduleToPureScript) where
import YesodDsl.AST
import Data.List
import Data.String.Utils (rstrip)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Shakespeare.Text hiding (toText)
import YesodDsl.Generator.Common

  
pureScriptFieldType :: Field -> String
pureScriptFieldType f = (if fieldOptional f then "Maybe " else "") 
    ++ case fieldContent f of
        NormalField ft -> case ft of
            FTWord32 -> "Int"
            FTWord64 -> "Number"
            FTInt32 -> "Int"
            FTInt -> "Number"
            FTInt64 -> "Number"
            FTText -> "String"
            FTBool -> "Boolean"
            FTDouble -> "Number"
            FTRational -> "Number"
            FTTimeOfDay -> "String"     -- TODO
            FTDay -> "String"           -- TODO
            FTUTCTime -> "String"       -- TODO
            FTCheckmark -> "Boolean"
        EntityField en -> en ++ "Id"
        EnumField en -> en

moduleToPureScript :: Module -> String
moduleToPureScript m = T.unpack $(codegenFile "codegen/purescript.cg")
    where
        enum e = T.unpack $(codegenFile "codegen/purescript-enum.cg")
            where
                value v = enumName e ++ v
                showValue v = T.unpack $(codegenFile "codegen/purescript-enum-show.cg")
        entity e = T.unpack $(codegenFile "codegen/purescript-entity.cg")
            where
                field f  = rstrip $ T.unpack $(codegenFile "codegen/purescript-field.cg")

