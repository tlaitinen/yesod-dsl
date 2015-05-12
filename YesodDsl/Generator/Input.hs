{-# LANGUAGE TemplateHaskell #-}
module YesodDsl.Generator.Input where
import Data.Maybe
import qualified Data.Text as T
import Text.Shakespeare.Text hiding (toText)
import Data.String.Utils (rstrip)
import YesodDsl.AST
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map
inputFieldRef :: FieldRef -> String
inputFieldRef AuthId = rstrip $ T.unpack $(codegenFile "codegen/input-field-authid.cg")
inputFieldRef (AuthField fn) = rstrip $ T.unpack $(codegenFile "codegen/input-field-auth.cg")
inputFieldRef (NamedLocalParam vn) = rstrip $ T.unpack $(codegenFile "codegen/map-input-field-localparam.cg")

inputFieldRef (LocalParamField (Var vn (Right e) _) fn) = let en = entityName e in
    rstrip $ T.unpack $(codegenFile "codegen/input-field-local-param-field.cg")
           
inputFieldRef (PathParam i) = T.unpack $(codegenFile "codegen/input-field-path-param.cg")
inputFieldRef (RequestField pn) = rstrip $ T.unpack $(codegenFile "codegen/input-field-normal.cg")
inputFieldRef ifr = show ifr

getJsonAttrs :: Stmt -> [FieldName]
getJsonAttrs (Insert (Right e) Nothing _) = [ fieldName f | f <- entityFields e, isNothing $ fieldDefault f, fieldOptional f == False ]
getJsonAttrs hp = [ fn | RequestField fn <- universeBi hp ]
                ++ (concat [ [ fieldName f | f <- entityFields e, isNothing $ fieldDefault f, fieldOptional f == False ]
                    | Insert (Right e) Nothing _ <- universeBi hp ])

getParamDefaults :: [Stmt] -> Map.Map ParamName FieldValue
getParamDefaults ps = Map.fromList [ (pn,fv) | ParamDefault pn fv <- universeBi ps ]
