{-# LANGUAGE TemplateHaskell #-}
module YesodDsl.Generator.Input where
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import Data.String.Utils (rstrip)
import Control.Monad.State
import YesodDsl.AST
import YesodDsl.Generator.Esqueleto
import YesodDsl.Generator.Common
import YesodDsl.Generator.Models
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map
inputFieldRef :: FieldRef -> State Context String
inputFieldRef AuthId = return $ rstrip $ T.unpack $(codegenFile "codegen/input-field-authid.cg")
inputFieldRef (AuthField fn) = return $ rstrip $ T.unpack $(codegenFile "codegen/input-field-auth.cg")
inputFieldRef (NamedLocalParam vn) = return $ rstrip $ T.unpack $(codegenFile "codegen/map-input-field-localparam.cg")

inputFieldRef (LocalParamField (Var vn (Right e) _) fn) = do
    let en = entityName e
    return $ rstrip $ T.unpack $(codegenFile "codegen/input-field-local-param-field.cg")
           
inputFieldRef (PathParam i) = return $ T.unpack $(codegenFile "codegen/input-field-path-param.cg")
inputFieldRef (RequestField pn) = return $ rstrip $ T.unpack $(codegenFile "codegen/input-field-normal.cg")
inputFieldRef ifr = return $ show ifr

getJsonAttrs :: Stmt -> [FieldName]
getJsonAttrs (Insert (Right e) Nothing _) = [ fieldName f | f <- entityFields e, isNothing $ fieldDefault f, fieldOptional f == False ]
getJsonAttrs hp = [ fn | RequestField fn <- universeBi hp ]
                ++ (concat [ [ fieldName f | f <- entityFields e, isNothing $ fieldDefault f, fieldOptional f == False ]
                    | Insert (Right e) Nothing _ <- universeBi hp ])

getParamDefaults :: [Stmt] -> Map.Map ParamName FieldValue
getParamDefaults ps = Map.fromList [ (pn,fv) | ParamDefault pn fv <- universeBi ps ]
