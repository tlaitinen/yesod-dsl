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
inputFieldRef FieldRefAuthId = return $ rstrip $ T.unpack $(codegenFile "codegen/input-field-authid.cg")
inputFieldRef (FieldRefAuth fn) = return $ rstrip $ T.unpack $(codegenFile "codegen/input-field-auth.cg")
inputFieldRef (FieldRefNamedLocalParam vn) = return $ rstrip $ T.unpack $(codegenFile "codegen/map-input-field-localparam.cg")
 
inputFieldRef (FieldRefLocalParamField vn fn) = do
    ps <- gets ctxHandlerParams
    let en = fromJust $ listToMaybe $ concatMap f ps
    return $ rstrip $ T.unpack $(codegenFile "codegen/input-field-local-param-field.cg")
    where 
          f (GetById er _ vn') = if vn' == vn then [entityRefName er] else []
          f _ = []
            
inputFieldRef (FieldRefPathParam i) = return $ T.unpack $(codegenFile "codegen/input-field-path-param.cg")
inputFieldRef (FieldRefRequest pn) = return $ rstrip $ T.unpack $(codegenFile "codegen/input-field-normal.cg")
inputFieldRef ifr = return $ show ifr

getJsonAttrs :: HandlerParam -> [FieldName]
getJsonAttrs (Insert (Right e) Nothing _) = [ fieldName f | f <- entityFields e, isNothing $ fieldDefault f, fieldOptional f == False ]
getJsonAttrs hp = [ fn | FieldRefRequest fn <- universeBi hp ]
                ++ (concat [ [ fieldName f | f <- entityFields e, isNothing $ fieldDefault f, fieldOptional f == False ]
                    | Insert (Right e) Nothing _ <- universeBi hp ])

getParamDefaults :: [HandlerParam] -> Map.Map ParamName FieldValue
getParamDefaults ps = Map.fromList [ (pn,fv) | ParamDefault pn fv <- universeBi ps ]
