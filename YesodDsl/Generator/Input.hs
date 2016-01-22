{-# LANGUAGE TemplateHaskell #-}
module YesodDsl.Generator.Input where
import Data.Maybe
import qualified Data.Text as T
import Text.Shakespeare.Text hiding (toText)
import qualified Data.List as L
import Data.Function (on)
import Data.String.Utils (rstrip)
import YesodDsl.AST
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map
import YesodDsl.Generator.Common

fieldRefMappingToAttrs :: Entity -> Bool -> [FieldRefMapping] -> [(FieldName, Maybe Field)]
fieldRefMappingToAttrs e onlyMapped fs = (if onlyMapped then [] else [ (fieldName f, Just f) | f <- entityFields e, isNothing $ fieldDefault f, fieldOptional f == False, fieldInternal f == False, fieldName f `notElem` mapped ]) ++ [ (pn, Just f) | f <- entityFields e, (fn,fr,_) <- fs, (RequestField pn) <- universeBi fr, fieldName f == fn ]
    where
        mapped = [ fn | (fn, _, _) <- fs ]

requestAttrs :: Stmt -> [(FieldName, Maybe Field)]
requestAttrs (Select sq) = [    
        ("start", Just $ mkField "start" (True, NormalField FTInt32)),
        ("limit", Just $ mkField "limit" (True, NormalField FTInt32))
    ]
requestAttrs (Update (Right e) _ Nothing) = [ (fieldJsonName f, Just f) | f <- entityFields e, fieldInternal f == False, fieldReadOnly f == False ]
requestAttrs (Update (Right e) _ (Just fs)) = fieldRefMappingToAttrs e False fs
requestAttrs (Insert (Right e) Nothing _) = [ (fieldJsonName f, Just f) | f <- entityFields e, fieldInternal f == False, fieldReadOnly f == False ]
requestAttrs (Insert (Right e) (Just (me, fs)) _) = fieldRefMappingToAttrs e (isJust me) fs
requestAttrs hp = [ (fn, Nothing) | RequestField fn <- universeBi hp ] ++ (concat $ [ requestAttrs i | i@(Insert _ _ _) <- universeBi hp ] ++ [ requestAttrs u | u@(Update _ _ _) <- universeBi hp ])
        ++ (concat [ 
                    [ (fieldJsonName f, Just f) | f <- entityFields e, isNothing $ fieldDefault f, fieldOptional f == False ]
                    | Insert (Right e) Nothing _ <- universeBi hp ])

nubAttrs :: [(FieldName, Maybe Field)] -> [(FieldName, Maybe Field)]
nubAttrs  = L.nubBy ((==) `on` fst) 



inputFieldRef :: FieldRef -> String
inputFieldRef AuthId = rstrip $ T.unpack $(codegenFile "codegen/input-field-authid.cg")
inputFieldRef (AuthField fn) = rstrip $ T.unpack $(codegenFile "codegen/input-field-auth.cg")
inputFieldRef (NamedLocalParam vn) = rstrip $ T.unpack $(codegenFile "codegen/map-input-field-localparam.cg")

inputFieldRef (LocalParamField (Var vn (Right e) _) fn) = let en = entityName e in
    rstrip $ T.unpack $(codegenFile "codegen/input-field-local-param-field.cg")
           
inputFieldRef (PathParam i) = T.unpack $(codegenFile "codegen/input-field-path-param.cg")
inputFieldRef (RequestField pn) = rstrip $ T.unpack $(codegenFile "codegen/input-field-normal.cg")
inputFieldRef (Const fv) = fieldValueToHs fv
inputFieldRef ifr = show ifr

getJsonAttrs :: Stmt -> [FieldName]
getJsonAttrs (Insert (Right e) Nothing _) = [ fieldJsonName f | f <- entityFields e, fieldInternal f == False, fieldReadOnly f == False ]
getJsonAttrs (Update (Right e) fr Nothing) = [ fn | RequestField fn <- universeBi fr ] ++ [ fieldName f | f <- entityFields e ]
getJsonAttrs hp = [ fn | RequestField fn <- universeBi hp ]
                ++ (concat [ [ fieldJsonName f | f <- entityFields e, isNothing $ fieldDefault f, fieldOptional f == False ]
                    | Insert (Right e) Nothing _ <- universeBi hp ])

getParamDefaults :: [Stmt] -> Map.Map ParamName FieldValue
getParamDefaults ps = Map.fromList [ (pn,fv) | ParamDefault pn fv <- universeBi ps ]
