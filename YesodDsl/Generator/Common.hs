module YesodDsl.Generator.Common where
import YesodDsl.AST
import Language.Haskell.HsColour.Classify

brackets :: Bool -> String -> String
brackets True s = "(" ++ s ++ ")"
brackets False s = s

maybeHsJust :: Bool -> String -> String
maybeHsJust True v = "(Just " ++ v ++ ")"
maybeHsJust False v = v

makeJust :: Int -> String -> String
makeJust n t 
    | n > 0 = "just (" ++ makeJust (n - 1) t ++ ")"
    | otherwise = t

quote :: String -> String
quote s = "\"" ++ s ++ "\""
indent :: Int -> String -> String
indent x = unlines . (map ((replicate x ' ')++)) . lines

prepend :: String -> String -> String
prepend x = (x++)

entityFieldName :: Entity -> Field -> String
entityFieldName e f = (lowerFirst . entityName) e ++ (upperFirst . fieldName) f

resultMapper :: Maybe FunctionName -> String
resultMapper mmapper = maybe "" ((" $ " ++) . (++ " $ ")) mmapper

isKeyword :: String -> Bool
isKeyword = (== [Keyword]) . map fst . tokenise

safeHsName :: String -> String
safeHsName x
    | isKeyword x = x ++ "_"
    | otherwise = x


