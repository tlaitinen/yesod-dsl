module Generator.Common where
import AST
maybeHsJust :: Bool -> String -> String
maybeHsJust True v = "(Just " ++ v ++ ")"
maybeHsJust False v = v
makeJust :: Int -> String -> String
makeJust n t 
    | n > 0 = "(just (" ++ makeJust (n - 1) t ++ "))"
    | otherwise = t

quote :: String -> String
quote s = "\"" ++ s ++ "\""
indent :: Int -> String -> String
indent x = unlines . (map ((replicate x ' ')++)) . lines

entityFieldName :: Entity -> Field -> String
entityFieldName e f = (lowerFirst . entityName) e ++ (upperFirst . fieldName) f

