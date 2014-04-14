module Generator.Common where
import AST
maybeHsJust :: Bool -> String -> String
maybeHsJust True v = "(Just " ++ v ++ ")"
maybeHsJust False v = v

makeJust :: Bool -> String -> String
makeJust True t = "just (" ++ t ++ ")"
makeJust False t = t

quote :: String -> String
quote s = "\"" ++ s ++ "\""
indent :: Int -> String -> String
indent x = unlines . (map ((replicate x ' ')++)) . lines

entityFieldName :: Entity -> Field -> String
entityFieldName e f = (lowerFirst . entityName) e ++ (upperFirst . fieldName) f

