module Generator.Common where
maybeHsJust :: Bool -> String -> String
maybeHsJust True v = "(Just " ++ v ++ ")"
maybeHsJust False v = v
makeJust :: Int -> String -> String
makeJust n t 
    | n > 0 = "(just (" ++ makeJust (n - 1) t ++ "))"
    | otherwise = t


indent :: Int -> String -> String
indent x = unlines . (map ((replicate x ' ')++)) . lines


