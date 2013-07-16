module Generator.Common where

maybeJust :: Bool -> String -> String
maybeJust True = makeJust 1
maybeJust False = makeJust 0

makeJust :: Int -> String -> String
makeJust n t 
    | n > 0 = "(just " ++ makeJust (n - 1) t ++ ")"
    | otherwise = t


indent :: Int -> String -> String
indent x = unlines . (map ((replicate x ' ')++)) . lines

