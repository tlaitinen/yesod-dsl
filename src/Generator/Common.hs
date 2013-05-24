module Generator.Common where

maybeJust :: Bool -> String -> String
maybeJust True s = "(Just " ++ s ++ ")"
maybeJust False s = s


indent :: Int -> String -> String
indent x = unlines . (map ((replicate x ' ')++)) . lines

