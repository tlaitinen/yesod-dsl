{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module YesodDsl.Generator.Routes where
import YesodDsl.AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import Data.Char
routeModuleName :: Route -> String
routeModuleName r = "Route" ++ (concat $ map f (routePath r))
    where f (PathText s) = upperFirst s
          f (PathId _ en) = upperFirst en
hsRouteName :: [PathPiece] -> String
hsRouteName = f . routeName 
    where f ('/':'#':xs) = f xs
          f ('/':x:xs) = toUpper x : f xs
          f (x:xs) = x : f xs
          f [] = "R"

hsRouteType :: [PathPiece] -> String
hsRouteType = (intercalate " ") . (mapMaybe toType)
    where toType (PathText _) = Nothing
          toType (PathId _ en) = Just $ en ++ "Id -> "

hsRoutePath :: Route -> String
hsRoutePath r = T.unpack $(codegenFile "codegen/route.cg")
    where handlers = intercalate " " (map (show . handlerType) (routeHandlers r))

routes :: Module -> String
routes m = T.unpack $(codegenFile "codegen/routes-header.cg")
         ++ (concatMap hsRoutePath (modRoutes m))
         ++ (T.unpack $(codegenFile "codegen/routes-footer.cg"))
     where
           routeImport r = T.unpack $(codegenFile "codegen/route-import.cg")



