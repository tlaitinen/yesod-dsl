{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Generator.Routes where
import AST
import Data.Maybe
import qualified Data.Text as T
import Data.List
import Text.Shakespeare.Text hiding (toText)
import Data.Char
hsRouteName :: [PathPiece] -> String
hsRouteName = f . routeName 
    where f ('/':'#':xs) = f xs
          f ('/':x:xs) = toUpper x : f xs
          f (x:xs) = x : f xs
          f [] = "R"

hsRouteType :: [PathPiece] -> String
hsRouteType = (intercalate " ") . (mapMaybe toType)
    where toType (PathText _) = Nothing
          toType (PathId en) = Just $ en ++ "Id -> "

routeResource :: Resource -> String
routeResource r = T.unpack $(codegenFile "codegen/route.cg")
    where handlers = intercalate " " (map (show . handlerType) (resHandlers r))

routes :: Module -> String
routes m = T.unpack $(codegenFile "codegen/routes-header.cg")
         ++ (concatMap routeResource (modResources m))
         ++ (T.unpack $(codegenFile "codegen/routes-footer.cg"))


