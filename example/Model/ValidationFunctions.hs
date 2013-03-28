{-# LANGUAGE OverloadedStrings #-}
module Model.ValidationFunctions where
import Import
import Model.EmailRegex
import Model.Timezones
import Text.Regex.Posix
import qualified  Data.Text as T

validEmail addr = return (T.unpack addr =~ emailRegex)

validTimezone tz = return (tz `elem` timezones)

nonEmpty "" = return False
nonEmpty _ = return True


