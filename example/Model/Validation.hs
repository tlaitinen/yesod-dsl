{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module Model.Validation (Validatable(..)) where
import Data.Text
import qualified Model.ValidationFunctions as V
import Import
checkResult :: forall (m :: * -> *). (Monad m) => Text -> m Bool -> m Text
checkResult msg f = do
   result <- f
   return $ if result then "" else msg

class Validatable a where
    validate :: forall m. (PersistQuery m, PersistEntityBackend a ~ PersistMonadBackend m) => a -> m [Text]
instance Validatable ChangeRecord where 
    validate e = sequence [
        ]

instance Validatable Note where 
    validate e = sequence [
        ]

instance Validatable Person where 
    validate e = sequence [
        checkResult "Person.email validEmail" (V.validEmail $ personEmail e),
        checkResult "Person.timezone validTimezone" (V.validTimezone $ personTimezone e),
        checkResult "Person.lastName nonEmpty" (V.nonEmpty $ personLastName e),
        checkResult "Person.firstName nonEmpty" (V.nonEmpty $ personFirstName e)
        ]

