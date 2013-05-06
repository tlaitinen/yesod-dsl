{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module Model.Validation (Validatable(..)) where
import Data.Text
import Data.Maybe
import qualified Model.ValidationFunctions as V
import Import
checkResult :: forall (m :: * -> *). (Monad m) => Text -> m Bool -> m (Maybe Text)
checkResult msg f = do
   result <- f
   return $ if result then Nothing else (Just msg)

class Validatable a where
    validate :: forall m. (PersistQuery m, PersistEntityBackend a ~ PersistMonadBackend m) => a -> m [Text]

instance Validatable ChangeRecord where 
    validate e = do
        errors <- sequence [
            ]
        return $ catMaybes errors

instance Validatable Note where 
    validate e = do
        errors <- sequence [
            ]
        return $ catMaybes errors

instance Validatable Person where 
    validate e = do
        errors <- sequence [
            checkResult "Person.email validEmail" (V.validEmail $ personEmail e),
            checkResult "Person.timezone validTimezone" (V.validTimezone $ personTimezone e),
            checkResult "Person.lastName nonEmpty" (V.nonEmpty $ personLastName e),
            checkResult "Person.firstName nonEmpty" (V.nonEmpty $ personFirstName e)
            ]
        return $ catMaybes errors

