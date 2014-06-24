module YesodDsl.Parser.Field where

import YesodDsl.AST
import YesodDsl.Parser.Types
import YesodDsl.Parser.Tokens
import YesodDsl.Parser.Indent
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec
value :: YdslParser FieldValue
value = 
    (do
        x <- quotedString 
        return $ StringValue x)
    <|> (do
        x <- parseDouble
        return $ FloatValue x)
    <|> (do
        x <- parseInteger
        return $ IntValue $ fromIntegral x)
    <|> (do
        string "True"
        return $ BoolValue True)
    <|> (do
        string "False"
        return $ BoolValue False)
    <|> (do
        string "Nothing"
        return $ NothingValue)

 
fieldTypeParser :: YdslParser FieldType 
fieldTypeParser = 
    (string "Word32" >> return FTWord32) <|>
    (string "Word64" >> return FTWord64) <|>
    (string "Int32" >> return FTInt32) <|>
    (string "Int64" >> return FTInt64) <|>
    (string "Text" >> return FTText) <|>
    (string "Bool" >> return FTBool) <|>
    (string "Double" >> return FTDouble) <|>
    (string "TimeOfDay" >> return FTTimeOfDay) <|>
    (string "Day" >> return FTDay) <|>
    (string "ZonedTime" >> return FTZonedTime)
       
fieldContentParser :: YdslParser FieldContent
fieldContentParser = (do
    ft <- fieldTypeParser
    return $ NormalField ft)
    <|> (do
        en <- upperCaseIdWithId
        return $ EntityField en)
    <|> (do
        en <- upperCaseId
        return $ EnumField en)
 
checkDef :: YdslParser FunctionName
checkDef = stringAndSpace "check" >> lowerCaseId
fieldOptionParser :: YdslParser FieldOption
fieldOptionParser =
    (fmap FieldCheck checkDef)
   <|> (do
        stringAndSpace "default"
        v <- value
        return $ FieldDefault v)
    <|> (string "internal" >> (return FieldInternal))



fieldDef :: YdslParser Field
fieldDef = do 
    checkIndent
    pos <- getPosition
    n <- lowerCaseId
    withPos $ do
        spaces'
        optional <- option False (sameOrIndented >> string "Maybe" >> return True)
        spaces'
        fc <- fieldContentParser
        opts <- many (try $ sameOrIndented >> spaces' >> fieldOptionParser)
        spaces'
        return $ Field pos optional n fc opts
 

fieldId :: YdslParser FieldName
fieldId = lowerCaseId -- TODO: validate

uniqueDef :: YdslParser Unique
uniqueDef = do
    checkIndent
    pos <- getPosition
    stringAndSpace "unique" 
    n <- upperCaseId
    withPos $ do
        fs <- many1 (try $ sameOrIndented >> spaces' >> fieldId)
        spaces'
        return $ Unique pos  n fs


