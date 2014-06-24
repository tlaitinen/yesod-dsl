module YesodDsl.Parser.Tokens where

import YesodDsl.Parser.Types
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

ydslStyle = P.LanguageDef {
    P.commentStart   = "{-",
    P.commentEnd     = "-}",
    P.commentLine    = "--",
    P.nestedComments = False,
    P.identStart     = letter,
    P.identLetter	 = alphaNum <|> oneOf "_'",
    P.opStart	 =  oneOf ":!#$%&*+./<=>?@\\^|-~",
    P.opLetter	 = oneOf ":!#$%&*+./<=>?@\\^|-~",
    P.reservedOpNames= [],
    P.reservedNames  = ["unique", "deriving", "instance", "of"],
    P.caseSensitive  = True
}
lexer = P.makeTokenParser ydslStyle

comma :: YdslParser Char
comma = char ','


upperCaseId :: YdslParser String
upperCaseId = do
    try $ lookAhead $ upper
    P.identifier lexer

upperCaseIdWithId :: YdslParser String
upperCaseIdWithId = do
    try $ lookAhead $ upper
    manyTill alphaNum (try (string "Id"))

lowerCaseId :: YdslParser String
lowerCaseId = do
    try $ lookAhead $ lower
    x <- P.identifier lexer
    return x

spaceNoEol :: YdslParser Char
spaceNoEol = oneOf [' ', '\t','\f', '\v']

spacesNoEol :: YdslParser ()
spacesNoEol = do
    many $ spaceNoEol
    return ()

spaces' :: YdslParser ()
spaces' = P.whiteSpace lexer

stringAndSpace :: String -> YdslParser ()
stringAndSpace s = do
    string s
    many1 spaceNoEol
    return ()

quotedString :: YdslParser String
quotedString = P.stringLiteral lexer

parseInteger :: YdslParser Integer
parseInteger = P.integer lexer

parseDouble :: YdslParser Double
parseDouble = P.float lexer

