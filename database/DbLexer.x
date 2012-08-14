{
module DbLexer (lexer, tokenType, tokenLineNum, tokenColNum, Token(..), TokenType(..) ) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$lower = [a-z]
$upper = [A-Z]
@stringWithoutSpecialChars = ($printable # [\" \\])*
@specialChars = [\\]$printable
@string = \" (@stringWithoutSpecialChars | @specialChars)* \"

tokens :-
	$white+	;
	"--".*	;
    \n ;
    @string { mkTvar (\s -> TString (stripQuotes s)) }
    \; { mkT TSemicolon } 
    \: { mkT TColon }
    \{ { mkT TLBrace }
    \} { mkT TRBrace }
    \[ { mkT TLBrack }
    \] { mkT TRBrack }
    \( { mkT TLParen }
    \) { mkT TRParen }
    \, { mkT TComma }
    "import" { mkT TImport }
    "entity" { mkT TEntity }
    "relation" { mkT TRelation }
    "interface" { mkT TIface  }
    "implements" { mkT TImplements }
    "unique" { mkT TUnique }
    "index" { mkT TIndex }
    "check" { mkT TCheck }
    "default" { mkT TDefault }
    "word32" { mkT TWord32 }
    "word64" { mkT TWord64 }
    "int32" { mkT TInt32 }
    "int64" { mkT TInt64 }
    "integer" { mkT TInteger }
    "string" { mkT TStringType }
    "bool" { mkT TBool }
    "char" { mkT TChar }
    "rational" { mkT TRational }
    "double" { mkT TDouble }
    "optional" { mkT TOptional }
    "localdate" { mkT TLocalDate }
    "localtime" { mkT TLocalTime }
    "localdatetime" { mkT TLocalDateTime }
    "zonedtime" { mkT TZonedTime }
    "zoneddatetime" { mkT TZonedDateTime }
    "utcdatetime" { mkT TUTCDateTime }
    "posixtime" { mkT TPosixTime }
	$digit+ 		{ mkTvar (\s -> TInt (read s)) }
    $digit+ "." $digit+ { mkTvar (\s -> TFloat (read s)) }
	$lower [$alpha $digit \_ ]*  { mkTvar (\s -> TLowerId s) }
    $upper [$alpha $digit \_ ]*  { mkTvar (\s -> TUpperId s) }
    
{

data Token = Tk AlexPosn TokenType
    deriving (Show)
data TokenType = TSemicolon
               | TColon
               | TLBrace
               | TRBrace
               | TLParen
               | TRParen
               | TLBrack
               | TRBrack
               | TComma
               | TImport
               | TEntity
               | TRelation
               | TImplements
               | TDefault
               | TIndex
               | TUnique
               | TIface
               | TString  String
               | TLowerId String
               | TUpperId String
               | TInt     Int
               | TFloat   Double
               | TCheck
               | TWord32
               | TWord64
               | TInt32
               | TInt64
               | TInteger
               | TStringType
               | TBool
               | TChar
               | TRational
               | TDouble
               | TOptional
               | TLocalTime
               | TLocalDate
               | TLocalDateTime
               | TZonedTime
               | TZonedDateTime
               | TUTCDateTime
               | TPosixTime
	deriving (Show)

stripQuotes s = take ((length s) -2) (tail s)

mkT :: TokenType -> AlexPosn -> String -> Token
mkT t p s = Tk p t

mkTvar :: (String -> TokenType) -> AlexPosn -> String -> Token
mkTvar st p s = Tk p (st s)

tokenLineNum (Tk p _) = getLineNum p
tokenColNum (Tk p _)  = getColNum p
tokenType (Tk _ t) = t

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn offset lineNum colNum) = lineNum

getColNum :: AlexPosn -> Int
getColNum (AlexPn offset lineNum colNum) = colNum

lexer = alexScanTokens 
}
