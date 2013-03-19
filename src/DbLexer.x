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
@fieldName = \' $lower [$alpha $digit \_ ]* \'


tokens :-
	$white+	;
	"--".*	;
    \n ;
    @string { mkTvar (TString . stripQuotes) }
    \; { mkT TSemicolon } 
    \: { mkT TColon }
    \{ { mkT TLBrace }
    \} { mkT TRBrace }
    \[ { mkT TLBrack }
    \] { mkT TRBrack }
    \( { mkT TLParen }
    \) { mkT TRParen }
    \, { mkT TComma }
    \. { mkT TDot }
    "import" { mkT TImport }
    "entity" { mkT TEntity }
    "class" { mkT TIface  }
    "unique" { mkT TUnique }
    "check" { mkT TCheck }
    "Word32" { mkT TWord32 }
    "Word64" { mkT TWord64 }
    "Int32" { mkT TInt32 }
    "Int64" { mkT TInt64 }
    "Text" { mkT TText }
    "Bool" { mkT TBool }
    "Double" { mkT TDouble }
    "Maybe" { mkT TMaybe }
    "Date" { mkT TDate }
    "Time" { mkT TTime }
    "DateTime" { mkT TDateTime }
    "ZonedTime" { mkT TZonedTime }
	$digit+ 		{ mkTvar (TInt . read) }
    $digit+ "." $digit+ { mkTvar (TFloat . read) }
	$lower [$alpha $digit \_ ]*  { mkTvar TLowerId  }
    $upper [$alpha $digit \_ ]*  { mkTvar TUpperId  }
    @fieldName { mkTvar (TLowerId . stripQuotes) }
    
    
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
               | TDot
               | TImport
               | TEntity
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
               | TText
               | TBool
               | TDouble
               | TMaybe
               | TTime
               | TDate
               | TDateTime
               | TZonedTime
               | TAsc
               | TDesc
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
