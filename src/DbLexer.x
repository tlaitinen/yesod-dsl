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
    \= { mkT TEquals }
    \| { mkT TPipe }
    "get" { mkT TGet }
    "put" { mkT TPut }
    "post" { mkT TPost }
    "validate" { mkT TValidate }

    "delete" { mkT TDelete }
    "import" { mkT TImport }
    "enum" { mkT TEnum }
    "entity" { mkT TEntity }
    "class" { mkT TClass  }
    "unique" { mkT TUnique }
    "check" { mkT TCheck }
    "pre-hook" { mkT TPreHook }
    "post-hook" { mkT TPostHook }
    "public" { mkT TPublic }
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
    "default-filter-sort" { mkT TDefaultFilterSort }
    "filter" { mkT TFilter }
    "select-opts" { mkT TSelectOpts }
    "deriving" { mkT TDeriving }
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
               | TEquals
               | TPipe
               | TComma
               | TDot
               | TImport
               | TEnum
               | TEntity
               | TUnique
               | TClass
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
               | TGet
               | TPut
               | TPost
               | TDelete
               | TPublic
               | TPreHook
               | TPostHook
               | TValidate
               | TDefaultFilterSort
               | TFilter
               | TSelectOpts
               | TDeriving
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
