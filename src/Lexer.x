{
module Lexer (lexer, tokenType, tokenLineNum, tokenColNum, Token(..), TokenType(..) ) where
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
@pathParam = \$ ($digit)+
@authId = \$ "authId"
@entityId = $upper [$alpha $digit \_]* "Id"
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
    \!\= { mkT TNe }
    \< { mkT TLt }
    \> { mkT TGt }
    \<\= { mkT TLe }
    \>\= { mkT TGe }
    "like" { mkT TLike }
    \| { mkT TPipe }
    \/ { mkT TSlash }
    \# { mkT THash }
    "get" { mkT TGet }
    "put" { mkT TPut }
    "post" { mkT TPost }
    "delete" { mkT TDelete }
    "import" { mkT TImport }
    "enum" { mkT TEnum }
    "entity" { mkT TEntity }
    "class" { mkT TClass  }
    "resource" { mkT TResource }
    "unique" { mkT TUnique }
    "check" { mkT TCheck }
    "before-handler" { mkT TBeforeHandler }
    "after-handler" { mkT TAfterHandler }
    "inner" { mkT TInner }
    "outer" { mkT TOuter }
    "left" { mkT TLeft }
    "right" { mkT TRight }
    "join" { mkT TJoin }
    "full" { mkT TFull }
    "cross" { mkT TCross }
    "on" { mkT TOn }
    "as" { mkT TAs }
    "public" { mkT TPublic }
    "select" { mkT TSelect }
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
    "text-search-filter" { mkT TTextSearchFilter }
    "order" { mkT TOrder }
    "by" { mkT TBy }
    "and" { mkT TAnd }
    "or" { mkT TOr }
    "asc" { mkT TAsc }
    "desc" { mkT TDesc }
    "limit" { mkT TLimit }
    "where" { mkT TWhere }
    "return" { mkT TReturn }
    "default" { mkT TDefault }
    "instance" { mkT TInstance }
    "of" { mkT TOf }
    "deriving" { mkT TDeriving }
	$digit+ 		{ mkTvar (TInt . read) }
    $digit+ "." $digit+ { mkTvar (TFloat . read) }
	$lower [$alpha $digit \_ ]*  { mkTvar TLowerId  }
    @entityId { mkTvar (TEntityId . (reverse . (drop 2) . reverse)) }
    $upper [$alpha $digit \_ ]*  { mkTvar TUpperId  }
    @fieldName { mkTvar (TLowerId . stripQuotes) }
    @pathParam { mkTvar (TPathParam . (read . (drop 1))) }
    @authId { mkT TAuthId }
    
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
               | TNe 
               | TLt
               | TGt
               | TLe
               | TGe
               | TLike
               | TPipe
               | TComma
               | TDot
               | TImport
               | TEnum
               | TEntity
               | TUnique
               | TClass
               | TResource
               | THash
               | TLimit 
               | TString  String
               | TLowerId String
               | TUpperId String
               | TInt     Int
               | TFloat   Double
               | TSlash
               | TOrder
               | TBy
               | TAsc
               | TDesc
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
               | TJoin
               | TLeft
               | TRight
               | TInner
               | TOuter
               | TFull
               | TCross
               | TOn
               | TGet
               | TPut
               | TPost
               | TInstance
               | TOf
               | TDelete
               | TPublic
               | TSelect
               | TAnd
               | TOr
               | TBeforeHandler
               | TAfterHandler
               | TDefaultFilterSort
               | TTextSearchFilter
               | TWhere
               | TAs
               | TReturn
               | TDeriving
               | TDefault
               | TPathParam Int
               | TAuthId 
               | TEntityId String
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
