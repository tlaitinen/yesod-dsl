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
@entityId = $upper [$alpha $digit \_]* "Id"
tokens :-
	$white+	;
	"--".*	;
    \n ;
    @string { mkTvar (TString . stripQuotes) }
    \; { mkT TSemicolon } 
    \{ { mkT TLBrace }
    \} { mkT TRBrace }
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
    \<\- { mkT TLArrow }
    "like" { mkT TLike }
    "ilike" { mkT TIlike }
    "||" { mkT TConcat }
    \| { mkT TPipe }
    \/ { mkT TSlash }
    \# { mkT THash }
    \* { mkT TAsterisk }
    \+ { mkT TPlus }
    \- { mkT TMinus }
    "get" { mkT TGet }
    "put" { mkT TPut }
    "post" { mkT TPost }
    "delete" { mkT TDelete }
    "import" { mkT TImport }
    "enum" { mkT TEnum }
    "module" { mkT TModule }
    "entity" { mkT TEntity }
    "class" { mkT TClass  }
    "route" { mkT TRoute }
    "unique" { mkT TUnique }
    "check" { mkT TCheck }
    "inner" { mkT TInner }
    "outer" { mkT TOuter }
    "left" { mkT TLeft }
    "right" { mkT TRight }
    "join" { mkT TJoin }
    "full" { mkT TFull }
    "cross" { mkT TCross }
    "on" { mkT TOn }
    "as" { mkT TAs }
    "is" { mkT TIs }
    "public" { mkT TPublic }
    "select" { mkT TSelect }
    "update" { mkT TUpdate }
    "insert" { mkT TInsert }
    "from" { mkT TFrom }
    "Word32" { mkT TWord32 }
    "Word64" { mkT TWord64 }
    "Int32" { mkT TInt32 }
    "Int64" { mkT TInt64 }
    "Text" { mkT TText }
    "Bool" { mkT TBool }
    "Double" { mkT TDouble }
    "Maybe" { mkT TMaybe }
    "Day" { mkT TDay }
    "True" { mkT TTrue }
    "False" { mkT TFalse }
    "Nothing" { mkT TNothing }
    "TimeOfDay" { mkT TTimeOfDay }
    "UTCTime" { mkT TUTCTime }
    "ZonedTime" { mkT TZonedTime }
    "default-filter-sort" { mkT TDefaultFilterSort }
    "param" { mkT TParam }
    "if" { mkT TIf }
    "then" { mkT TThen }
    "order" { mkT TOrder }
    "identified" { mkT TIdentified }
    "with" { mkT TWith }
    "by" { mkT TBy }
    "and" { mkT TAnd }
    "or" { mkT TOr }
    "asc" { mkT TAsc }
    "desc" { mkT TDesc }
    "in" { mkT TIn }
    "not" { mkT TNot }
    "limit" { mkT TLimit }
    "offset" { mkT TOffset }
    "where" { mkT TWhere }
    "default" { mkT TDefault }
    "instance" { mkT TInstance }
    "of" { mkT TOf }
    "deriving" { mkT TDeriving }
    "$request" { mkT TRequest }
    "require" { mkT TRequire }
    "internal" { mkT TInternal }
    "define" { mkT TDefine }
    "extract" { mkT TExtract }
     "return" { mkT TReturn }
    "$auth"  { mkT TAuth }
    "for" { mkT TFor }
    $digit+ 		{ mkTvar (TInt . read) }
    $digit+ "." $digit+ { mkTvar (TFloat . read) }
     "id" { mkT TId }
    $lower [$alpha $digit \_ ]*  { mkTvar TLowerId  }
    @entityId { mkTvar (TEntityId . (reverse . (drop 2) . reverse)) }
    $upper [$alpha $digit \_ ]*  { mkTvar TUpperId  }
    @fieldName { mkTvar (TLowerId . stripQuotes) }
    @pathParam { mkTvar (TPathParam . (read . (drop 1))) }
     "$$"  { mkT TLocalParam }
     "_" { mkT TUnderScore }
     "now()" { mkT TNow }

{

data Token = Tk AlexPosn TokenType deriving (Show)
data TokenType = TSemicolon
           | TLBrace
           | TRBrace
           | TLParen
           | TRParen
           | TEquals
           | TNe 
           | TLt
           | TGt
           | TLe
           | TGe
           | TLike
           | TIlike
           | TPipe
           | TComma
           | TDot
           | TImport
           | TEnum
           | TModule
           | TEntity
           | TUnique
           | TClass
           | TRoute
           | THash
           | TIn
           | TLimit 
           | TOffset
           | TString  String
           | TLowerId String
           | TUpperId String
           | TInt     Int
           | TFloat   Double
           | TConcat
           | TSlash
           | TOrder
           | TIdentified 
           | TUpdate
           | TInsert
           | TWith 
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
           | TTimeOfDay
           | TDay
           | TUTCTime
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
           | TDefine
           | TFrom
           | TAnd
           | TOr
           | TBeforeHandler
           | TAfterHandler
           | TDefaultFilterSort
           | TId
           | TNot
           | TIf
           | TThen
           | TAsterisk
           | TPlus
           | TMinus
           | TParam
           | TWhere
           | TAs
           | TIs
           | TDeriving
           | TDefault
           | TPathParam Int
           | TEntityId String
           | TLocalParam
           | TTrue
           | TFalse
           | TNothing
           | TRequest
           | TLArrow
           | TNow
           | TAuth
           | TReturn
           | TInternal
           | TRequire
           | TUnderScore
           | TFor
           | TExtract
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
