{
module Parser (parse) where
import Lexer
import AST
import ModuleMerger
import System.IO
import Data.Maybe
import Data.Typeable
import Prelude hiding (catch) 
import Control.Exception hiding (Handler)
import System.Exit


}

%name moduleDefs
%tokentype { Token }
%error { parseError }

%token
    module { Tk _ TModule }
    import     { Tk _ TImport }
    enum       { Tk _ TEnum }
    pipe       { Tk _ TPipe }
    entity   { Tk _ TEntity }
    class      { Tk _ TClass }
    route    { Tk _ TRoute }
    unique     { Tk _ TUnique }
    check      { Tk _ TCheck }
    lowerId    { Tk _ (TLowerId $$) }
    upperId    { Tk _ (TUpperId $$)  }
    intval        { Tk _ (TInt $$)  }
    floatval      { Tk _ (TFloat $$) }
    semicolon  { Tk _ TSemicolon }
    hash        { Tk _ THash }
    equals { Tk _ TEquals }
    concat { Tk _ TConcat }
    ne { Tk _ TNe }
    lt { Tk _ TLt }
    gt { Tk _ TGt }
    le { Tk _ TLe }
    ge { Tk _ TGe }
    and { Tk _ TAnd }
    or { Tk _ TOr }
    like  { Tk _ TLike }
    ilike { Tk _ TIlike }
    lbrace { Tk _ TLBrace }
    rbrace { Tk _ TRBrace }
    lparen { Tk _ TLParen }
    rparen { Tk _ TRParen }
    comma  { Tk _ TComma }
    dot  { Tk _ TDot }
    slash { Tk _ TSlash }
    stringval     { Tk _ (TString $$) }
    word32   { Tk _ TWord32 }
    word64   { Tk _ TWord64 }
    int32    { Tk _ TInt32 }
    int64    { Tk _ TInt64 }
    text { Tk _ TText }
    bool { Tk _ TBool }
    double { Tk _ TDouble }
    timeofday { Tk _ TTimeOfDay }
    day { Tk _ TDay }
    utctime { Tk _ TUTCTime }
    zonedtime { Tk _ TZonedTime }
    maybe { Tk _ TMaybe }
    get { Tk _ TGet }
    param { Tk _ TParam }
    not { Tk _ TNot }
    if {  Tk _ TIf }
    then { Tk _ TThen }
    asterisk { Tk _ TAsterisk }
    put { Tk _ TPut }
    post { Tk _ TPost }
    delete { Tk _ TDelete }
    public { Tk _ TPublic }
    instance { Tk _ TInstance }
    of { Tk _ TOf }
    in { Tk _ TIn }
    limit { Tk _ TLimit }
    offset { Tk _ TOffset } 
    select { Tk _ TSelect }
    from { Tk _ TFrom }
    join { Tk _ TJoin }
    inner { Tk _ TInner }
    outer { Tk _ TOuter }
    left { Tk _ TLeft }
    right { Tk _ TRight }
    full { Tk _ TFull }
    cross { Tk _ TCross }
    on { Tk _ TOn }
    as { Tk _ TAs }
    is { Tk _ TIs }
    insert { Tk _ TInsert }
    update { Tk _ TUpdate }
    defaultfiltersort { Tk _ TDefaultFilterSort }
    identified { Tk _ TIdentified }
    with { Tk _ TWith }
    order { Tk _ TOrder }
    by { Tk _ TBy }
    asc { Tk _ TAsc }
    desc { Tk _ TDesc }
    where { Tk _ TWhere }
    deriving { Tk _ TDeriving }
    default  { Tk _ TDefault }
    pathParam { Tk _ (TPathParam $$) }
    idField { Tk _ TId }
    entityId { Tk _ (TEntityId $$) }
    localParam { Tk _ TLocalParam }
    true { Tk _ TTrue }
    false { Tk _ TFalse }
    nothing { Tk _ TNothing }
    request { Tk _ TRequest }
    larrow { Tk _ TLArrow }
    now { Tk _ TNow }
    auth { Tk _ TAuth }
    return { Tk _ TReturn }
    require { Tk _ TRequire }
    internal { Tk _ TInternal }
    underscore { Tk _ TUnderScore }
%%

dbModule : maybeModuleName 
           imports defs { Module $1 (reverse $2) ((reverse . getEntities) $3) 
                                    ((reverse . getClasses) $3)
                                    ((reverse . getEnums) $3)
                                    ((reverse . getRoutes) $3)}


maybeModuleName : { Nothing }
                | module upperId semicolon { Just $2 }
imports : { [] }
        | imports importStmt { $2 : $1 }

importStmt : import stringval semicolon { $2 }
defs : { [] }
       | defs def  { $2 : $1 }
def : routeDef     { RouteDef $1 }
      | entityDef      { EntityDef $1 } 
      | classDef      { ClassDef $1 }
      | enumDef       { EnumDef $1 }

enumDef : enum upperId equals enumValues semicolon 
         { EnumType (mkLoc $1) $2 $4 }

enumValues : upperId { [$1] }
           | enumValues pipe upperId { $3 : $1 }
    
entityDef : entity upperId lbrace 
            maybeInstances
            fields
            uniques
            maybeDeriving
            checks
            rbrace { Entity (mkLoc $1) $2 $4 (reverse $5)
                            (reverse $6) $7 (reverse $8) }

routeDef : route pathPieces lbrace handlers rbrace { Route (mkLoc $1) (reverse $2) (reverse $4) }
pathPieces : slash pathPiece { [$2] }
           | pathPieces slash pathPiece { $3 : $1 }

pathPiece : lowerId { PathText $1 } 
          | hash entityId { PathId $2 }

handlers : handlerdef  { [$1] }
         | handlers handlerdef { $2 : $1 }

handlerdef : get handlerParamsBlock { Handler (mkLoc $1) GetHandler $2 }
           | put handlerParamsBlock { Handler (mkLoc $1) PutHandler $2 }
           | post handlerParamsBlock { Handler (mkLoc $1) PostHandler $2 }
           | delete handlerParamsBlock { Handler (mkLoc $1) DeleteHandler $2 }

fieldRef : lowerId dot idField { FieldRefId $1 }
          | lowerId dot lowerId { FieldRefNormal $1 $3 } 
          | upperId dot upperId { FieldRefEnum $1 $3 }
          | pathParam { FieldRefPathParam $1 }
          | auth dot idField { FieldRefAuthId }
          | auth dot lowerId { FieldRefAuth $3 }
          | localParam { FieldRefLocalParam }
          | request dot lowerId { FieldRefRequest $3 }
          | lparen select selectField from upperId as lowerId 
               joins maybeWhere rparen  
                   { FieldRefSubQuery (SelectQuery [$3] ($5,$7) (reverse $8) $9 
                                      [] (0,0)) }
 
    
handlerParamsBlock : lbrace handlerParams rbrace { (reverse $2) }

handlerParams : { [] }
              | handlerParams handlerParam semicolon { $2 : $1 }
handlerParam : public { Public }
             | select selectFields from upperId as lowerId 
               joins maybeWhere maybeOrder maybeLimitOffset 
              { Select (SelectQuery $2 ($4,$6) (reverse $7) $8 $9 $10) }
             | update upperId identified by inputRef with inputJson { Update $2 $5 (Just $7) } 
             | delete from upperId as lowerId { DeleteFrom $3 $5 Nothing }
             | delete from upperId as lowerId where expr { DeleteFrom $3 $5 (Just $7) }
             | update upperId identified by inputRef { Update $2 $5 Nothing }
             | bindResult get upperId identified by inputRef { GetById $3 $6 $1 }
             | maybeBindResult insert upperId from inputJson { Insert $3 (Just $5) $1 }
             | maybeBindResult insert upperId { Insert $3 Nothing $1 }
             | defaultfiltersort { DefaultFilterSort }
             | if param stringval equals localParam then joins where expr { IfFilter ($3 ,(reverse $7) ,$9, True) }
             | if param stringval equals underscore then joins where expr { IfFilter ($3, (reverse $7), $9, False) }
             | return outputJson { Return $2 }
             | require upperId as lowerId joins where expr { Require (SelectQuery [] ($2,$4) (reverse $5) (Just $7) [] (0,0)) }
maybeBindResult: { Nothing }
               | bindResult { Just $1 }

bindResult: lowerId larrow { $1 }

selectFields: selectField moreSelectFields { $1 : (reverse $2) }

moreSelectFields: { [] }
                | moreSelectFields comma selectField { $3 : $1 }

selectField: lowerId dot asterisk { SelectAllFields $1 }                    
           | lowerId dot idField maybeSelectAlias { SelectIdField $1 $4 }
           | lowerId dot lowerId maybeSelectAlias { SelectField $1 $3 $4 }
       
maybeSelectAlias: { Nothing }
                | as lowerId { Just $2 }
joins : { [] }
      | joins jointype upperId as lowerId maybeJoinOn { (Join $2 $3 $5 $6):$1 }

maybeWhere : { Nothing }
           | where expr { Just $2 }

maybeOrder: { [] }
          | order by orderByList { (reverse $3) }

maybeLimitOffset: { (10000,0) }
                | limit intval maybeOffset {  ($2,$3) }
maybeOffset: { 0 }
           | offset intval { $2 }

orderByList : orderByListitem { [$1] }
        | orderByList comma orderByListitem { $3 : $1 }
orderByListitem : fieldRef orderByDir { ($1, $2) }

orderByDir : asc { SortAsc }
        | desc  { SortDesc }
 
inputJson:  lbrace inputJsonFields rbrace { $2 }
inputJsonField : lowerId equals inputRef { ($1, $3) }

inputRef: request dot lowerId { InputFieldNormal $3 }
        | lowerId { InputFieldLocalParam $1 }
        | lowerId dot lowerId { InputFieldLocalParamField $1 $3 }
        | pathParam { InputFieldPathParam $1 }
        | auth dot idField { InputFieldAuthId }
        | auth dot lowerId { InputFieldAuth $3 }
        | value { InputFieldConst $1 }
        | now { InputFieldNow }

inputJsonFields : inputJsonField { [$1] }
           | inputJsonFields comma inputJsonField  { $3:$1 }

outputJson: lbrace maybeOutputJsonFields rbrace { $2 }

maybeOutputJsonFields: { [] }
                     | outputJsonFields { $1 }
outputJsonFields: outputJsonField { [ $1 ] }
                | outputJsonFields comma outputJsonField { $3:$1 }

outputJsonField : lowerId equals outputRef { ($1,$3) }

outputRef: lowerId { OutputFieldLocalParam $1 }

binop : equals { Eq }
      | ne { Ne }
      | lt { Lt }
      | gt { Gt }
      | le { Le }
      | ge { Ge }
      | like { Like }
      | ilike { Ilike }
      | is { Is }
      

expr : expr and expr { AndExpr $1 $3 }
     | expr or expr { OrExpr $1 $3 }
     | not expr { NotExpr $2 }
     | lparen expr rparen { $2 } 
     | valexpr binop valexpr { BinOpExpr $1 $2 $3 }
     | fieldRef listOp fieldRef { ListOpExpr $1 $2 $3 }
listOp: in { In }
      | not in { NotIn }

valexpr : value { ConstExpr $1 }
        | fieldRef { FieldExpr $1 }
        | valexpr concat valexpr { ConcatExpr $1 $3 }

maybeJoinOn : { Nothing }
            | on expr { Just $2 }
            
jointype : inner join { InnerJoin }
         | cross join { CrossJoin } 
         | left outer join { LeftOuterJoin }
         | right outer join { RightOuterJoin }
         | full outer join { FullOuterJoin }
         
             
maybeInstances : { [] }
               | instance of instances semicolon { (reverse $3) }

instances : upperId { [$1] }
            | instances comma upperId { $3 : $1 }

classDef : class upperId lbrace
             fields
            uniques
            rbrace { Class (mkLoc $1) $2 (reverse $4) (reverse $5)  }

fields : { [] }
              | fields field semicolon { $2 : $1 }
 
field : lowerId maybeMaybe fieldType fieldOptions fieldFlags { Field $2 (FieldInternal `elem` $5) $1 (NormalField $3 (reverse $4)) } 
      | lowerId maybeMaybe entityId fieldFlags { Field $2 (FieldInternal `elem` $4) $1 (EntityField $3) }
      | lowerId maybeMaybe upperId fieldFlags { Field $2 (FieldInternal `elem` $4) $1 (EnumField $3) }

fieldOptions : { [] }
             | fieldOptionsList { $1 }
fieldOptionsList : fieldOption { [$1] }
                 | fieldOptionsList  fieldOption { $2 : $1 }
fieldOption : check lowerId { FieldCheck $2 }
            | default value { FieldDefault $2 }

fieldFlags : { [] }
           | fieldFlagList { $1 }
fieldFlagList : fieldFlag { [$1] }
              | fieldFlagList fieldFlag { $2 : $1 }
fieldFlag : internal { FieldInternal }              
            
value : stringval { StringValue $1 }
      | intval { IntValue $1 }
      | floatval { FloatValue $1 }
      | true { BoolValue True }
      | false { BoolValue False }
      | nothing { NothingValue }

      
uniques : { [] }
        | uniques uniqueDef semicolon { $2 : $1 }
uniqueDef :  unique upperId fieldIdList { Unique $2 (reverse $3) }

maybeDeriving : { [] }
             | deriving derives semicolon { (reverse $2) }
derives : upperId { [$1] }
        | derives comma upperId { $3 : $1 }

checks : { [] }
        | check fieldIdList semicolon { reverse $2 }

fieldIdList : lowerId { [$1] }
            | fieldIdList comma lowerId { $3 : $1 }

fieldType : word32 { FTWord32 }
          | word64{ FTWord64 }
          | int32{ FTInt32 }
          | int64{ FTInt64 }
          | text { FTText }
          | bool{ FTBool }
          | double{ FTDouble }
          | timeofday { FTTimeOfDay }
          | day { FTDay }
          | utctime{ FTUTCTime }
          | zonedtime{ FTZonedTime }

maybeMaybe : { False }
              | maybe { True }

{

data ModDef = EntityDef Entity
           | ClassDef Class
           | EnumDef EnumType
           | RouteDef Route
           deriving (Show)

data FieldFlag = FieldInternal deriving (Eq)
getEntities :: [ModDef] -> [Entity]
getEntities defs = mapMaybe (\d -> case d of (EntityDef e) -> Just e ; _ -> Nothing) defs

getClasses :: [ModDef] -> [Class]
getClasses defs = mapMaybe (\d -> case d of (ClassDef c) -> Just c; _ -> Nothing) defs

getEnums :: [ModDef] -> [EnumType]
getEnums defs = mapMaybe (\d -> case d of (EnumDef e) -> Just e; _ -> Nothing) defs

getRoutes :: [ModDef] -> [Route]
getRoutes defs = mapMaybe (\d -> case d of (RouteDef e) -> Just e; _ -> Nothing) defs



data ParseError = ParseError String deriving (Show, Typeable)
instance Exception ParseError

parseError :: [Token] -> a
parseError (t:ts) = throw (ParseError $ "Parse error : unexpected " ++ show (tokenType t) ++ " at line " ++ show (tokenLineNum t) ++ " col " ++ show (tokenColNum t))
parseError _ = throw (ParseError $ "Parse error : unexpected end of file")

parseModule :: FilePath -> IO Module
parseModule path = catch 
        (do
            s <- readFile path
            return $! moduleDefs $! lexer s)
        (\(ParseError msg) -> do 
            hPutStrLn stderr $ path ++ ": " ++ msg
            exitWith (ExitFailure 1))
       

parseModules :: [FilePath] -> [FilePath] -> IO [(FilePath,Module)]
parseModules handled (path:paths)
    | path `elem` handled = return []
    | otherwise = do
        mod <- parseModule path
        rest <- parseModules (path:handled) (paths ++ modImports mod)
        return ((path,mod):rest)
parseModules _ [] = return []

parse path = parseModules [] [path]
}
