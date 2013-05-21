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
    resource    { Tk _ TResource }
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
    lbrack { Tk _ TLBrack }
    rbrack { Tk _ TRBrack }
    comma  { Tk _ TComma }
    colon { Tk _ TColon }
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
    time { Tk _ TTime }
    date { Tk _ TDay }
    utctime { Tk _ TUTCTime }
    zonedtime { Tk _ TZonedTime }
    maybe { Tk _ TMaybe }
    get { Tk _ TGet }
    filter { Tk _ TFilter }
    if {  Tk _ TIf }
    then { Tk _ TThen }
    asterisk { Tk _ TAsterisk }
    put { Tk _ TPut }
    post { Tk _ TPost }
    delete { Tk _ TDelete }
    public { Tk _ TPublic }
    return { Tk _ TReturn }
    instance { Tk _ TInstance }
    of { Tk _ TOf }
    beforehandler { Tk _ TBeforeHandler }
    afterhandler { Tk _ TAfterHandler }
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
    insert { Tk _ TInsert }
    replace { Tk _ TReplace }
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
    authId { Tk _ TAuthId }
    idField { Tk _ TId }
    entityId { Tk _ (TEntityId $$) }
    localParam { Tk _ TLocalParam }
%%

dbModule : maybeModuleName 
           imports defs { Module $1 (reverse $2) ((reverse . getEntities) $3) 
                                    ((reverse . getClasses) $3)
                                    ((reverse . getEnums) $3)
                                    ((reverse . getResources) $3)}


maybeModuleName : { Nothing }
                | module upperId semicolon { Just $2 }
imports : { [] }
        | imports importStmt { $2 : $1 }

importStmt : import stringval semicolon { $2 }
defs : { [] }
       | defs def  { $2 : $1 }
def : resourceDef     { ResourceDef $1 }
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
            derives
            checks
            rbrace { Entity (mkLoc $1) $2 (reverse $4) (reverse $5)
                            (reverse $6) (reverse $7) (reverse $8) }

resourceDef : resource pathPieces lbrace handlers rbrace { Resource (mkLoc $1) (reverse $2) (reverse $4) }
pathPieces : slash pathPiece { [$2] }
           | pathPieces slash pathPiece { $3 : $1 }

pathPiece : lowerId { PathText $1 } 
          | hash entityId { PathId $2 }

handlers : handlerdef  { [$1] }
         | handlers handlerdef { $2 : $1 }

handlerdef : get handlerParamsBlock { Handler GetHandler $2 }
           | put handlerParamsBlock { Handler PutHandler $2 }
           | post handlerParamsBlock { Handler PostHandler $2 }
           | delete handlerParamsBlock { Handler DeleteHandler $2 }

fieldRefList : fieldRef { [$1] }
              | fieldRefList comma fieldRef { $3 : $1 }

fieldRef : lowerId dot idField { FieldRefId $1 }
          | lowerId dot lowerId { FieldRefNormal $1 $3 } 
          | pathParam { FieldRefPathParam $1 }
          | authId { FieldRefAuthId }
          | localParam { FieldRefLocalParam }
    
handlerParamsBlock : lbrace handlerParams rbrace { (reverse $2) }

handlerParams : { [] }
              | handlerParams handlerParam semicolon { $2 : $1 }
handlerParam : public { Public }
             | select selectFields from upperId as lowerId 
               joins maybeWhere maybeOrder maybeLimitOffset 
              { Select (SelectQuery $2 ($4,$6) (reverse $7) $8 $9 $10) }
             | replace upperId identified by inputRef with inputJson { Replace $2 $5 (Just $7) } 
             | delete from upperId as lowerId { DeleteFrom $3 $5 Nothing }
             | delete from upperId as lowerId where expr { DeleteFrom $3 $5 (Just $7) }
             | replace upperId identified by inputRef { Replace $2 $5 Nothing }
             | insert upperId from inputJson { Insert $2 (Just $4) }
             | insert upperId { Insert $2 Nothing }
             | defaultfiltersort { DefaultFilterSort }
             | if filter stringval equals localParam then joins where expr { IfFilter ($3 ,(reverse $7) ,$9) }
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

inputRef: lowerId { InputFieldNormal $1 }
        | pathParam { InputFieldPathParam $1 }
        | authId { InputFieldAuthId }
        | value { InputFieldConst $1 }

inputJsonFields : inputJsonField { [$1] }
           | inputJsonFields comma inputJsonField  { $3:$1 }
            
binop : equals { Eq }
      | ne { Ne }
      | lt { Lt }
      | gt { Gt }
      | le { Le }
      | ge { Ge }
      | like { Like }
      | ilike { Ilike }
      

expr : lparen expr rparen and lparen expr rparen { AndExpr $2 $6 }
     | lparen expr rparen or lparen expr rparen { OrExpr $2 $6 }
     | valexpr binop valexpr { BinOpExpr $1 $2 $3 }

valexpr : value { ConstExpr $1 }
        | fieldRef { FieldExpr $1 }
        | valexpr concat valexpr { ConcatExpr $1 $3 }

maybeJoinOn : { Nothing }
            | on fieldRef binop fieldRef { Just ($2,$3,$4) }
            
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
 
field : lowerId maybeMaybe fieldType fieldOptions { Field $2 $1 (NormalField $3 (reverse $4)) } 
      | lowerId maybeMaybe entityId { Field $2 $1 (EntityField $3) }
      | lowerId maybeMaybe upperId { Field $2 $1 (EnumField $3) }

fieldOptions : { [] }
             | fieldOptionsList { $1 }
fieldOptionsList : fieldOption { [$1] }
                 | fieldOptionsList  fieldOption { $2 : $1 }
fieldOption : check lowerId { FieldCheck $2 }
            | default value { FieldDefault $2 }

value : stringval { StringValue $1 }
      | intval { IntValue $1 }
      | floatval { FloatValue $1 }

      
uniques : { [] }
        | uniques uniqueDef semicolon { $2 : $1 }
uniqueDef :  unique upperId fieldIdList { Unique $2 (reverse $3) }

derives : { [] }
        | derives deriveDef semicolon { $2 : $1 }
deriveDef :  deriving upperId  { $2  }

checks : { [] }
        | checks checkDef semicolon { $2 : $1 }
checkDef :  check lowerId { $2  }


fieldIdList : lowerId { [$1] }
            | fieldIdList comma lowerId { $3 : $1 }

fieldType : word32 { FTWord32 }
          | word64{ FTWord64 }
          | int32{ FTInt32 }
          | int64{ FTInt64 }
          | text { FTText }
          | bool{ FTBool }
          | double{ FTDouble }
          | time { FTTime }
          | date { FTDay }
          | utctime{ FTUTCTime }
          | zonedtime{ FTZonedTime }

maybeMaybe : { False }
              | maybe { True }

{
data ParseError = ParseError String deriving (Show, Typeable)
instance Exception ParseError

parseError :: [Token] -> a
parseError (t:ts) = throw (ParseError $ "Parse error : unexpected " ++ show (tokenType t) ++ " at line " ++ show (tokenLineNum t) ++ " col " ++ show (tokenColNum t))
parseError _ = throw (ParseError $ "Parse error : unexpected end of file")

parseModules :: [ImportPath] -> [FilePath] -> IO [(FilePath,Module)]
parseModules handled (path:paths)
    | path `elem` handled = return []
    | otherwise = do
        s <- readFile path
        let mod = (moduleDefs . lexer) s
        catch (do rest <- parseModules (path:handled) (paths ++ modImports mod)
                  return ((path,mod):rest))
              (\(ParseError msg) -> do 
                    hPutStrLn stderr $ path ++ ": " ++ msg
                    exitWith (ExitFailure 1))
parseModules _ [] = return []

parse path = parseModules [] [path]
}
