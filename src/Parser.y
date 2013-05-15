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
    ne { Tk _ TNe }
    lt { Tk _ TLt }
    gt { Tk _ TGt }
    le { Tk _ TLe }
    ge { Tk _ TGe }
    and { Tk _ TAnd }
    or { Tk _ TOr }
    like  { Tk _ TLike }
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
    date { Tk _ TDate }
    datetime { Tk _ TDateTime }
    zonedtime { Tk _ TZonedTime }
    maybe { Tk _ TMaybe }
    get { Tk _ TGet }
    put { Tk _ TPut }
    post { Tk _ TPost }
    delete { Tk _ TDelete }
    public { Tk _ TPublic }
    return { Tk _ TReturn }
    instance { Tk _ TInstance }
    of { Tk _ TOf }
    beforehandler { Tk _ TBeforeHandler }
    afterhandler { Tk _ TAfterHandler }
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
    textsearchfilter { Tk _ TTextSearchFilter }
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
    entityId { Tk _ (TEntityId $$) }
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

fieldRef : lowerId { FieldRefId $1 }
          | lowerId dot lowerId { FieldRefNormal $1 $3 } 
          | pathParam { FieldRefPathParam $1 }
          | authId { FieldRefAuthId }
    
handlerParamsBlock : lbrace handlerParams rbrace { (reverse $2) }

handlerParams : { [] }
              | handlerParams handlerParam semicolon { $2 : $1 }
handlerParam : public { Public }
             | select from upperId as lowerId { SelectFrom $3 $5 }
             | replace upperId identified by inputRef with inputJson { Replace $2 $5 (Just $7) } 
             | delete from upperId as lowerId { DeleteFrom $3 $5 Nothing }
             | delete from upperId as lowerId where expr { DeleteFrom $3 $5 (Just $7) }
             | replace upperId identified by inputRef { Replace $2 $5 Nothing }
             | insert upperId from inputJson { Insert $2 (Just $4) }
             | insert upperId { Insert $2 Nothing }
             | jointype upperId as lowerId maybeJoinOn { Join $1 $2 $4 $5 }
             | where expr { Where $2 }
             | defaultfiltersort { DefaultFilterSort }
             | textsearchfilter stringval fieldRefList { TextSearchFilter $2 (reverse $3) }
             | order by sortbylist { OrderBy (reverse $3) }
             | return lowerId { ReturnEntity $2 }
             | return lbrace outputJsonFields rbrace { ReturnFields (reverse $3) }
             
inputJson:  lbrace inputJsonFields rbrace { $2 }
inputJsonField : lowerId equals inputRef { ($1, $3) }

inputRef: lowerId { InputFieldNormal $1 }
        | pathParam { InputFieldPathParam $1 }
        | authId { InputFieldAuthId }
        | value { InputFieldConst $1 }

inputJsonFields : inputJsonField { [$1] }
           | inputJsonFields comma inputJsonField  { $3:$1 }
 
outputJsonField : stringval colon fieldRef { ($1, $3) }

outputJsonFields : outputJsonField { [$1] }
           | outputJsonFields comma outputJsonField  { $3:$1 }
             
binop : equals { Eq }
      | ne { Ne }
      | lt { Lt }
      | gt { Gt }
      | le { Le }
      | ge { Ge }
      | like { Like }

expr : lparen expr rparen and lparen expr rparen { AndExpr $2 $6 }
     | lparen expr rparen or lparen expr rparen { OrExpr $2 $6 }
     | valexpr binop valexpr { BinOpExpr $1 $2 $3 }

valexpr : value { ConstExpr $1 }
        | fieldRef { FieldExpr $1 }

maybeJoinOn : { Nothing }
            | on fieldRef binop fieldRef { Just ($2,$3,$4) }
            
jointype : inner join { InnerJoin }
         | cross join { CrossJoin } 
         | left outer join { LeftOuterJoin }
         | right outer join { RightOuterJoin }
         | full outer join { FullOuterJoin }
         
sortbylist : sortbylistitem { [$1] }
        | sortbylist comma sortbylistitem { $3 : $1 }
sortbylistitem : fieldRef sortdir { ($1, $2) }

sortdir : asc { SortAsc }
        | desc  { SortDesc }
              
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
      | lowerId maybeMaybe upperId { Field $2 $1 (EntityField $3) }

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
          | date { FTDate }
          | datetime{ FTDateTime }
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
