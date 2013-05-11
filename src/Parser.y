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
    instanceof { Tk _ TInstanceOf }
    mapby { Tk _ TMapBy }
    beforehandler { Tk _ TBeforeHandler }
    afterhandler { Tk _ TAfterHandler }
    selectfrom { Tk _ TSelectFrom }
    join { Tk _ TJoin }
    inner { Tk _ TInner }
    outer { Tk _ TOuter }
    left { Tk _ TLeft }
    right { Tk _ TRight }
    full { Tk _ TFull }
    cross { Tk _ TCross }
    on { Tk _ TOn }
    as { Tk _ TAs }
    defaultfiltersort { Tk _ TDefaultFilterSort }
    textsearchfilter { Tk _ TTextSearchFilter }
    orderby { Tk _ TOrderBy }
    asc { Tk _ TAsc }
    desc { Tk _ TDesc }
    where { Tk _ TWhere }
    deriving { Tk _ TDeriving }
    default  { Tk _ TDefault }
    pathParam { Tk _ (TPathParam $$) }
%%

dbModule : imports defs { Module $1 (getEntities $2) 
                                    (getClasses $2)
                                    (getEnums $2)
                                    (getResources $2)}

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
            rbrace { Entity (mkLoc $1) $2 $4 $5 $6 $7 $8 }

resourceDef : resource pathpieces lbrace handlers rbrace { Resource (mkLoc $1) $2 $4 }
pathpieces : slash pathpiece { [$2] }
           | pathpieces pathpiece { $2 : $1 }

pathpiece : lowerId { PathText $1 } 
          | hash upperId { PathId $2 }

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
    
handlerParamsBlock : lbrace handlerParams rbrace { $2 }

handlerParams : { [] }
              | handlerParams handlerParam semicolon { $2 : $1 }
handlerParam : public { Public }
             | entity upperId { HandlerEntity $2 }
             | selectfrom upperId as lowerId { SelectFrom $2 $4 }
             | jointype upperId as lowerId maybeJoinOn { Join $1 $2 $4 $5 }
             | where expr { Where $2 }
             | beforehandler lowerId { BeforeHandler $2 }
             | afterhandler lowerId { AfterHandler $2 }
             | mapby lowerId { MapBy $2 }
             | defaultfiltersort { DefaultFilterSort }
             | textsearchfilter stringval fieldRefList { TextSearchFilter $2 $3 }
             | orderby sortbylist { OrderBy $2 }
             | return lowerId { ReturnEntity $2 }
             | return lbrace returnfields rbrace { ReturnFields $3 }
             
returnfields : stringval colon fieldRef { [($1, $3)] }
             | returnfields comma stringval colon fieldRef { ($3,$5):$1}
             
binop : equals { Eq }
      | ne { Ne }
      | lt { Lt }
      | gt { Gt }
      | le { Le }
      | ge { Ge }
      | like { Like }

expr : lparen expr rparen and lparen expr rparen { AndExpr $2 $6 }
     | lparen expr rparen or lparen expr rparen { OrExpr $2 $6 }
     | valexpr binop valexpr { BinOpExpr $1 $3 }

valexpr : value { ConstExpr $1 }
        | fieldRef { FieldExpr $1 }

maybeJoinOn : { Nothing }
            | on fieldRef binop fieldRef { Just ($2,$3,$4) }
            
jointype : join { InnerJoin }
         | cross join { CrossJoin } 
         | left outer join { LeftOuterJoin }
         | right outer join { RightOuterJoin }
         | full outer join { FullOuterJoin }
         
sortbylist : sortbylistitem { [$1] }
        | sortbylist sortbylistitem { $2 : $1 }
sortbylistitem : fieldRef sortdir { ($1, $2) }

sortdir : asc { SortAsc }
        | desc  { SortDesc }
              
maybeInstances : { [] }
               | instanceof instances { $2 }

instances : upperId { [$1] }
            | instances comma upperId { $3 : $1 }

classDef : class upperId lbrace
             fields
            uniques
            rbrace { Class (mkLoc $1) $2 $4 $5 }

fields : { [] }
              | fields field semicolon { $2 : $1 }
 
field : lowerId maybeMaybe fieldType fieldOptions { Field $2 $1 (NormalField $3 $4) } 
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
uniqueDef :  unique upperId fieldIdList { Unique $2 $3 }

derives : { [] }
        | derives deriveDef semicolon { $2 : $1 }
deriveDef :  deriving upperId  { $2  }

checks : { [] }
        | checks checkDef semicolon { $2 : $1 }
checkDef :  check lowerId { $2 }

fieldIdList : lowerId { [] }
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
                    putStrLn $ path ++ ": " ++ msg
                    exitWith (ExitFailure 1))
parseModules _ [] = return []

parse path = parseModules [] [path]
}
