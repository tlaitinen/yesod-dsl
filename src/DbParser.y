{
module DbParser (parse) where
import DbLexer
import DbTypes
import ModuleMerger
import System.IO
import Data.Maybe
import Data.Typeable
import Prelude hiding (catch) 
import Control.Exception
import System.Exit
}

%name dbdef
%tokentype { Token }
%error { parseError }

%token
    import     { Tk _ TImport }
    enum       { Tk _ TEnum }
    pipe       { Tk _ TPipe }
    entity   { Tk _ TEntity }
    class      { Tk _ TClass }
    unique     { Tk _ TUnique }
    check      { Tk _ TCheck }
    lowerId    { Tk _ (TLowerId $$) }
    upperId    { Tk _ (TUpperId $$)  }
    intval        { Tk _ (TInt $$)  }
    floatval      { Tk _ (TFloat $$) }
    semicolon  { Tk _ TSemicolon }
    equals { Tk _ TEquals }
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
    pretransform { Tk _ TPreTransform }
    posttransform { Tk _ TPostTransform }
    prehook { Tk _ TPreHook }
    posthook { Tk _ TPostHook }
    join { Tk _ TJoin }
    on { Tk _ TOn }
    validate { Tk _ TValidate }
    defaultfiltersort { Tk _ TDefaultFilterSort }
    textsearchfilter { Tk _ TTextSearchFilter }
    sortby { Tk _ TSortBy }
    asc { Tk _ TAsc }
    desc { Tk _ TDesc }
    filter { Tk _ TFilter }
    selectopts {  Tk _ TSelectOpts }
    deriving { Tk _ TDeriving }
    default  { Tk _ TDefault }
%%

dbModule : imports dbDefs { DbModule $1 (getEntities $2) 
                                        (getClasses $2)
                                        (getEnums $2)}

imports : { [] }
        | imports importStmt { $2 : $1 }

importStmt : import stringval semicolon { $2 }
dbDefs : {- empty -}   { [] }
       | dbDefs dbDef  { $2 : $1 }
dbDef : entityDef      { EntityDef $1 } 
      | classDef      { ClassDef $1 }
      | enumDef       { EnumDef $1 }

enumDef : enum upperId equals enumValues semicolon 
         { DbEnum (mkLoc $1) $2 $4 }

enumValues : upperId { [$1] }
           | enumValues pipe upperId { $3 : $1 }
    

entityDef : entity upperId maybeImplementations lbrace 
            fields
            uniques
            derives
            checks
            services
            rbrace { Entity (mkLoc $1) $2 $3 $5 $6 $7 $8 $9 }

services : { [] }
         | services servicedef { $2 : $1 }

servicedef : get serviceParamsBlock { Service GetService $2 }
         | get slash lowerId joins serviceParamsBlock { Service (GetServiceNested $3 $4) $5 }
         | put serviceParamsBlock { Service PutService $2 }
         | post serviceParamsBlock { Service PostService $2 }
         | delete serviceParamsBlock { Service DeleteService $2 }
         | validate serviceParamsBlock { Service ValidateService $2 }

joins : joinitem { [$1] }
      | joins joinitem { $2 : $1 }

joinitem : join upperId on fieldPath equals fieldPath { Join $2 $4 $6 }

fieldPath : upperId { FieldPathId $1 }
          | upperId dot lowerId { FieldPathNormal $1 $3 } 
    
serviceParamsBlock : lbrace serviceParams rbrace { $2 }

serviceParams : { [] }
              | serviceParams serviceParam semicolon { $2 : $1 }
serviceParam : public { PublicService }
             | prehook lowerId { ServicePreHook $2 }
             | posthook lowerId { ServicePostHook $2 }
             | pretransform lowerId { ServicePreTransform $2 }
             | posttransform lowerId { ServicePostTransform $2 }
             | defaultfiltersort { ServiceDefaultFilterSort }
             | textsearchfilter stringval fieldIdList { ServiceTextSearchFilter $2 $3 }
             | sortby sortbylist { ServiceSortBy $2 }
             | filter lowerId { ServiceFilter $2 }
             | selectopts lowerId { ServiceSelectOpts $2 }
             
sortbylist : sortbylistitem { [$1] }
        | sortbylist sortbylistitem { $2 : $1 }
sortbylistitem : lowerId sortdir { ($1, $2) }
sortdir : asc { SortAsc }
        | desc  { SortDesc }
              
maybeImplementations : { [] }
                     | colon implementations { $2 }

implementations : upperId { [$1] }
            | implementations comma upperId { $3 : $1 }

classDef : class upperId lbrace
             fields
            uniques
            rbrace { Class (mkLoc $1) $2 $4 $5 }

fields : { [] }
              | fields field semicolon { $2 : $1 }
 
field : lowerId maybeMaybe fieldType fieldOptions { Field $2 $1 (NormalField (tokenType $3) $4) } 
      | lowerId maybeMaybe upperId { Field $2 $1 (EntityField $3) }

fieldOptions : { [] }
             | fieldOptionsList { $1 }
fieldOptionsList : fieldOption { [$1] }
                 | fieldOptionsList  fieldOption { $2 : $1 }
fieldOption : check lowerId { FieldCheck $2 }
            | default stringval { FieldDefault $2 }

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


fieldIdList : { [] }
            | lowerId fieldIdList { $1 : $2 }

fieldType : word32 { $1 }
          | word64{ $1 }
          | int32{ $1 }
          | int64{ $1 }
          | text { $1 }
          | bool{ $1 }
          | double{ $1 }
          | time { $1 }
          | date { $1 }
          | datetime{ $1 }
          | zonedtime{ $1 }

maybeMaybe : { False }
              | maybe { True }

{
data ParseError = ParseError String deriving (Show, Typeable)
instance Exception ParseError

parseError :: [Token] -> a
parseError (t:ts) = throw (ParseError $ "Parse error : unexpected " ++ show (tokenType t) ++ " at line " ++ show (tokenLineNum t) ++ " col " ++ show (tokenColNum t))
parseError _ = throw (ParseError $ "Parse error : unexpected end of file")

parseModules :: [ImportPath] -> [FilePath] -> IO [(FilePath,DbModule)]
parseModules handled (path:paths)
    | path `elem` handled = return []
    | otherwise = do
        s <- readFile path
        let mod = (dbdef . lexer) s
        catch (do rest <- parseModules (path:handled) (paths ++ dbImports mod)
                  return ((path,mod):rest))
              (\(ParseError msg) -> do 
                    putStrLn $ path ++ ": " ++ msg
                    exitWith (ExitFailure 1))
parseModules _ [] = return []

parse path = parseModules [] [path]
}
