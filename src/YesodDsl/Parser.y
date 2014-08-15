{
module YesodDsl.Parser (parse) where
import YesodDsl.ParserState
import YesodDsl.Lexer
import YesodDsl.AST
import YesodDsl.ModuleMerger
import YesodDsl.ClassImplementer
import YesodDsl.ExpandMacros
import Control.Monad.Trans.Class
import System.IO
import Data.Maybe
import Data.Typeable
import Prelude hiding (catch) 
import Control.Exception hiding (Handler)
import System.Exit
import Control.Monad.IO.Class
import Control.Monad
import Data.List

}

%name parseModuleDefs
%tokentype { Token }
%error { parseError }
%monad { ParserMonad }

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
    lowerIdTk    { Tk _ (TLowerId _) }
    upperIdTk    { Tk _ (TUpperId _)  }
    intval        { Tk _ (TInt $$)  }
    floatval      { Tk _ (TFloat $$) }
    semicolon  { Tk _ TSemicolon }
    hash        { Tk _ THash }
    equals { Tk _ TEquals }
    concatop { Tk _ TConcatOp }
    ne { Tk _ TNe }
    lt { Tk _ TLt }
    gt { Tk _ TGt }
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
    plus { Tk _ TPlus }
    minus { Tk _ TMinus }
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
    update {Tk _ TUpdate }
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
    pathParam {Tk _ (TPathParam $$) }
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
    define { Tk _ TDefine }
    for { Tk _ TFor }
    extract { Tk _ TExtract }
    concat { Tk _ TConcat }
    random { Tk _ TRandom }
    floor { Tk _ TFloor }
    ceiling { Tk _ TCeiling }
    exists { Tk _ TExists }
%left in 
%left plus minus
%left asterisk slash
%%


dbModule : maybeModuleName 
           imports defs {%
           do
               path <- getPath
               let m =  Module $1  ((reverse . getEntities) $3) 
                                    ((reverse . getClasses) $3)
                                    ((reverse . getEnums) $3)
                                    ((reverse . getRoutes) $3)
                                    ((reverse . getDefines) $3)
               return $ mergeModules $ (path,m):$2 
           }

lowerId: lowerIdTk { tkString $1 }
upperId: upperIdTk { tkString $1 }

maybeModuleName : { Nothing }
                | module upperId semicolon { Just $2 }
imports : { [] }
        | importStmt imports { $1++ $2 }

importStmt : import stringval semicolon {%  
            do
                parsed <- getParsed
                if not ($2 `elem` parsed)
                    then do
                        ps <- getParserState
                        (m,ps') <- liftIO $parseModule ps $2
                        setParserState ps'
                        return [($2,m)]
                    else return []
            }
defs : { [] }
       | defs def  { $2 : $1 }
def : routeDef     { RouteDef $1 }
      | entityDef      { EntityDef $1 } 
      | classDef      { ClassDef $1 }
      | enumDef       { EnumDef $1 }
      | defineDef     { DefineDef $1 }

defineDef : define lowerIdTk pushScope lparen maybeEmptyParamList rparen equals defineContent popScope semicolon {% 
    do
        l <- mkLoc $2 
        let n = tkString $2
        let d = Define n l $5 $8
        declare l n (SDefine d) 
        return d
    } 

maybeEmptyParamList: { [] }
         | paramList { $1 } 

paramList : paramDef { [$1] }
          | paramList comma paramDef { $1 ++ [$3] }

paramDef: lowerIdTk {%
    do
        l <- mkLoc $1
        let n = tkString $1
        declare l n SParam
        return n
    } 

maybeEmptyLowerIdList : { [] }
                      | lowerIdList { (reverse $1) }

lowerIdList : lowerIdTk { [tkString $1] }
            | lowerIdList comma lowerIdTk { (tkString $3) : $1 }

defineContent : selectQuery
               { DefineSubQuery $1 }
      
enumDef : enum upperIdTk equals pushScope enumValues popScope semicolon 
         {% 
    do
        l <- mkLoc $2
        let n = tkString $2
        let e = EnumType l n $5
        declare l n (SEnum e)
        forM_ (nub $ enumValues e) $ \ev -> declare l (n ++ ev) SReserved
        return e
    }

uniqueUpperIdTk: upperIdTk {%
    do
        l <- mkLoc $1
        declare l (tkString $1) SReserved
        return $1 
    }

uniqueUpperId: uniqueUpperIdTk { tkString $1 }
          
enumValues : uniqueUpperId { [$1] }
          | enumValues pipe uniqueUpperId { $3 : $1 }
    
entityDef : entity upperIdTk lbrace 
            pushScope
            maybeInstances
            fields
            uniques
            maybeDeriving
            checks
            popScope
            rbrace {% 
    do
        l <- mkLoc $2
        let n = tkString $2
        let e = Entity l n $5 (reverse $6) [] (reverse $7) $8 (reverse $9) 
        declare l n (SEntity n)
        return e
    }

routeDef : 
    route 
    pathPieces 
    lbrace 
    pushScope
    handlers 
    popScope
    rbrace {% 
        mkLoc $1 >>= \l -> return $ Route l (reverse $2) (reverse $5) 
    }

pathPieces : slash pathPiece { [$2] }
           | pathPieces slash pathPiece { $3 : $1 }

pathPiece : lowerIdTk { PathText $ tkString $1 } 
          | hash entityId { PathId $2 }

handlers : handlerdef  { [$1] }
         | handlers handlerdef { $2 : $1 }

handlerType: 
    get {% 
        do
             l <- mkLoc $1 
             declare l "get handler" SReserved
             setCurrentHandlerType GetHandler
             return $ Handler l GetHandler 
    } 
    | put {%
        do 
             l <- mkLoc $1
             declare l "put handler" SReserved
             setCurrentHandlerType PutHandler
             return $ Handler l PutHandler
    } 
    | post {%
        do
            l <- mkLoc $1
            declare l "post handler" SReserved
            setCurrentHandlerType PostHandler
            return $ Handler l PostHandler
    } 
    | delete {%
        do 
            l <- mkLoc $1
            declare l "delete handler" SReserved
            setCurrentHandlerType DeleteHandler
            return $ Handler l DeleteHandler
    }

handlerdef : handlerType pushScope handlerParamsBlock popScope {% 
        return $ $1 $3 
    }

fieldRef : 
    lowerIdTk dot idField {%
        do
            l1 <- mkLoc $1
            let s1 = tkString $1
            withSymbol l1 s1 $ requireEntity $ \_ -> return ()
            return $ FieldRefId s1
    }
    | lowerIdTk dot lowerIdTk {% 
        do 
            l1 <- mkLoc $1
            let s1 = tkString $1
            l3 <- mkLoc $3
            let s3 = tkString $3
            withSymbol l1 s1 $ requireEntityField l3 s3 $ \_ -> return () 
            return $ FieldRefNormal s1 s3
    }
    | upperIdTk dot upperIdTk {%
        do
            l1 <- mkLoc $1
            let s1 = tkString $1
            l3 <- mkLoc $3
            let s3 = tkString $3
            withSymbol l1 s1 $ requireEnumValue l3 s3
            return $ FieldRefEnum s1 s3
    }
    | lowerIdTk dot lbrace lowerIdTk rbrace {%
        do
            l1 <- mkLoc $1
            let s1 = tkString $1
            l4 <- mkLoc $4
            let s4 = tkString $4
            withSymbol l1 s1 $ requireEntity $ \_ -> return ()
            withSymbol l4 s4 $ requireParam
            return $ FieldRefParamField s1 s4 
    }
          | pathParam { FieldRefPathParam $1 }
          | auth dot idField { FieldRefAuthId }
          | auth dot lowerIdTk { FieldRefAuth $ tkString $3 }
          | localParam { FieldRefLocalParam }
          | request dot lowerIdTk { FieldRefRequest $ tkString $3 }
          | lowerIdTk { FieldRefNamedLocalParam (tkString $1) }
 
declareFromEntity: upperIdTk as lowerIdTk {%
        do
            l1 <- mkLoc $1
            l3 <- mkLoc $3
            let (s1,s3) = (tkString $1, tkString $3)
            declare l3 s3 (SEntity s1)
            withSymbol l1 s1 $ requireEntity $ \_ -> return ()
            return (s1,s3)
    }
selectQuery:
    select 
    selectFields 
    from
    declareFromEntity
    joins 
    maybeWhere 
    maybeOrder 
    maybeLimitOffset 
    {%
        do
            let sfs = [ sf | (_,sf) <- $2 ]
            return $ SelectQuery sfs $4 $5 $6 $7 $8
    }
 

handlerParamsBlock : lbrace handlerParams rbrace { (reverse $2) }

handlerParams : { [] }
              | handlerParams handlerParam semicolon { $2 : $1 }
handlerParam : public { Public }
             | selectQuery
                         { Select $1 }
             | update upperId identified by inputRef with inputJson { Update $2 $5 (Just $7) } 
             | delete pushScope from declareFromEntity maybeWhere popScope { let (en,vn) = $4 in DeleteFrom en vn $5 }
             | update upperId identified by inputRef { Update $2 $5 Nothing }
             | bindResult get upperId identified by inputRef { GetById $3 $6 $1 }
             | maybeBindResult insert upperId from inputJson { Insert $3 (Just $5) $1 }
             | maybeBindResult insert upperId { Insert $3 Nothing $1 }
             | defaultfiltersort { DefaultFilterSort }
             | if param stringval equals localParam then pushScope joins where expr popScope { IfFilter ($3 ,$8 ,$10, True) }
             | if param stringval equals underscore then pushScope joins where expr popScope { IfFilter ($3, $8, $10, False) }
             | return outputJson { Return $2 }
             | require pushScope declareFromEntity joins where expr popScope { Require (SelectQuery [] $3 $4 (Just $6) [] (0,0)) }
             | for pushScope lowerIdTk in inputRef lbrace handlerParams rbrace popScope { For (tkString $3) $5 $7 }
             | lowerIdTk  inputRefList  { Call (tkString $1) (reverse $2) }

maybeBindResult: { Nothing }
               | bindResult { Just $1 }

bindResult: lowerIdTk larrow { tkString $1 }

selectFields: selectField moreSelectFields { $1 : (reverse $2) }
           | { [] }

moreSelectFields: { [] }
                | moreSelectFields comma selectField { $3 : $1 }

selectField: lowerIdTk dot asterisk {%
        do
            l1 <- mkLoc $1
            return (l1, SelectAllFields $ tkString $1)
    }
    | lowerIdTk dot idField maybeSelectAlias {% 
        do
            l1 <- mkLoc $1
            return (l1, SelectIdField (tkString $1) $4) 
    }
    | lowerIdTk dot lowerIdTk maybeSelectAlias {%
        do
            l1 <- mkLoc $1
            return (l1, SelectField (tkString $1) (tkString $3) $4) 
    }
    | lowerIdTk dot lbrace lowerIdTk rbrace maybeSelectAlias {% 
        do
            l1 <- mkLoc $1
            return (l1, SelectParamField (tkString $1) (tkString $4) $6)
    }
    | valexpr as lowerIdTk {% 
        do
            l3 <- mkLoc $3
            return (l3, SelectValExpr $1 (tkString $3) )
    }
           
       
maybeSelectAlias: { Nothing }
                | as lowerIdTk { Just $ tkString $2 }
joins : { [] }
      | jointype declareFromEntity maybeJoinOn joins
        {% 
            do
                let (en,vn) = $2
                return $ (Join $1 en vn $3):$4
        }

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
        |desc  { SortDesc }
 
inputJson:  lbrace inputJsonFields rbrace { $2 }
inputJsonField : lowerIdTk equals inputRef { (tkString $1, $3) }

inputRefList:  { [] }
            | inputRefList inputRef  { $2 : $1 }

inputRef: request dot lowerIdTk { InputFieldNormal $ tkString $3 }
        | lowerIdTk { InputFieldLocalParam (tkString $1) }
        | lowerIdTk dot lowerIdTk { InputFieldLocalParamField (tkString $1) (tkString $3) }
        | pathParam { InputFieldPathParam $1 }
        | auth dot idField { InputFieldAuthId }
        | auth dot lowerIdTk { InputFieldAuth $ tkString $3 }
        | value { InputFieldConst $1 }
        | now lparen rparen { InputFieldNow }

inputJsonFields : inputJsonField { [$1] }
           | inputJsonFields comma inputJsonField  { $3:$1 }

outputJson: lbrace maybeOutputJsonFields rbrace { $2 }

maybeOutputJsonFields: { [] }
                     | outputJsonFields { $1 }
outputJsonFields: outputJsonField { [ $1 ] }
                | outputJsonFields comma outputJsonField { $3:$1 }

outputJsonField : lowerIdTk equals outputRef { (tkString $1,$3) }

outputRef: lowerIdTk { OutputFieldLocalParam $ tkString $1 }

binop : equals { Eq }
      | ne { Ne }
      | lt { Lt }
      | gt { Gt }
      | le { Le }
      | ge { Ge }
      | like { Like }
      | ilike {Ilike }
      | is { Is }
      | in { In }
      | not in { NotIn }
expr : expr and expr { AndExpr $1 $3 }
     | expr or expr { OrExpr $1 $3 }
     | not expr { NotExpr $2 }
     | lparen expr rparen { $2 } 
     | valexpr binop valexpr { BinOpExpr $1 $2 $3 }
     | exists lparen pushScope selectQuery popScope rparen
                   { ExistsExpr $4 }
 

valbinop :      
      slash { Div }
      | asterisk { Mul } 
      | plus { Add }
      | minus { Sub }
      | concatop { Concat }

valexpr : lparen valexpr rparen { $2 }
        | value { ConstExpr $1 }
        | fieldRef { FieldExpr $1 }
        | valexpr valbinop valexpr { ValBinOpExpr $1 $2 $3 }
        | concat lparen valexprlist rparen { ConcatManyExpr (reverse $3) }
        | random lparen rparen { RandomExpr }
        | floor lparen valexpr rparen { FloorExpr $3 }
        | ceiling lparen valexpr rparen { CeilingExpr $3 }
        | extract lparen lowerIdTk from valexpr rparen { 
                ExtractExpr (tkString $3) $5  }
        | lparen pushScope selectQuery popScope rparen
                   { SubQueryExpr $3 }
        | lowerIdTk lparen maybeEmptyLowerIdList rparen { ApplyExpr (tkString $1) $3 }

valexprlist: valexpr { [$1] }        
           | valexprlist comma valexpr { $3 : $1 }

maybeJoinOn : { Nothing }
            | on expr { Just $2 }
            
jointype : inner join { InnerJoin }
         | cross join { CrossJoin } 
         | left outer join { LeftOuterJoin }
         | right outer join { RightOuterJoin }
         | full outer join { FullOuterJoin }
         
             
maybeInstances : { [] }
               | instance of instances semicolon { (reverse $3) }

instanceDef: uniqueUpperIdTk {%
    do
        l <- mkLoc $1
        let n = tkString $1
        withSymbolNow l n $ requireClass $ \c -> do
            forM_ (classFields c) $ \f -> do
                declare (fieldLoc f) (fieldName f) (SField f)
        return n
    }
instances : instanceDef { [$1] }
            | instances comma instanceDef { $3 : $1 }

classDef : class upperIdTk lbrace
            pushScope
            fields
            uniques
            popScope
            rbrace {% 
            do
                l <- mkLoc $2
                let n = tkString $2
                let c = Class l n (reverse $5) (reverse $6)  
                declare l n (SClass c)
                return c
            }



pushScope: {% pushScope }

popScope: {% popScope }

fields : { [] }
              | fields field semicolon { $2 : $1 }
 
field : lowerIdTk maybeMaybe fieldType fieldOptions fieldFlags {%
        do
            l <- mkLoc $1
            let n = tkString $1
            let f = Field l $2 (FieldInternal `elem` $5) n (NormalField $3 (reverse $4)) 
            declare l n (SField f)
            return f
        } 
      | lowerIdTk maybeMaybe entityId fieldFlags {% 
        do
            l <- mkLoc $1
            let n = tkString $1
            let f = Field l $2 (FieldInternal `elem` $4) n (EntityField $3) 
            declare l n (SField f)
            return f}
      | lowerIdTk maybeMaybe upperId fieldFlags {%
        do  
            l <- mkLoc $1
            let n = tkString $1
            let f = Field l $2 (FieldInternal `elem` $4) n (EnumField $3) 
            declare l n (SField f)
            return f
            }

fieldOptions : { [] }
             | pushScope fieldOptionsList popScope { $2 }
fieldOptionsList : fieldOption { [$1] }
                 | fieldOptionsList  fieldOption { $2 : $1 }
fieldOption : check lowerId {%
        do
            l <- mkLoc $1
            declare l ("check " ++ $2) SReserved
            return $ FieldCheck $2 
    }
            | default value {%
        do
            l <- mkLoc $1
            declare l "default value" SReserved
            return $ FieldDefault $2 
    }

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
uniqueDef :  unique uniqueUpperId fieldIdList { Unique $2 (reverse $3) }

maybeDeriving : { [] }
             | deriving derives semicolon { (reverse $2) }
derives : upperId { [$1] }
        | derives comma upperId { $3 : $1 }

checks : { [] }
        | check fieldIdList semicolon { reverse $2 }

fieldId: lowerIdTk {%
    do
        l <- mkLoc $1
        let n = tkString $1
        withSymbol l n $ requireField $ \_ -> return ()
        return n
    }

fieldIdList : fieldId { [$1] }
            | fieldIdList comma fieldId { $3 : $1 }

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
              | maybe {True }

{

data ModDef = EntityDef Entity
           | ClassDef Class
           | EnumDef EnumType
           | RouteDef Route
           | DefineDef Define
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

getDefines :: [ModDef] -> [Define]
getDefines defs = mapMaybe (\d -> case d of (DefineDef e) -> Just e; _ -> Nothing) defs


data ParseError = ParseError String deriving (Show, Typeable)
instance Exception ParseError

parseError :: [Token] -> a
parseError (t:ts) = throw (ParseError $ "Parse error : unexpected " ++ show (tokenType t) ++ " at line " ++ show (tokenLineNum t) ++ " col " ++ show (tokenColNum t))
parseError _ = throw (ParseError $ "Parse error : unexpected end of file")

parseModule :: ParserState -> FilePath -> IO (Module,ParserState)
parseModule ps path = catch 
        (do
            s <- readFile path
            (m,ps') <- runParser path ps (parseModuleDefs $! lexer s)
            return $! (m,ps'))
        (\(ParseError msg) -> do 
            hPutStrLn stderr $ path ++ ": " ++ msg
            exitWith (ExitFailure 1))
       
parse path = do
    (m,ps) <- parseModule initParserState path
    let ast = expandMacros $ implementClasses m
    errors <- postValidation ast ps
    if errors == 0
        then return $ Just ast
        else do
            hPutStrLn stderr $ show errors ++ " errors"
            return Nothing
}
