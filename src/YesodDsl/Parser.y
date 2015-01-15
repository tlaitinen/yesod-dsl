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
    lbracket { Tk _ TLBracket }
    rbracket { Tk _ TRBracket }
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
    pathParam {Tk _ (TPathParam _) }
    idField { Tk _ TId }
    entityId { Tk _ (TEntityId _) }
    localParam { Tk _ TLocalParam }
    true { Tk _ TTrue }
    false { Tk _ TFalse }
    nothing { Tk _ TNothing }
    request { Tk _ TRequest }
    larrow { Tk _ TLArrow }
    rarrow { Tk _ TRArrow }
    doublecolon { Tk _ TDoubleColon }
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
    checkmark { Tk _ TCheckmark }
    checkmarkActive { Tk _ TCheckmarkActive }
    checkmarkInactive { Tk _ TCheckmarkInactive }

%left in 
%left plus minus
%left asterisk slash
%%


dbModule : maybeModuleName 
           pushScope imports defs popScope {%
           do
               path <- getPath
               let m =  Module $1  ((reverse . getEntities) $4) 
                                    ((reverse . getClasses) $4)
                                    ((reverse . getEnums) $4)
                                    ((reverse . getRoutes) $4)
                                    ((reverse . getDefines) $4)
               return $ mergeModules $ (path,m):$3
           }

upperId: upperIdTk { tkString $1 }

maybeModuleName : { Nothing }
    | module upperIdTk semicolon {%
        do
            let s2 = tkString $2
            l2 <- mkLoc $2
            declare l2 "module name" $ SReserved
            return $ Just s2 
    }
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
        declareGlobal l n (SEnum e)
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
        declareGlobal l n (SEntity n)
        return e
    }

routeDef : 
    route 
    pushScope
    pathPiecesDef
    lbrace 
    handlers 
    popScope
    rbrace {% 
        mkLoc $1 >>= \l -> return $ Route l $3 (reverse $5) 
    }

pathPiecesDef: pathPieces {% 
    do
        forM_ (zip [1..] (filter isPathParam $1)) $ \(idx,pp) -> case pp of
            PathId l en -> declare l ("$" ++ show idx) (SEntityId en) 
            _           -> return ()
        return $1
    }
pathPieces : slash pathPiece { [$2] }
           | pathPieces slash pathPiece { $1 ++ [$3]}

pathPiece : lowerIdTk { PathText $ tkString $1 } 
          | hash entityId {%
              do
                  l2 <- mkLoc $2
                  return $ PathId l2 (tkString $2) }

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
        do
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
    | pathParam {%
        do
            l1 <- mkLoc $1
            let i1 = tkInt $1
            withSymbolNow Nothing l1 ("$" ++ show i1) $ requireEntityId $ \_ -> return Nothing

            return $ FieldRefPathParam i1 }
    | auth dot idField { FieldRefAuthId }
    | auth dot lowerIdTk {%
        do 

            l1 <- mkLoc $1
            l3 <- mkLoc $3
            let n3 = tkString $3
            withSymbol l1 "User" $ requireEntityField l3 n3 $ \_ -> return ()
            return $ FieldRefAuth n3 
    }
    | localParam { FieldRefLocalParam }
    | request dot lowerIdTk { FieldRefRequest $ tkString $3 }
    | lowerIdTk {%
        do
            l1 <- mkLoc $1
            let s1 = tkString $1
            withSymbol l1 s1 $ \_ _ _ -> return ()
            return $ FieldRefNamedLocalParam s1
    }
 
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
    selectQueryContent { $2 }

selectQueryContent:
    selectFields 
    from
    declareFromEntity
    joins 
    maybeWhere 
    maybeOrder 
    maybeLimitOffset 
    {%
        do
            let sfs = [ sf | (_,sf) <- $1 ]
            return $ SelectQuery sfs $3 $4 $5 $6 $7
    }

targetEntity: upperIdTk {%
        do
            let s1 = tkString $1
            l1 <- mkLoc $1
            declare l1 "target entity" $ SEntity s1
            return $1
    } 

beginHandler: {% beginHandler }
handlerParamsBlock : lbrace beginHandler handlerParams rbrace {%
        do
            ht <- getCurrentHandlerType
            when (ht == Just GetHandler) $ do
                hasSelect <- hasReserved "select"
                l4 <- mkLoc $4
                when (hasSelect == False) $ 
                    pError l4 "missing select in GET handler"
            return (reverse $3) 
    }

handlerParams : { [] }
     | handlerParams handlerParam semicolon { $2 : $1 }
handlerParam : public {%
        do
            l <- mkLoc $1
            declare l "public" SReserved
            statement l "public"
            return Public 
    }
    | select selectQueryContent {%
        do
            l <- mkLoc $1
            declare l "select" SReserved
            requireHandlerType l "select" (==GetHandler)
            statement l "select"
            return $ Select $2
    }
    | update pushScope targetEntity identified by inputRef maybeWithInputJson popScope {%
        do
            l <- mkLoc $1
            statement l "update"
            requireHandlerType l "update" (/=GetHandler)
            return $ Update (tkString $3) (fst $6) $7
    } 
    | delete pushScope from declareFromEntity maybeWhere popScope {% 
        do
            l <- mkLoc $1
            statement l "delete"
            requireHandlerType l "delete" (/=GetHandler)
            let (en,vn) = $4 
            return $ DeleteFrom en vn $5 
    }
    | bindResult get upperId identified by inputRef {% 
        do
            l <- mkLoc $2
            statement l "get"
            let (l1,s1) = $1
            declare l1 s1 $ SEntity $3
            requireHandlerType l "get" (/=GetHandler)
            return $ GetById $3 (fst $6) s1
    }
    | maybeBindResult insert pushScope targetEntity maybeFromInputJson popScope {%
        do
            l <- mkLoc $2
            statement l "insert"
            let s1 = $1 >>= \(l1,s1') -> return s1'
            let s4 = tkString $4
            case $1 of
                Just (l1,s1) -> declare l1 s1 $ SEntity s4
                Nothing -> return ()
            l4 <- mkLoc $4
            let i = Insert s4 $5 s1 
            withSymbol l4 s4 $ requireEntity $ \e -> validateInsert l e $5
            requireHandlerType l "insert" (/=GetHandler)
            return i
    } 
     | defaultfiltersort {%
        do
            l1 <- mkLoc $1
            statement l1 "default-filter-sort"
            requireHandlerType l1 "default-filter-sort" (==GetHandler)
            return DefaultFilterSort 
    }
    | if param stringval equals localParam then pushScope joins where expr popScope {% 
        do
            l1 <- mkLoc $1
            statement l1 "if-then"
            requireHandlerType l1 "if-then" (==GetHandler)
            return $ IfFilter ($3 ,$8 ,$10, True) 
    }
    | if param stringval equals underscore then pushScope joins where expr popScope {% 
        do
            l1 <- mkLoc $1
            statement l1 "if-then"
            requireHandlerType l1 "if-then" (==GetHandler)
            return $ IfFilter ($3, $8, $10, False) 
    }
    | return outputJson {% 
        do
            l <- mkLoc $1
            lastStatement l "return"
            requireHandlerType l "return" (/=GetHandler)
            return $ Return $2 
    }
    | require pushScope declareFromEntity joins where expr popScope {%
        do
            l <- mkLoc $1
            statement l "require"
            return $ Require (SelectQuery [] $3 $4 (Just $6) [] (0,0)) 
    }
    | for pushScope lowerIdParam in inputRef lbrace handlerParams rbrace popScope {%
        do
            l <- mkLoc $1
            statement l "for"
            requireHandlerType l "for" (/=GetHandler)
            return $ For $3 (fst $5) $7 
    }
    | lowerIdTk inputRefList {% 
        do 
            l <- mkLoc $1
            statement l (tkString $1)
            requireHandlerType l (tkString $1) (/=GetHandler)
            return $ Call (tkString $1) $2
    } 
    | lparen lowerIdTk doublecolon functionType rparen inputRefList {%
        do 
            l2 <- mkLoc $2
            let s2 = tkString $2
            statement l2 s2
            requireHandlerType l2 s2 (/=GetHandler)
            when (length $4 /= length $6) $ pError l2 $ "'" ++ s2 
                ++ "' expects " ++ show (length $4) ++ " parameters, " 
                ++ show (length $6) ++ " given"
            let types = zip $4 $6
            forM_ [ (n,t1,fromJust mt2) | (n,(t1,(_,mt2))) <- zip [1..] types, 
                    Just t1 /= mt2, isJust mt2 ] $ \(n,t1,t2) -> 
                        pError l2 $ "'" ++ s2 ++ "' expects " ++ show t1 ++ " as the parameter #" ++ show n ++ ", got " ++ show t2 ++ " instead."
            return $ Call s2 [ (ifr,Just $ fromMaybe t1 t2) | (t1,(ifr,t2)) <- types ]
    }

functionType: functionTypes rarrow lowerIdTk lparen rparen { $1 }

functionTypes: type { [$1] }
             | functionTypes rarrow type { $1 ++ [$3] }

type: entityId {%
    do
        l1 <- mkLoc $1
        let s1 = tkString $1
        withSymbol l1 s1 $ requireEntity $ \_ -> return ()
        return $ TypeEntityId s1
    }
    | upperIdTk {%
    do
        l1 <- mkLoc $1
        let s1 = tkString $1
        withSymbol l1 s1 $ requireEnum 
        return $ TypeEnum s1
    }
    | lbracket type rbracket { TypeList $2 }
    | fieldTypeContent { TypeField (snd $1) }
    | maybe type { TypeMaybe $2 }

lowerIdParam: lowerIdTk {%
    do
        l1 <- mkLoc $1
        let n1 = tkString $1
        declare l1 n1 SParam
        return $ n1
}

maybeBindResult: { Nothing }
               | bindResult { Just $1 }

bindResult: lowerIdTk larrow {%
    do
        l1 <- mkLoc $1
        return $ (l1, tkString $1) }

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

maybeWithInputJson: with inputJson { Just $2 }
             | { Nothing }
         
maybeFromInputJson: from inputJson { Just (Nothing, $2) }
             | from lowerIdTk inputJson {%
                 do
                     l2 <- mkLoc $2
                     let s2 = tkString $2
                     tgt <- withSymbolNow Nothing l2 "target entity" $ getEntitySymbol
                     withSymbolNow () l2 s2 $ requireEntity $ \e -> when (Just (entityName e) /= tgt) $ pError l2 $ "Reference to " ++ (entityName e) ++ " (expected " ++ (fromMaybe "" tgt) ++ ")"
                     return $ Just (Just s2, $3)
             }
             | { Nothing }
              
inputJson:  lbrace inputJsonFields rbrace { $2 }
inputJsonField : lowerIdTk equals inputRef {%
        do
            l1 <- mkLoc $1
            let s1 = tkString $1
            withSymbol l1 "target entity" $ 
                requireEntityField l1 s1 $ \_ -> return ()
            let (ir, _) = $3
            return (s1, ir) 
     }

inputRefList:  { [] }
            | inputRefList inputRef  { $1 ++ [$2] }

inputRef: request dot lowerIdTk { (InputFieldNormal $ tkString $3, Nothing) }
        | lowerIdTk {%
            do
                l1 <- mkLoc $1
                let n1 = tkString $1
                mtype <- withSymbolNow Nothing l1 n1 $ getSymbolType
                return $ (InputFieldLocalParam n1, mtype) 
        }
        | lowerIdTk dot lowerIdTk {%
            do
                l1 <- mkLoc $1
                l3 <- mkLoc $3
                let (s1,s3) = (tkString $1, tkString $3)
                withSymbol l1 s1 $ requireEntityField l3 s3 $ \_ -> return ()
                return (InputFieldLocalParamField s1 s3, Nothing) 
        }
        | pathParam {%
        do
            l1 <- mkLoc $1
            let i1 = tkInt $1
            mtype <- withSymbolNow Nothing l1 ("$" ++ show i1) $ getSymbolType 
            return $ (InputFieldPathParam i1, mtype) 
         }
        | auth dot idField { (InputFieldAuthId, Just $ TypeEntityId "User") }
        | auth dot lowerIdTk {% 
          do 
                l1 <- mkLoc $1
                l3 <- mkLoc $3
                let n3 = tkString $3
                withSymbol l1 "User" $ requireEntityField l3 n3 $ \_ -> return ()
                return $ (InputFieldAuth n3, Nothing)
           }
        | value { (InputFieldConst $1, fieldValueToType $1) } 
        | now lparen rparen { (InputFieldNow, Just $ TypeField FTUTCTime) }
        | checkmarkValue { (InputFieldCheckmark $1, Just TypeCheckmark) }

checkmarkValue: checkmarkActive { CheckmarkActive }
              | checkmarkInactive { CheckmarkInactive }

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
        | extract lparen lowerIdTk from valexpr rparen {% 
            do
                let s3 = tkString $3
                l3 <- mkLoc $3 
                validateExtractField l3 s3
                return $ ExtractExpr (tkString $3) $5  
        }
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
        withSymbolNow () l n $ requireClass $ \c -> do
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
                declareGlobal l n (SClass c)
                return c
            }



pushScope: {% pushScope }

popScope: {% popScope }

fields : { [] }
              | fields field semicolon { $2 : $1 }
 
field : lowerIdTk maybeMaybe pushScope fieldType fieldOptions fieldFlags popScope {%
        do
            l <- mkLoc $1
            let n = tkString $1
            let f = Field l $2 (FieldInternal `elem` $6) n (NormalField $4 (reverse $5)) 
            declare l n (SField f)
            return f
        } 
      | lowerIdTk maybeMaybe entityId fieldFlags {% 
        do
            l <- mkLoc $1
            let n = tkString $1
            l3 <- mkLoc $3
            let s3 = tkString $3
            let f = Field l $2 (FieldInternal `elem` $4) n (EntityField s3)
            withGlobalSymbol l3 s3 requireEntityOrClass
            declare l n (SField f)
            return f}
      | lowerIdTk maybeMaybe enumFieldContent fieldFlags {%
        do  
            l <- mkLoc $1
            let n = tkString $1
            let f = Field l $2 (FieldInternal `elem` $4) n $3
            declare l n (SField f)
            return f
            }
      | lowerIdTk checkmark maybeDefaultCheckmarkValue fieldFlags {%
        do
            l <- mkLoc $1
            let n = tkString $1
            let f = Field l False (FieldInternal `elem` $4) n (CheckmarkField $3)
            declare l n (SField f)
            return f
            }      
enumFieldContent: 
    upperIdTk default upperIdTk {%
        do
            l1 <- mkLoc $1
            let s1 = tkString $1
            withGlobalSymbol l1 s1 requireEnum
            l3 <- mkLoc $3
            let s3 = tkString $3
            withGlobalSymbol l1 s1 $ requireEnumValue l3 s3
            return $ EnumField s1 (Just s3)
        } 
    | upperIdTk {% 
        do
            l1 <- mkLoc $1
            let s1 = tkString $1
            withGlobalSymbol l1 s1 requireEnum
            return $ EnumField s1 Nothing
    }

maybeDefaultCheckmarkValue: { Nothing }
    | default checkmarkValue { Just $2 }
        

fieldOptions : { [] }
             | pushScope fieldOptionsList popScope { $2 }
fieldOptionsList : fieldOption { [$1] }
                 | fieldOptionsList  fieldOption { $2 : $1 }
fieldOption : check lowerIdTk {%
        do
            l2 <- mkLoc $2
            let s2 = tkString $2
            declare l2 ("check " ++ s2) SReserved
            withSymbolNow () l2 "current field type"  $ requireFieldType $ \ft ->
                addCheck l2 s2 ft
            return $ FieldCheck s2
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
        | check lowerIdList semicolon { reverse $2 }

fieldId: lowerIdTk {%
    do
        l <- mkLoc $1
        let n = tkString $1
        withSymbol l n $ requireField $ \_ -> return ()
        return n
    }

fieldIdList : fieldId { [$1] }
            | fieldIdList comma fieldId { $3 : $1 }

fieldType : fieldTypeContent {%
        do
            let (l,ft) = $1
            declare l "current field type" $ SFieldType ft
            return ft 
    }

fieldTypeContent: 
    word32      {% fieldTypeWithLoc ($1,FTWord32) }
    | word64    {% fieldTypeWithLoc ($1,FTWord64) }
    | int32     {% fieldTypeWithLoc ($1,FTInt32) }
    | int64     {% fieldTypeWithLoc ($1,FTInt64) }
    | text      {% fieldTypeWithLoc ($1,FTText) }
    | bool      {% fieldTypeWithLoc ($1,FTBool) }
    | double    {% fieldTypeWithLoc ($1,FTDouble) }
    | timeofday {% fieldTypeWithLoc ($1,FTTimeOfDay) }
    | day       {% fieldTypeWithLoc ($1,FTDay) }
    | utctime   {% fieldTypeWithLoc ($1,FTUTCTime) }
    | zonedtime {% fieldTypeWithLoc ($1,FTZonedTime) }

maybeMaybe : { False }
              | maybe {True }

{

fieldTypeWithLoc :: (Token,FieldType) -> ParserMonad (Location, FieldType)
fieldTypeWithLoc (tk,ft) = mkLoc tk >>= \l -> return (l,ft)

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
