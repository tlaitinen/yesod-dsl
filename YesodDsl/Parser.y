{
module YesodDsl.Parser (parse) where
import YesodDsl.ParserState
import YesodDsl.Lexer
import YesodDsl.AST
import YesodDsl.ModuleMerger
import YesodDsl.ClassImplementer
import YesodDsl.Simplify
import Control.Monad.Trans.Class
import System.IO
import Data.Maybe
import Data.Typeable
import Data.Either
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
    verbatim { Tk _ (TVerbatim _) }
    word32   { Tk _ TWord32 }
    word64   { Tk _ TWord64 }
    int32    { Tk _ TInt32 }
    int { Tk _ TIntType }
    int64    { Tk _ TInt64 }
    text { Tk _ TText }
    bool { Tk _ TBool }
    double { Tk _ TDouble }
    timeofday { Tk _ TTimeOfDay }
    day { Tk _ TDay }
    utctime { Tk _ TUTCTime }
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
    entityId { Tk _ (TEntityId _) }
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
                                    (rights $3)
               return $ mergeModules $ (path,m):lefts $3
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
        | import importContent semicolon imports { $2++ $4 }

importContent : stringval {%  
    do
        parsed <- getParsed
        if not ($1 `elem` parsed)
            then do
                ps <- getParserState
                (m,ps') <- liftIO $ parseModule ps $1
                setParserState ps'
                return [Left ($1,m)]
            else return []
   } | moduleName lparen lowerIdList rparen {%
   do   
       let (ns,l) = $1
           n = intercalate "." ns
       forM_ $3 $ \f -> do
           declareGlobal l f SFunction
       return [Right $ Import n $3] 
   }

moduleName: upperIdTk moduleNames {%
    do
        l <- mkLoc $1
        return (tkString $1 : $2, l)
    }
moduleNames: { [] }
    | moduleNames dot upperId moduleNames { $1 ++ [$3] }
                
defs : { [] }
       | defs def  { $2 : $1 }
def : routeDef     { RouteDef $1 }
      | entityDef      { EntityDef $1 } 
      | classDef      { ClassDef $1 }
      | enumDef       { EnumDef $1 }

lowerIdList : lowerIdTk { [tkString $1] }
            | lowerIdList comma lowerIdTk { (tkString $3) : $1 }

     
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
fieldRefList : { [ ] }
             | fieldRefList fieldRef { $1 ++ [$2] }
fieldRef : 
    value { Const $1 }
    | now lparen rparen { Now }
    | lowerIdTk dot lowerIdTk {% 
        do 
            l1 <- mkLoc $1
            let s1 = tkString $1
            l3 <- mkLoc $3
            let s3 = tkString $3
                a = SqlField (Var s1 (Left "") False) s3
                b = LocalParamField (Var s1 (Left "") False) s3
            if s3 == "id"
                then do
                    withSymbol l1 s1 $ requireEntity $ \_ -> return ()
                    return $ SqlId (Var s1 (Left "") False)
                else do
                    withSymbol l1 s1 $ requireEntityFieldSelectedOrResult l3 s3
                    m <- symbolMatches s1 $ \st -> case st of SEntityResult _ -> True; _ -> False
                    return $ if m then b else a
    }
    | lowerIdTk dot upperIdTk dot lowerIdTk {%
        do
           l1 <- mkLoc $1
           let s1 = tkString $1
           l3 <- mkLoc $3
           let s3 = tkString $3
           l5 <- mkLoc $5
           let s5 = tkString $5
           -- TODO: validation
           if s5 == "id"
               then return $ SqlId (Var (s1 ++ "_" ++ s3) (Left "") False)
               else return $ SqlField (Var (s1 ++ "_" ++ s3) (Left "") False) s5
    }
    | upperIdTk dot upperIdTk {%
        do
            l1 <- mkLoc $1
            let s1 = tkString $1
            l3 <- mkLoc $3
            let s3 = tkString $3
            withSymbol l1 s1 $ requireEnumValue l3 s3
            return $ EnumValueRef s1 s3
    }
    | pathParam {%
        do
            l1 <- mkLoc $1
            let i1 = tkInt $1
            withSymbolNow Nothing l1 ("$" ++ show i1) $ requireEntityId $ \_ -> return Nothing

            return $ PathParam i1 }
    | auth dot lowerIdTk {%
        do 

            l1 <- mkLoc $1
            l3 <- mkLoc $3
            let n3 = tkString $3
            if n3 == "id"
                then return AuthId
                else do
                    withSymbol l1 "User" $ requireEntityField l3 n3 $ \_ -> return ()
                    return $ AuthField n3 
    }
    | localParam { LocalParam }
    | request dot lowerIdTk { RequestField (tkString $3) }
    | lowerIdTk {%
        do
            l1 <- mkLoc $1
            let s1 = tkString $1
            withSymbol l1 s1 $ \_ _ _ -> return ()
            return $ NamedLocalParam s1
    }

declareFromEntity: upperIdTk as lowerIdTk {%
        do
            l1 <- mkLoc $1
            l3 <- mkLoc $3
            let (s1,s3) = (tkString $1, tkString $3)
            declare l3 s3 (SEntity s1)

            withSymbol l1 s1 $ requireEntityOrClass
            return (Left s1,s3)
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
    | default for request dot lowerIdTk value  {%
        do
            l5 <- mkLoc $5
            let t5 = tkString $5
            declare l5 (concat ["default for param \"", t5, "\""]) SReserved
            return $ ParamDefault t5 $6 
        }
    | select selectQueryContent {%
        do
            l <- mkLoc $1
            declare l "select" SReserved
            requireHandlerType l "select" (==GetHandler)
            statement l "select"
            return $ Select $2
    }
    | update pushScope targetEntity identified by fieldRef maybeWithJson popScope {%
        do
            l <- mkLoc $1
            statement l "update"
            requireHandlerType l "update" (/=GetHandler)
            l3 <- mkLoc $3
            let s3 = tkString $3
            withSymbol l3 s3 $ requireEntity $ \_ -> return ()
            return $ Update (Left $ s3) $6 $7
    } 
    | delete pushScope from declareFromEntity maybeWhere popScope {% 
        do
            l <- mkLoc $1
            statement l "delete"
            requireHandlerType l "delete" (/=GetHandler)
            let (en,vn) = $4 
            return $ DeleteFrom en vn $5 
    }
    | bindResult get upperId identified by fieldRef {% 
        do
            l <- mkLoc $2
            statement l "get"
            let (l1,s1) = $1
            declare l1 s1 $ SEntityResult $3
            requireHandlerType l "get" (/=GetHandler)
            return $ GetById (Left $3) $6 s1
    }
    | maybeBindResult insert pushScope targetEntity maybeFromJson popScope {%
        do
            l <- mkLoc $2
            statement l "insert"
            let s1 = $1 >>= \(l1,s1') -> return s1'
            let s4 = tkString $4
            case $1 of
                Just (l1,s1) -> declare l1 s1 $ SEntityResult s4
                Nothing -> return ()
            l4 <- mkLoc $4
            let i = Insert (Left s4) $5 s1 
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
    | return json {% 
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
            return $ Require $ SelectQuery [] $3 $4 (Just $6) [] (0,0)
    }
    | for pushScope lowerIdParam in fieldRef lbrace handlerParams rbrace popScope {%
        do
            l <- mkLoc $1
            statement l "for"
            requireHandlerType l "for" (/=GetHandler)
            return $ For $3 $5 $7 
    }
    | lowerIdTk fieldRefList {% 
        do 
            l <- mkLoc $1
            statement l (tkString $1)
            return $ Call (tkString $1) $2
    } 
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
            return (l1, SelectAllFields $ Var (tkString $1) (Left "") False)
    }
    | lowerIdTk dot lowerIdTk maybeSelectAlias {%
        do
            l1 <- mkLoc $1
            if tkString $3 == "id"
                then return (l1, SelectIdField (Var (tkString $1) (Left "") False) $4) 
                else return (l1, SelectField (Var (tkString $1) (Left "") False) (tkString $3) $4) 
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

maybeWithJson: with json { Just $2 }
             | { Nothing }
         
maybeFromJson: from json { Just (Nothing, $2) }
             | from lowerIdTk json {%
                 do
                     l2 <- mkLoc $2
                     let s2 = tkString $2
                     tgt <- withSymbolNow Nothing l2 "target entity" $ getEntitySymbol
                     withSymbolNow () l2 s2 $ requireEntityResult $ \e -> when (Just (entityName e) /= tgt) $ pError l2 $ "Reference to " ++ (entityName e) ++ " (expected " ++ (fromMaybe "" tgt) ++ ")"
                     return $ Just (Just s2, $3)
             }
             | { Nothing }
              
json:  lbrace jsonFields rbrace { $2 }
jsonField : lowerIdTk equals fieldRef {%
        do
            l1 <- mkLoc $1
            let s1 = tkString $1
            te <- symbolMatches "target entity" $ \st -> case st of SEntity _ -> True; _ -> False 
            when te $ withSymbol l1 "target entity" $ 
                requireEntityField l1 s1 $ \_ -> return ()
            return (s1, $3, Nothing) 
     }
    | lowerIdTk equals functionRef lparen fieldRef rparen {%
        do
            l1 <- mkLoc $1
            let s1 = tkString $1
            te <- symbolMatches "target entity" $ \st -> case st of SEntity _ -> True; _ -> False 
            when te $ withSymbol l1 "target entity" $ 
                requireEntityField l1 s1 $ \_ -> return ()
            return (s1, $5, Just $3)
    }



jsonFields : jsonField { [$1] }
           | jsonFields comma jsonField  { $3:$1 }



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
     | lowerIdTk functionParamList {%
     do
         l1 <- mkLoc $1
         let s1 = tkString $1
         withSymbol l1 s1 requireFunction
         return $ ExternExpr s1 $2
     }         

functionParamList: functionParam { [$1] }
                 | functionParamList functionParam { $1 ++ [$2] }
functionParam: fieldRef { FieldRefParam $1 }
             | verbatim { VerbatimParam (tkString $1) }
    

valbinop :      
      slash { Div }
      | asterisk { Mul } 
      | plus { Add }
      | minus { Sub }
      | concatop { Concat }

valexpr : lparen valexpr rparen { $2 }
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
            let f = Field l $2 (FieldInternal `elem` $6) n (NormalField $4 (reverse $5)) Nothing 
            declare l n (SField f)
            return f
        } 
      | lowerIdTk maybeMaybe entityId fieldFlags {% 
        do
            l <- mkLoc $1
            let n = tkString $1
            l3 <- mkLoc $3
            let s3 = tkString $3
            let f = Field l $2 (FieldInternal `elem` $4) n (EntityField s3) Nothing
            withGlobalSymbol l3 s3 requireEntityOrClass
            declare l n (SField f)
            return f}
      | lowerIdTk maybeMaybe enumFieldContent fieldFlags {%
        do  
            l <- mkLoc $1
            let n = tkString $1
            let f = Field l $2 (FieldInternal `elem` $4) n $3 Nothing
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

        

fieldOptions : { [] }
             | pushScope fieldOptionsList popScope { $2 }
fieldOptionsList : fieldOption { [$1] }
                 | fieldOptionsList  fieldOption { $2 : $1 }
fieldOption : check lowerIdTk {%
        do
            l2 <- mkLoc $2
            let s2 = tkString $2
            withSymbol l2 s2 requireFunction
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
      | lbracket rbracket { EmptyList }
      | checkmarkActive { CheckmarkValue Active }
      | checkmarkInactive { CheckmarkValue Inactive }
      
      
uniques : { [] }
        | uniques uniqueDef semicolon { $2 : $1 }
uniqueDef :  unique uniqueUpperId fieldIdList { Unique $2 (reverse $3) }

maybeDeriving : { [] }
             | deriving derives semicolon { (reverse $2) }
derives : upperId { [$1] }
        | derives comma upperId { $3 : $1 }

checks : { [] }
        | check functionRefList semicolon { $2 }

functionRefList: functionRef { [$1] }
              | functionRefList comma functionRef { $1 ++ [$3] }

functionRef: lowerIdTk {%
    do
        l <- mkLoc $1
        let n = tkString $1
        withSymbol l n requireFunction
        return n
    }              

fieldId: lowerIdTk {%
    do
        l <- mkLoc $1
        let n = tkString $1
        withSymbol l n $ requireField $ \_ -> return ()
        return n
    }

fieldIdList : fieldId { [$1] }
            | fieldIdList comma fieldId { $3 : $1 }


fieldType: 
    word32      { FTWord32 }
    | word64    { FTWord64 }
    | int32     { FTInt32 }
    | int       { FTInt }
    | int64     { FTInt64 }
    | text      { FTText }
    | bool      { FTBool }
    | double    { FTDouble }
    | timeofday { FTTimeOfDay }
    | day       { FTDay }
    | utctime   { FTUTCTime }
    | checkmark { FTCheckmark }

maybeMaybe : { False }
              | maybe {True }

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
    let ast = implementClasses m
    errors <- postValidation ast ps
    if errors == 0
        then return $ Just $ simplify ast
        else do
            hPutStrLn stderr $ show errors ++ " errors"
            return Nothing
}
