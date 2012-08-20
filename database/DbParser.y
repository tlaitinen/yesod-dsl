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
    entity      { Tk _ TEntity }
    iface      { Tk _ TIface }
    relation   { Tk _ TRelation }
    implements { Tk _ TImplements }
    default    { Tk _ TDefault }
    index      { Tk _ TIndex }
    unique     { Tk _ TUnique }
    check      { Tk _ TCheck }
    lowerId    { Tk _ (TLowerId $$) }
    upperId    { Tk _ (TUpperId $$)  }
    intval        { Tk _ (TInt $$)  }
    floatval      { Tk _ (TFloat $$) }
    semicolon  { Tk _ TSemicolon }
    lbrace { Tk _ TLBrace }
    rbrace { Tk _ TRBrace }
    lparen { Tk _ TLParen }
    rparen { Tk _ TRParen }
    lbrack { Tk _ TLBrack }
    rbrack { Tk _ TRBrack }
    comma  { Tk _ TComma }
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
    zoneddatetime { Tk _ TZonedDateTime }
    optional { Tk _ TOptional }

%%

dbModule : imports dbDefs { DbModule $1 (getEntities $2) 
                                        (getRelations $2)
                                        (getIfaces $2)}

imports : { [] }
        | imports importStmt { $2 : $1 }

importStmt : import stringval semicolon { $2 }
dbDefs : {- empty -}   { [] }
       | dbDefs dbDef  { $2 : $1 }
dbDef : entityDef      { EntityDef $1 } 
      | ifaceDef      { IfaceDef $1 }
      | relDef        { RelDef $1 }

entityDef : entity upperId lbrace 
            implementations 
            fields
            indices
            rbrace { Entity (mkLoc $1) $2 $4 $5 $6 }

implementations : { [] }
                | implementations implementation semicolon { $2 : $1 }

implementation : implements upperId { $2 }

ifaceDef : iface upperId lbrace
             fields
             indices
            rbrace { Iface (mkLoc $1) $2 $4 $5 }

relDef : relation upperId lbrace
            fields
            indices
        rbrace { Relation (mkLoc $1) $2 $4 $5 } 

fields : { [] }
              | fields field semicolon { $2 : $1 }
 
field : maybeOptional fieldType lowerId fieldOptions { Field $1 $3 (NormalField (tokenType $2) $4) } 
      | maybeOptional upperId lowerId backRef { Field $1 $2 (RelField $3 $4) }

fieldOptions : { [] }
             | lparen fieldOptionsList rparen { $2 }
fieldOptionsList : fieldOption { [$1] }
                 | fieldOptionsList comma fieldOption { $3 : $1 }
fieldOption : unique { FieldUnique }
            | default value { FieldDefaultValue $2 }
            | check lowerId { FieldCheck $2 }

value : stringval { StringValue $1 }
      | intval { IntValue $1 }
      | floatval { FloatValue $1 }

indices : { [] }
        | indices indexDef semicolon { $2 : $1 }
indexDef :  maybeUnique index lparen lowerIdList rparen { Index $1 $4 }

maybeUnique : { False }
            | unique { True }
lowerIdList : lowerId { [$1] }
            | lowerIdList comma lowerId { $3:  $1 }

backRef : { Nothing } 
        | lparen multiplicity lowerId rparen { Just (BackRefField $2 $3) }

multiplicity : 
             { One }
             | lbrack rbrack { Many }

fieldType : word32 { $1 }
          | word64{ $1 }
          | int32{ $1 }
          | int64{ $1 }
          | text { $1 }
          | bool{ $1 }
          | double{ $1 }
          | time { $1 }
          | datetime{ $1 }
          | zonedtime{ $1 }
          | zoneddatetime{ $1 }

maybeOptional : { False }
              | optional { True }

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
