{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
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
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.18.9

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23

action_0 (4#) = happyGoto action_3
action_0 (5#) = happyGoto action_2
action_0 x = happyTcHack x happyReduce_2

action_1 (5#) = happyGoto action_2
action_1 x = happyTcHack x happyFail

action_2 (24#) = happyShift action_6
action_2 (6#) = happyGoto action_4
action_2 (7#) = happyGoto action_5
action_2 x = happyTcHack x happyReduce_5

action_3 (56#) = happyAccept
action_3 x = happyTcHack x happyFail

action_4 x = happyTcHack x happyReduce_3

action_5 (25#) = happyShift action_11
action_5 (26#) = happyShift action_12
action_5 (8#) = happyGoto action_8
action_5 (9#) = happyGoto action_9
action_5 (12#) = happyGoto action_10
action_5 x = happyTcHack x happyReduce_1

action_6 (43#) = happyShift action_7
action_6 x = happyTcHack x happyFail

action_7 (34#) = happyShift action_15
action_7 x = happyTcHack x happyFail

action_8 x = happyTcHack x happyReduce_6

action_9 x = happyTcHack x happyReduce_7

action_10 x = happyTcHack x happyReduce_8

action_11 (31#) = happyShift action_14
action_11 x = happyTcHack x happyFail

action_12 (31#) = happyShift action_13
action_12 x = happyTcHack x happyFail

action_13 (35#) = happyShift action_17
action_13 x = happyTcHack x happyFail

action_14 (35#) = happyShift action_16
action_14 x = happyTcHack x happyFail

action_15 x = happyTcHack x happyReduce_4

action_16 (10#) = happyGoto action_19
action_16 x = happyTcHack x happyReduce_10

action_17 (13#) = happyGoto action_18
action_17 x = happyTcHack x happyReduce_14

action_18 (30#) = happyShift action_24
action_18 (36#) = happyShift action_25
action_18 (14#) = happyGoto action_23
action_18 x = happyTcHack x happyFail

action_19 (27#) = happyShift action_22
action_19 (11#) = happyGoto action_20
action_19 (13#) = happyGoto action_21
action_19 x = happyTcHack x happyReduce_14

action_20 (34#) = happyShift action_31
action_20 x = happyTcHack x happyFail

action_21 (30#) = happyShift action_24
action_21 (14#) = happyGoto action_23
action_21 (19#) = happyGoto action_30
action_21 x = happyTcHack x happyReduce_26

action_22 (31#) = happyShift action_29
action_22 x = happyTcHack x happyFail

action_23 (34#) = happyShift action_28
action_23 x = happyTcHack x happyFail

action_24 (55#) = happyShift action_27
action_24 (23#) = happyGoto action_26
action_24 x = happyTcHack x happyReduce_41

action_25 x = happyTcHack x happyReduce_13

action_26 (31#) = happyShift action_36
action_26 (44#) = happyShift action_37
action_26 (45#) = happyShift action_38
action_26 (46#) = happyShift action_39
action_26 (47#) = happyShift action_40
action_26 (48#) = happyShift action_41
action_26 (49#) = happyShift action_42
action_26 (50#) = happyShift action_43
action_26 (51#) = happyShift action_44
action_26 (53#) = happyShift action_45
action_26 (54#) = happyShift action_46
action_26 (22#) = happyGoto action_35
action_26 x = happyTcHack x happyFail

action_27 x = happyTcHack x happyReduce_42

action_28 x = happyTcHack x happyReduce_15

action_29 x = happyTcHack x happyReduce_12

action_30 (28#) = happyShift action_33
action_30 (36#) = happyShift action_34
action_30 (20#) = happyGoto action_32
action_30 x = happyTcHack x happyFail

action_31 x = happyTcHack x happyReduce_11

action_32 (34#) = happyShift action_52
action_32 x = happyTcHack x happyFail

action_33 (31#) = happyShift action_51
action_33 x = happyTcHack x happyFail

action_34 x = happyTcHack x happyReduce_9

action_35 (29#) = happyShift action_50
action_35 (15#) = happyGoto action_47
action_35 (16#) = happyGoto action_48
action_35 (17#) = happyGoto action_49
action_35 x = happyTcHack x happyReduce_18

action_36 x = happyTcHack x happyReduce_17

action_37 x = happyTcHack x happyReduce_31

action_38 x = happyTcHack x happyReduce_32

action_39 x = happyTcHack x happyReduce_33

action_40 x = happyTcHack x happyReduce_34

action_41 x = happyTcHack x happyReduce_35

action_42 x = happyTcHack x happyReduce_36

action_43 x = happyTcHack x happyReduce_37

action_44 x = happyTcHack x happyReduce_38

action_45 x = happyTcHack x happyReduce_39

action_46 x = happyTcHack x happyReduce_40

action_47 x = happyTcHack x happyReduce_16

action_48 (29#) = happyShift action_50
action_48 (17#) = happyGoto action_56
action_48 x = happyTcHack x happyReduce_19

action_49 x = happyTcHack x happyReduce_20

action_50 (30#) = happyShift action_55
action_50 x = happyTcHack x happyFail

action_51 (30#) = happyShift action_54
action_51 (21#) = happyGoto action_53
action_51 x = happyTcHack x happyReduce_29

action_52 x = happyTcHack x happyReduce_27

action_53 x = happyTcHack x happyReduce_28

action_54 (30#) = happyShift action_54
action_54 (21#) = happyGoto action_57
action_54 x = happyTcHack x happyReduce_29

action_55 x = happyTcHack x happyReduce_22

action_56 x = happyTcHack x happyReduce_21

action_57 x = happyTcHack x happyReduce_30

happyReduce_1 = happySpecReduce_2  4# happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (DbModule happy_var_1 (getEntities happy_var_2) 
                                        (getIfaces happy_var_2)
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5# happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5# happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 : happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6# happyReduction_4
happyReduction_4 _
	(HappyTerminal (Tk _ (TString happy_var_2)))
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  7# happyReduction_5
happyReduction_5  =  HappyAbsSyn7
		 ([]
	)

happyReduce_6 = happySpecReduce_2  7# happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_2 : happy_var_1
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8# happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (EntityDef happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8# happyReduction_8
happyReduction_8 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn8
		 (IfaceDef happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happyReduce 7# 9# happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_6) `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tk _ (TUpperId happy_var_2))) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Entity (mkLoc happy_var_1) happy_var_2 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_0  10# happyReduction_10
happyReduction_10  =  HappyAbsSyn10
		 ([]
	)

happyReduce_11 = happySpecReduce_3  10# happyReduction_11
happyReduction_11 _
	(HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_2 : happy_var_1
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  11# happyReduction_12
happyReduction_12 (HappyTerminal (Tk _ (TUpperId happy_var_2)))
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 5# 12# happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tk _ (TUpperId happy_var_2))) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Iface (mkLoc happy_var_1) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_0  13# happyReduction_14
happyReduction_14  =  HappyAbsSyn13
		 ([]
	)

happyReduce_15 = happySpecReduce_3  13# happyReduction_15
happyReduction_15 _
	(HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_1
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 4# 14# happyReduction_16
happyReduction_16 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	(HappyTerminal (Tk _ (TLowerId happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Field happy_var_2 happy_var_1 (NormalField (tokenType happy_var_3) happy_var_4)
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3  14# happyReduction_17
happyReduction_17 (HappyTerminal (Tk _ (TUpperId happy_var_3)))
	(HappyAbsSyn23  happy_var_2)
	(HappyTerminal (Tk _ (TLowerId happy_var_1)))
	 =  HappyAbsSyn14
		 (Field happy_var_2 happy_var_1 (EntityField happy_var_3)
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  15# happyReduction_18
happyReduction_18  =  HappyAbsSyn15
		 ([]
	)

happyReduce_19 = happySpecReduce_1  15# happyReduction_19
happyReduction_19 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  16# happyReduction_20
happyReduction_20 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  16# happyReduction_21
happyReduction_21 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_2 : happy_var_1
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  17# happyReduction_22
happyReduction_22 (HappyTerminal (Tk _ (TLowerId happy_var_2)))
	_
	 =  HappyAbsSyn17
		 (FieldCheck happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  18# happyReduction_23
happyReduction_23 (HappyTerminal (Tk _ (TString happy_var_1)))
	 =  HappyAbsSyn18
		 (StringValue happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  18# happyReduction_24
happyReduction_24 (HappyTerminal (Tk _ (TInt happy_var_1)))
	 =  HappyAbsSyn18
		 (IntValue happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  18# happyReduction_25
happyReduction_25 (HappyTerminal (Tk _ (TFloat happy_var_1)))
	 =  HappyAbsSyn18
		 (FloatValue happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  19# happyReduction_26
happyReduction_26  =  HappyAbsSyn19
		 ([]
	)

happyReduce_27 = happySpecReduce_3  19# happyReduction_27
happyReduction_27 _
	(HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_2 : happy_var_1
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  20# happyReduction_28
happyReduction_28 (HappyAbsSyn21  happy_var_3)
	(HappyTerminal (Tk _ (TUpperId happy_var_2)))
	_
	 =  HappyAbsSyn20
		 (Unique happy_var_2 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_0  21# happyReduction_29
happyReduction_29  =  HappyAbsSyn21
		 ([]
	)

happyReduce_30 = happySpecReduce_2  21# happyReduction_30
happyReduction_30 (HappyAbsSyn21  happy_var_2)
	(HappyTerminal (Tk _ (TLowerId happy_var_1)))
	 =  HappyAbsSyn21
		 (happy_var_1 : happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  22# happyReduction_31
happyReduction_31 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  22# happyReduction_32
happyReduction_32 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  22# happyReduction_33
happyReduction_33 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  22# happyReduction_34
happyReduction_34 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  22# happyReduction_35
happyReduction_35 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  22# happyReduction_36
happyReduction_36 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  22# happyReduction_37
happyReduction_37 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  22# happyReduction_38
happyReduction_38 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  22# happyReduction_39
happyReduction_39 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  22# happyReduction_40
happyReduction_40 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_0  23# happyReduction_41
happyReduction_41  =  HappyAbsSyn23
		 (False
	)

happyReduce_42 = happySpecReduce_1  23# happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn23
		 (True
	)

happyNewToken action sts stk [] =
	action 56# 56# notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Tk _ TImport -> cont 24#;
	Tk _ TEntity -> cont 25#;
	Tk _ TIface -> cont 26#;
	Tk _ TImplements -> cont 27#;
	Tk _ TUnique -> cont 28#;
	Tk _ TCheck -> cont 29#;
	Tk _ (TLowerId happy_dollar_dollar) -> cont 30#;
	Tk _ (TUpperId happy_dollar_dollar) -> cont 31#;
	Tk _ (TInt happy_dollar_dollar) -> cont 32#;
	Tk _ (TFloat happy_dollar_dollar) -> cont 33#;
	Tk _ TSemicolon -> cont 34#;
	Tk _ TLBrace -> cont 35#;
	Tk _ TRBrace -> cont 36#;
	Tk _ TLParen -> cont 37#;
	Tk _ TRParen -> cont 38#;
	Tk _ TLBrack -> cont 39#;
	Tk _ TRBrack -> cont 40#;
	Tk _ TComma -> cont 41#;
	Tk _ TDot -> cont 42#;
	Tk _ (TString happy_dollar_dollar) -> cont 43#;
	Tk _ TWord32 -> cont 44#;
	Tk _ TWord64 -> cont 45#;
	Tk _ TInt32 -> cont 46#;
	Tk _ TInt64 -> cont 47#;
	Tk _ TText -> cont 48#;
	Tk _ TBool -> cont 49#;
	Tk _ TDouble -> cont 50#;
	Tk _ TTime -> cont 51#;
	Tk _ TDate -> cont 52#;
	Tk _ TDateTime -> cont 53#;
	Tk _ TZonedTime -> cont 54#;
	Tk _ TMaybe -> cont 55#;
	_ -> happyError' (tk:tks)
	}

happyError_ 56# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

dbdef tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 1#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 1# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j ) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Happy_GHC_Exts.Int# ->                    -- token number
         Happy_GHC_Exts.Int# ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 1# tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop 0# l = l
happyDrop n ((_):(t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (1# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 1# tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  1# tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action 1# 1# tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action 1# 1# tk (HappyState (action)) sts ( (HappyErrorToken (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
