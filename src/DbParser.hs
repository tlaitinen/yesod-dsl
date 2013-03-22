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

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30
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
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30

action_0 (4#) = happyGoto action_3
action_0 (5#) = happyGoto action_2
action_0 x = happyTcHack x happyReduce_2

action_1 (5#) = happyGoto action_2
action_1 x = happyTcHack x happyFail

action_2 (31#) = happyShift action_6
action_2 (6#) = happyGoto action_4
action_2 (7#) = happyGoto action_5
action_2 x = happyTcHack x happyReduce_5

action_3 (70#) = happyAccept
action_3 x = happyTcHack x happyFail

action_4 x = happyTcHack x happyReduce_3

action_5 (32#) = happyShift action_11
action_5 (33#) = happyShift action_12
action_5 (8#) = happyGoto action_8
action_5 (9#) = happyGoto action_9
action_5 (17#) = happyGoto action_10
action_5 x = happyTcHack x happyReduce_1

action_6 (50#) = happyShift action_7
action_6 x = happyTcHack x happyFail

action_7 (40#) = happyShift action_15
action_7 x = happyTcHack x happyFail

action_8 x = happyTcHack x happyReduce_6

action_9 x = happyTcHack x happyReduce_7

action_10 x = happyTcHack x happyReduce_8

action_11 (37#) = happyShift action_14
action_11 x = happyTcHack x happyFail

action_12 (37#) = happyShift action_13
action_12 x = happyTcHack x happyFail

action_13 (41#) = happyShift action_18
action_13 x = happyTcHack x happyFail

action_14 (48#) = happyShift action_17
action_14 (15#) = happyGoto action_16
action_14 x = happyTcHack x happyReduce_20

action_15 x = happyTcHack x happyReduce_4

action_16 (41#) = happyShift action_22
action_16 x = happyTcHack x happyFail

action_17 (37#) = happyShift action_21
action_17 (16#) = happyGoto action_20
action_17 x = happyTcHack x happyFail

action_18 (18#) = happyGoto action_19
action_18 x = happyTcHack x happyReduce_25

action_19 (36#) = happyShift action_26
action_19 (42#) = happyShift action_27
action_19 (19#) = happyGoto action_25
action_19 x = happyTcHack x happyFail

action_20 (47#) = happyShift action_24
action_20 x = happyTcHack x happyReduce_21

action_21 x = happyTcHack x happyReduce_22

action_22 (18#) = happyGoto action_23
action_22 x = happyTcHack x happyReduce_25

action_23 (36#) = happyShift action_26
action_23 (19#) = happyGoto action_25
action_23 (24#) = happyGoto action_32
action_23 x = happyTcHack x happyReduce_37

action_24 (37#) = happyShift action_31
action_24 x = happyTcHack x happyFail

action_25 (40#) = happyShift action_30
action_25 x = happyTcHack x happyFail

action_26 (62#) = happyShift action_29
action_26 (30#) = happyGoto action_28
action_26 x = happyTcHack x happyReduce_55

action_27 x = happyTcHack x happyReduce_24

action_28 (37#) = happyShift action_37
action_28 (51#) = happyShift action_38
action_28 (52#) = happyShift action_39
action_28 (53#) = happyShift action_40
action_28 (54#) = happyShift action_41
action_28 (55#) = happyShift action_42
action_28 (56#) = happyShift action_43
action_28 (57#) = happyShift action_44
action_28 (58#) = happyShift action_45
action_28 (60#) = happyShift action_46
action_28 (61#) = happyShift action_47
action_28 (29#) = happyGoto action_36
action_28 x = happyTcHack x happyFail

action_29 x = happyTcHack x happyReduce_56

action_30 x = happyTcHack x happyReduce_26

action_31 x = happyTcHack x happyReduce_23

action_32 (34#) = happyShift action_35
action_32 (25#) = happyGoto action_33
action_32 (26#) = happyGoto action_34
action_32 x = happyTcHack x happyReduce_40

action_33 (40#) = happyShift action_56
action_33 x = happyTcHack x happyFail

action_34 (35#) = happyShift action_55
action_34 (10#) = happyGoto action_53
action_34 (27#) = happyGoto action_54
action_34 x = happyTcHack x happyReduce_10

action_35 (37#) = happyShift action_52
action_35 x = happyTcHack x happyFail

action_36 (35#) = happyShift action_51
action_36 (20#) = happyGoto action_48
action_36 (21#) = happyGoto action_49
action_36 (22#) = happyGoto action_50
action_36 x = happyTcHack x happyReduce_29

action_37 x = happyTcHack x happyReduce_28

action_38 x = happyTcHack x happyReduce_45

action_39 x = happyTcHack x happyReduce_46

action_40 x = happyTcHack x happyReduce_47

action_41 x = happyTcHack x happyReduce_48

action_42 x = happyTcHack x happyReduce_49

action_43 x = happyTcHack x happyReduce_50

action_44 x = happyTcHack x happyReduce_51

action_45 x = happyTcHack x happyReduce_52

action_46 x = happyTcHack x happyReduce_53

action_47 x = happyTcHack x happyReduce_54

action_48 x = happyTcHack x happyReduce_27

action_49 (35#) = happyShift action_51
action_49 (22#) = happyGoto action_68
action_49 x = happyTcHack x happyReduce_30

action_50 x = happyTcHack x happyReduce_31

action_51 (36#) = happyShift action_67
action_51 x = happyTcHack x happyFail

action_52 (36#) = happyShift action_66
action_52 (28#) = happyGoto action_65
action_52 x = happyTcHack x happyReduce_43

action_53 (42#) = happyShift action_60
action_53 (63#) = happyShift action_61
action_53 (64#) = happyShift action_62
action_53 (65#) = happyShift action_63
action_53 (66#) = happyShift action_64
action_53 (11#) = happyGoto action_59
action_53 x = happyTcHack x happyFail

action_54 (40#) = happyShift action_58
action_54 x = happyTcHack x happyFail

action_55 (36#) = happyShift action_57
action_55 x = happyTcHack x happyFail

action_56 x = happyTcHack x happyReduce_38

action_57 x = happyTcHack x happyReduce_42

action_58 x = happyTcHack x happyReduce_41

action_59 x = happyTcHack x happyReduce_11

action_60 x = happyTcHack x happyReduce_9

action_61 (41#) = happyShift action_71
action_61 (12#) = happyGoto action_74
action_61 x = happyTcHack x happyFail

action_62 (41#) = happyShift action_71
action_62 (12#) = happyGoto action_73
action_62 x = happyTcHack x happyFail

action_63 (41#) = happyShift action_71
action_63 (12#) = happyGoto action_72
action_63 x = happyTcHack x happyFail

action_64 (41#) = happyShift action_71
action_64 (12#) = happyGoto action_70
action_64 x = happyTcHack x happyFail

action_65 x = happyTcHack x happyReduce_39

action_66 (36#) = happyShift action_66
action_66 (28#) = happyGoto action_69
action_66 x = happyTcHack x happyReduce_43

action_67 x = happyTcHack x happyReduce_33

action_68 x = happyTcHack x happyReduce_32

action_69 x = happyTcHack x happyReduce_44

action_70 x = happyTcHack x happyReduce_15

action_71 (13#) = happyGoto action_75
action_71 x = happyTcHack x happyReduce_17

action_72 x = happyTcHack x happyReduce_14

action_73 x = happyTcHack x happyReduce_13

action_74 x = happyTcHack x happyReduce_12

action_75 (42#) = happyShift action_77
action_75 (67#) = happyShift action_78
action_75 (14#) = happyGoto action_76
action_75 x = happyTcHack x happyFail

action_76 (40#) = happyShift action_79
action_76 x = happyTcHack x happyFail

action_77 x = happyTcHack x happyReduce_16

action_78 x = happyTcHack x happyReduce_19

action_79 x = happyTcHack x happyReduce_18

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
happyReduction_8 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn8
		 (IfaceDef happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happyReduce 9# 9# happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_8) `HappyStk`
	(HappyAbsSyn26  happy_var_7) `HappyStk`
	(HappyAbsSyn24  happy_var_6) `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	(HappyTerminal (Tk _ (TUpperId happy_var_2))) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Entity (mkLoc happy_var_1) happy_var_2 happy_var_3 happy_var_5 happy_var_6 happy_var_7 happy_var_8
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_0  10# happyReduction_10
happyReduction_10  =  HappyAbsSyn10
		 ([]
	)

happyReduce_11 = happySpecReduce_2  10# happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_2 : happy_var_1
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  11# happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Service GetService happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  11# happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Service PutService happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  11# happyReduction_14
happyReduction_14 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Service PostService happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  11# happyReduction_15
happyReduction_15 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Service DeleteService happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  12# happyReduction_16
happyReduction_16 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  13# happyReduction_17
happyReduction_17  =  HappyAbsSyn13
		 ([]
	)

happyReduce_18 = happySpecReduce_3  13# happyReduction_18
happyReduction_18 _
	(HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_1
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  14# happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn14
		 (PublicService
	)

happyReduce_20 = happySpecReduce_0  15# happyReduction_20
happyReduction_20  =  HappyAbsSyn15
		 ([]
	)

happyReduce_21 = happySpecReduce_2  15# happyReduction_21
happyReduction_21 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  16# happyReduction_22
happyReduction_22 (HappyTerminal (Tk _ (TUpperId happy_var_1)))
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  16# happyReduction_23
happyReduction_23 (HappyTerminal (Tk _ (TUpperId happy_var_3)))
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_3 : happy_var_1
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 5# 17# happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tk _ (TUpperId happy_var_2))) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (Iface (mkLoc happy_var_1) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_0  18# happyReduction_25
happyReduction_25  =  HappyAbsSyn18
		 ([]
	)

happyReduce_26 = happySpecReduce_3  18# happyReduction_26
happyReduction_26 _
	(HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_2 : happy_var_1
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 4# 19# happyReduction_27
happyReduction_27 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	(HappyTerminal (Tk _ (TLowerId happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Field happy_var_2 happy_var_1 (NormalField (tokenType happy_var_3) happy_var_4)
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_3  19# happyReduction_28
happyReduction_28 (HappyTerminal (Tk _ (TUpperId happy_var_3)))
	(HappyAbsSyn30  happy_var_2)
	(HappyTerminal (Tk _ (TLowerId happy_var_1)))
	 =  HappyAbsSyn19
		 (Field happy_var_2 happy_var_1 (EntityField happy_var_3)
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_0  20# happyReduction_29
happyReduction_29  =  HappyAbsSyn20
		 ([]
	)

happyReduce_30 = happySpecReduce_1  20# happyReduction_30
happyReduction_30 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  21# happyReduction_31
happyReduction_31 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  21# happyReduction_32
happyReduction_32 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_2 : happy_var_1
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  22# happyReduction_33
happyReduction_33 (HappyTerminal (Tk _ (TLowerId happy_var_2)))
	_
	 =  HappyAbsSyn22
		 (FieldCheck happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  23# happyReduction_34
happyReduction_34 (HappyTerminal (Tk _ (TString happy_var_1)))
	 =  HappyAbsSyn23
		 (StringValue happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  23# happyReduction_35
happyReduction_35 (HappyTerminal (Tk _ (TInt happy_var_1)))
	 =  HappyAbsSyn23
		 (IntValue happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  23# happyReduction_36
happyReduction_36 (HappyTerminal (Tk _ (TFloat happy_var_1)))
	 =  HappyAbsSyn23
		 (FloatValue happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  24# happyReduction_37
happyReduction_37  =  HappyAbsSyn24
		 ([]
	)

happyReduce_38 = happySpecReduce_3  24# happyReduction_38
happyReduction_38 _
	(HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_2 : happy_var_1
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  25# happyReduction_39
happyReduction_39 (HappyAbsSyn28  happy_var_3)
	(HappyTerminal (Tk _ (TUpperId happy_var_2)))
	_
	 =  HappyAbsSyn25
		 (Unique happy_var_2 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_0  26# happyReduction_40
happyReduction_40  =  HappyAbsSyn26
		 ([]
	)

happyReduce_41 = happySpecReduce_3  26# happyReduction_41
happyReduction_41 _
	(HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_2 : happy_var_1
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_2  27# happyReduction_42
happyReduction_42 (HappyTerminal (Tk _ (TLowerId happy_var_2)))
	_
	 =  HappyAbsSyn27
		 (happy_var_2
	)
happyReduction_42 _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_0  28# happyReduction_43
happyReduction_43  =  HappyAbsSyn28
		 ([]
	)

happyReduce_44 = happySpecReduce_2  28# happyReduction_44
happyReduction_44 (HappyAbsSyn28  happy_var_2)
	(HappyTerminal (Tk _ (TLowerId happy_var_1)))
	 =  HappyAbsSyn28
		 (happy_var_1 : happy_var_2
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  29# happyReduction_45
happyReduction_45 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  29# happyReduction_46
happyReduction_46 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  29# happyReduction_47
happyReduction_47 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  29# happyReduction_48
happyReduction_48 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  29# happyReduction_49
happyReduction_49 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  29# happyReduction_50
happyReduction_50 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  29# happyReduction_51
happyReduction_51 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  29# happyReduction_52
happyReduction_52 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  29# happyReduction_53
happyReduction_53 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  29# happyReduction_54
happyReduction_54 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_0  30# happyReduction_55
happyReduction_55  =  HappyAbsSyn30
		 (False
	)

happyReduce_56 = happySpecReduce_1  30# happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn30
		 (True
	)

happyNewToken action sts stk [] =
	action 70# 70# notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Tk _ TImport -> cont 31#;
	Tk _ TEntity -> cont 32#;
	Tk _ TIface -> cont 33#;
	Tk _ TUnique -> cont 34#;
	Tk _ TCheck -> cont 35#;
	Tk _ (TLowerId happy_dollar_dollar) -> cont 36#;
	Tk _ (TUpperId happy_dollar_dollar) -> cont 37#;
	Tk _ (TInt happy_dollar_dollar) -> cont 38#;
	Tk _ (TFloat happy_dollar_dollar) -> cont 39#;
	Tk _ TSemicolon -> cont 40#;
	Tk _ TLBrace -> cont 41#;
	Tk _ TRBrace -> cont 42#;
	Tk _ TLParen -> cont 43#;
	Tk _ TRParen -> cont 44#;
	Tk _ TLBrack -> cont 45#;
	Tk _ TRBrack -> cont 46#;
	Tk _ TComma -> cont 47#;
	Tk _ TColon -> cont 48#;
	Tk _ TDot -> cont 49#;
	Tk _ (TString happy_dollar_dollar) -> cont 50#;
	Tk _ TWord32 -> cont 51#;
	Tk _ TWord64 -> cont 52#;
	Tk _ TInt32 -> cont 53#;
	Tk _ TInt64 -> cont 54#;
	Tk _ TText -> cont 55#;
	Tk _ TBool -> cont 56#;
	Tk _ TDouble -> cont 57#;
	Tk _ TTime -> cont 58#;
	Tk _ TDate -> cont 59#;
	Tk _ TDateTime -> cont 60#;
	Tk _ TZonedTime -> cont 61#;
	Tk _ TMaybe -> cont 62#;
	Tk _ TGet -> cont 63#;
	Tk _ TPut -> cont 64#;
	Tk _ TPost -> cont 65#;
	Tk _ TDelete -> cont 66#;
	Tk _ TPublic -> cont 67#;
	Tk _ TBefore -> cont 68#;
	Tk _ TAfter -> cont 69#;
	_ -> happyError' (tk:tks)
	}

happyError_ 70# tk tks = happyError' tks
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
