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

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27
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

action_0 (4#) = happyGoto action_3
action_0 (5#) = happyGoto action_2
action_0 x = happyTcHack x happyReduce_2

action_1 (5#) = happyGoto action_2
action_1 x = happyTcHack x happyFail

action_2 (28#) = happyShift action_6
action_2 (6#) = happyGoto action_4
action_2 (7#) = happyGoto action_5
action_2 x = happyTcHack x happyReduce_5

action_3 (68#) = happyAccept
action_3 x = happyTcHack x happyFail

action_4 x = happyTcHack x happyReduce_3

action_5 (29#) = happyShift action_12
action_5 (30#) = happyShift action_13
action_5 (31#) = happyShift action_14
action_5 (8#) = happyGoto action_8
action_5 (9#) = happyGoto action_9
action_5 (12#) = happyGoto action_10
action_5 (13#) = happyGoto action_11
action_5 x = happyTcHack x happyReduce_1

action_6 (49#) = happyShift action_7
action_6 x = happyTcHack x happyFail

action_7 (41#) = happyShift action_18
action_7 x = happyTcHack x happyFail

action_8 x = happyTcHack x happyReduce_6

action_9 x = happyTcHack x happyReduce_7

action_10 x = happyTcHack x happyReduce_8

action_11 x = happyTcHack x happyReduce_9

action_12 (38#) = happyShift action_17
action_12 x = happyTcHack x happyFail

action_13 (38#) = happyShift action_16
action_13 x = happyTcHack x happyFail

action_14 (38#) = happyShift action_15
action_14 x = happyTcHack x happyFail

action_15 (42#) = happyShift action_21
action_15 x = happyTcHack x happyFail

action_16 (42#) = happyShift action_20
action_16 x = happyTcHack x happyFail

action_17 (42#) = happyShift action_19
action_17 x = happyTcHack x happyFail

action_18 x = happyTcHack x happyReduce_4

action_19 (10#) = happyGoto action_24
action_19 x = happyTcHack x happyReduce_11

action_20 (14#) = happyGoto action_23
action_20 x = happyTcHack x happyReduce_16

action_21 (14#) = happyGoto action_22
action_21 x = happyTcHack x happyReduce_16

action_22 (38#) = happyReduce_57
action_22 (50#) = happyReduce_57
action_22 (51#) = happyReduce_57
action_22 (52#) = happyReduce_57
action_22 (53#) = happyReduce_57
action_22 (54#) = happyReduce_57
action_22 (55#) = happyReduce_57
action_22 (56#) = happyReduce_57
action_22 (57#) = happyReduce_57
action_22 (58#) = happyReduce_57
action_22 (59#) = happyReduce_57
action_22 (60#) = happyReduce_57
action_22 (62#) = happyReduce_57
action_22 (63#) = happyReduce_57
action_22 (64#) = happyReduce_57
action_22 (65#) = happyReduce_57
action_22 (66#) = happyReduce_57
action_22 (67#) = happyShift action_31
action_22 (15#) = happyGoto action_28
action_22 (20#) = happyGoto action_32
action_22 (27#) = happyGoto action_30
action_22 x = happyTcHack x happyReduce_30

action_23 (38#) = happyReduce_57
action_23 (50#) = happyReduce_57
action_23 (51#) = happyReduce_57
action_23 (52#) = happyReduce_57
action_23 (53#) = happyReduce_57
action_23 (54#) = happyReduce_57
action_23 (55#) = happyReduce_57
action_23 (56#) = happyReduce_57
action_23 (57#) = happyReduce_57
action_23 (58#) = happyReduce_57
action_23 (59#) = happyReduce_57
action_23 (60#) = happyReduce_57
action_23 (62#) = happyReduce_57
action_23 (63#) = happyReduce_57
action_23 (64#) = happyReduce_57
action_23 (65#) = happyReduce_57
action_23 (66#) = happyReduce_57
action_23 (67#) = happyShift action_31
action_23 (15#) = happyGoto action_28
action_23 (20#) = happyGoto action_29
action_23 (27#) = happyGoto action_30
action_23 x = happyTcHack x happyReduce_30

action_24 (32#) = happyShift action_27
action_24 (11#) = happyGoto action_25
action_24 (14#) = happyGoto action_26
action_24 x = happyTcHack x happyReduce_16

action_25 (41#) = happyShift action_59
action_25 x = happyTcHack x happyFail

action_26 (38#) = happyReduce_57
action_26 (50#) = happyReduce_57
action_26 (51#) = happyReduce_57
action_26 (52#) = happyReduce_57
action_26 (53#) = happyReduce_57
action_26 (54#) = happyReduce_57
action_26 (55#) = happyReduce_57
action_26 (56#) = happyReduce_57
action_26 (57#) = happyReduce_57
action_26 (58#) = happyReduce_57
action_26 (59#) = happyReduce_57
action_26 (60#) = happyReduce_57
action_26 (62#) = happyReduce_57
action_26 (63#) = happyReduce_57
action_26 (64#) = happyReduce_57
action_26 (65#) = happyReduce_57
action_26 (66#) = happyReduce_57
action_26 (67#) = happyShift action_31
action_26 (15#) = happyGoto action_28
action_26 (20#) = happyGoto action_58
action_26 (27#) = happyGoto action_30
action_26 x = happyTcHack x happyReduce_30

action_27 (38#) = happyShift action_57
action_27 x = happyTcHack x happyFail

action_28 (41#) = happyShift action_56
action_28 x = happyTcHack x happyFail

action_29 (35#) = happyShift action_35
action_29 (43#) = happyShift action_55
action_29 (21#) = happyGoto action_33
action_29 (22#) = happyGoto action_34
action_29 x = happyTcHack x happyReduce_33

action_30 (38#) = happyShift action_38
action_30 (50#) = happyShift action_39
action_30 (51#) = happyShift action_40
action_30 (52#) = happyShift action_41
action_30 (53#) = happyShift action_42
action_30 (54#) = happyShift action_43
action_30 (55#) = happyShift action_44
action_30 (56#) = happyShift action_45
action_30 (57#) = happyShift action_46
action_30 (58#) = happyShift action_47
action_30 (59#) = happyShift action_48
action_30 (60#) = happyShift action_49
action_30 (62#) = happyShift action_50
action_30 (63#) = happyShift action_51
action_30 (64#) = happyShift action_52
action_30 (65#) = happyShift action_53
action_30 (66#) = happyShift action_54
action_30 (26#) = happyGoto action_37
action_30 x = happyTcHack x happyFail

action_31 x = happyTcHack x happyReduce_58

action_32 (35#) = happyShift action_35
action_32 (43#) = happyShift action_36
action_32 (21#) = happyGoto action_33
action_32 (22#) = happyGoto action_34
action_32 x = happyTcHack x happyReduce_33

action_33 (41#) = happyShift action_64
action_33 x = happyTcHack x happyFail

action_34 (34#) = happyShift action_63
action_34 x = happyTcHack x happyFail

action_35 x = happyTcHack x happyReduce_34

action_36 x = happyTcHack x happyReduce_15

action_37 (37#) = happyShift action_62
action_37 x = happyTcHack x happyFail

action_38 (37#) = happyShift action_61
action_38 x = happyTcHack x happyFail

action_39 x = happyTcHack x happyReduce_41

action_40 x = happyTcHack x happyReduce_42

action_41 x = happyTcHack x happyReduce_43

action_42 x = happyTcHack x happyReduce_44

action_43 x = happyTcHack x happyReduce_45

action_44 x = happyTcHack x happyReduce_46

action_45 x = happyTcHack x happyReduce_47

action_46 x = happyTcHack x happyReduce_48

action_47 x = happyTcHack x happyReduce_49

action_48 x = happyTcHack x happyReduce_50

action_49 x = happyTcHack x happyReduce_51

action_50 x = happyTcHack x happyReduce_52

action_51 x = happyTcHack x happyReduce_53

action_52 x = happyTcHack x happyReduce_54

action_53 x = happyTcHack x happyReduce_55

action_54 x = happyTcHack x happyReduce_56

action_55 x = happyTcHack x happyReduce_14

action_56 x = happyTcHack x happyReduce_17

action_57 x = happyTcHack x happyReduce_13

action_58 (35#) = happyShift action_35
action_58 (43#) = happyShift action_60
action_58 (21#) = happyGoto action_33
action_58 (22#) = happyGoto action_34
action_58 x = happyTcHack x happyReduce_33

action_59 x = happyTcHack x happyReduce_12

action_60 x = happyTcHack x happyReduce_10

action_61 (44#) = happyShift action_69
action_61 (24#) = happyGoto action_68
action_61 x = happyTcHack x happyReduce_37

action_62 (44#) = happyShift action_67
action_62 (16#) = happyGoto action_66
action_62 x = happyTcHack x happyReduce_20

action_63 (44#) = happyShift action_65
action_63 x = happyTcHack x happyFail

action_64 x = happyTcHack x happyReduce_31

action_65 (37#) = happyShift action_78
action_65 (23#) = happyGoto action_77
action_65 x = happyTcHack x happyFail

action_66 x = happyTcHack x happyReduce_18

action_67 (33#) = happyShift action_74
action_67 (35#) = happyShift action_75
action_67 (36#) = happyShift action_76
action_67 (17#) = happyGoto action_72
action_67 (18#) = happyGoto action_73
action_67 x = happyTcHack x happyFail

action_68 x = happyTcHack x happyReduce_19

action_69 (46#) = happyShift action_71
action_69 (25#) = happyGoto action_70
action_69 x = happyTcHack x happyReduce_39

action_70 (37#) = happyShift action_89
action_70 x = happyTcHack x happyFail

action_71 (47#) = happyShift action_88
action_71 x = happyTcHack x happyFail

action_72 (45#) = happyShift action_86
action_72 (48#) = happyShift action_87
action_72 x = happyTcHack x happyFail

action_73 x = happyTcHack x happyReduce_22

action_74 (39#) = happyShift action_83
action_74 (40#) = happyShift action_84
action_74 (49#) = happyShift action_85
action_74 (19#) = happyGoto action_82
action_74 x = happyTcHack x happyFail

action_75 x = happyTcHack x happyReduce_24

action_76 (37#) = happyShift action_81
action_76 x = happyTcHack x happyFail

action_77 (45#) = happyShift action_79
action_77 (48#) = happyShift action_80
action_77 x = happyTcHack x happyFail

action_78 x = happyTcHack x happyReduce_35

action_79 x = happyTcHack x happyReduce_32

action_80 (37#) = happyShift action_92
action_80 x = happyTcHack x happyFail

action_81 x = happyTcHack x happyReduce_26

action_82 x = happyTcHack x happyReduce_25

action_83 x = happyTcHack x happyReduce_28

action_84 x = happyTcHack x happyReduce_29

action_85 x = happyTcHack x happyReduce_27

action_86 x = happyTcHack x happyReduce_21

action_87 (33#) = happyShift action_74
action_87 (35#) = happyShift action_75
action_87 (36#) = happyShift action_76
action_87 (18#) = happyGoto action_91
action_87 x = happyTcHack x happyFail

action_88 x = happyTcHack x happyReduce_40

action_89 (45#) = happyShift action_90
action_89 x = happyTcHack x happyFail

action_90 x = happyTcHack x happyReduce_38

action_91 x = happyTcHack x happyReduce_23

action_92 x = happyTcHack x happyReduce_36

happyReduce_1 = happySpecReduce_2  4# happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (DbModule happy_var_1 (getEntities happy_var_2) 
                                        (getRelations happy_var_2)
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

happyReduce_9 = happySpecReduce_1  8# happyReduction_9
happyReduction_9 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn8
		 (RelDef happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happyReduce 7# 9# happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_6) `HappyStk`
	(HappyAbsSyn14  happy_var_5) `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tk _ (TUpperId happy_var_2))) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Entity (mkLoc happy_var_1) happy_var_2 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_0  10# happyReduction_11
happyReduction_11  =  HappyAbsSyn10
		 ([]
	)

happyReduce_12 = happySpecReduce_3  10# happyReduction_12
happyReduction_12 _
	(HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_2 : happy_var_1
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  11# happyReduction_13
happyReduction_13 (HappyTerminal (Tk _ (TUpperId happy_var_2)))
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 6# 12# happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_5) `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tk _ (TUpperId happy_var_2))) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Iface (mkLoc happy_var_1) happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 6# 13# happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_5) `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tk _ (TUpperId happy_var_2))) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Relation (mkLoc happy_var_1) happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_0  14# happyReduction_16
happyReduction_16  =  HappyAbsSyn14
		 ([]
	)

happyReduce_17 = happySpecReduce_3  14# happyReduction_17
happyReduction_17 _
	(HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_2 : happy_var_1
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4# 15# happyReduction_18
happyReduction_18 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	(HappyTerminal (Tk _ (TLowerId happy_var_3))) `HappyStk`
	(HappyAbsSyn26  happy_var_2) `HappyStk`
	(HappyAbsSyn27  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Field happy_var_1 (tokenType happy_var_2) happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 4# 15# happyReduction_19
happyReduction_19 ((HappyAbsSyn24  happy_var_4) `HappyStk`
	(HappyTerminal (Tk _ (TLowerId happy_var_3))) `HappyStk`
	(HappyTerminal (Tk _ (TUpperId happy_var_2))) `HappyStk`
	(HappyAbsSyn27  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (RefField happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_0  16# happyReduction_20
happyReduction_20  =  HappyAbsSyn16
		 ([]
	)

happyReduce_21 = happySpecReduce_3  16# happyReduction_21
happyReduction_21 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  17# happyReduction_22
happyReduction_22 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  17# happyReduction_23
happyReduction_23 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_3 : happy_var_1
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  18# happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn18
		 (FieldUnique
	)

happyReduce_25 = happySpecReduce_2  18# happyReduction_25
happyReduction_25 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (FieldDefaultValue happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  18# happyReduction_26
happyReduction_26 (HappyTerminal (Tk _ (TLowerId happy_var_2)))
	_
	 =  HappyAbsSyn18
		 (FieldCheck happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  19# happyReduction_27
happyReduction_27 (HappyTerminal (Tk _ (TString happy_var_1)))
	 =  HappyAbsSyn19
		 (StringValue happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  19# happyReduction_28
happyReduction_28 (HappyTerminal (Tk _ (TInt happy_var_1)))
	 =  HappyAbsSyn19
		 (IntValue happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  19# happyReduction_29
happyReduction_29 (HappyTerminal (Tk _ (TFloat happy_var_1)))
	 =  HappyAbsSyn19
		 (FloatValue happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_0  20# happyReduction_30
happyReduction_30  =  HappyAbsSyn20
		 ([]
	)

happyReduce_31 = happySpecReduce_3  20# happyReduction_31
happyReduction_31 _
	(HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_2 : happy_var_1
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happyReduce 5# 21# happyReduction_32
happyReduction_32 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Index happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_0  22# happyReduction_33
happyReduction_33  =  HappyAbsSyn22
		 (False
	)

happyReduce_34 = happySpecReduce_1  22# happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn22
		 (True
	)

happyReduce_35 = happySpecReduce_1  23# happyReduction_35
happyReduction_35 (HappyTerminal (Tk _ (TLowerId happy_var_1)))
	 =  HappyAbsSyn23
		 ([happy_var_1]
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  23# happyReduction_36
happyReduction_36 (HappyTerminal (Tk _ (TLowerId happy_var_3)))
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_3:  happy_var_1
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  24# happyReduction_37
happyReduction_37  =  HappyAbsSyn24
		 (Nothing
	)

happyReduce_38 = happyReduce 4# 24# happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyTerminal (Tk _ (TLowerId happy_var_3))) `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (Just (BackRefField happy_var_2 happy_var_3)
	) `HappyStk` happyRest

happyReduce_39 = happySpecReduce_0  25# happyReduction_39
happyReduction_39  =  HappyAbsSyn25
		 (One
	)

happyReduce_40 = happySpecReduce_2  25# happyReduction_40
happyReduction_40 _
	_
	 =  HappyAbsSyn25
		 (Many
	)

happyReduce_41 = happySpecReduce_1  26# happyReduction_41
happyReduction_41 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  26# happyReduction_42
happyReduction_42 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  26# happyReduction_43
happyReduction_43 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  26# happyReduction_44
happyReduction_44 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  26# happyReduction_45
happyReduction_45 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  26# happyReduction_46
happyReduction_46 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  26# happyReduction_47
happyReduction_47 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  26# happyReduction_48
happyReduction_48 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  26# happyReduction_49
happyReduction_49 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  26# happyReduction_50
happyReduction_50 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  26# happyReduction_51
happyReduction_51 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  26# happyReduction_52
happyReduction_52 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  26# happyReduction_53
happyReduction_53 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  26# happyReduction_54
happyReduction_54 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  26# happyReduction_55
happyReduction_55 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  26# happyReduction_56
happyReduction_56 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_0  27# happyReduction_57
happyReduction_57  =  HappyAbsSyn27
		 (False
	)

happyReduce_58 = happySpecReduce_1  27# happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn27
		 (True
	)

happyNewToken action sts stk [] =
	action 68# 68# notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Tk _ TImport -> cont 28#;
	Tk _ TEntity -> cont 29#;
	Tk _ TIface -> cont 30#;
	Tk _ TRelation -> cont 31#;
	Tk _ TImplements -> cont 32#;
	Tk _ TDefault -> cont 33#;
	Tk _ TIndex -> cont 34#;
	Tk _ TUnique -> cont 35#;
	Tk _ TCheck -> cont 36#;
	Tk _ (TLowerId happy_dollar_dollar) -> cont 37#;
	Tk _ (TUpperId happy_dollar_dollar) -> cont 38#;
	Tk _ (TInt happy_dollar_dollar) -> cont 39#;
	Tk _ (TFloat happy_dollar_dollar) -> cont 40#;
	Tk _ TSemicolon -> cont 41#;
	Tk _ TLBrace -> cont 42#;
	Tk _ TRBrace -> cont 43#;
	Tk _ TLParen -> cont 44#;
	Tk _ TRParen -> cont 45#;
	Tk _ TLBrack -> cont 46#;
	Tk _ TRBrack -> cont 47#;
	Tk _ TComma -> cont 48#;
	Tk _ (TString happy_dollar_dollar) -> cont 49#;
	Tk _ TWord32 -> cont 50#;
	Tk _ TWord64 -> cont 51#;
	Tk _ TInt32 -> cont 52#;
	Tk _ TInt64 -> cont 53#;
	Tk _ TInteger -> cont 54#;
	Tk _ TStringType -> cont 55#;
	Tk _ TBool -> cont 56#;
	Tk _ TChar -> cont 57#;
	Tk _ TRational -> cont 58#;
	Tk _ TDouble -> cont 59#;
	Tk _ TLocalTime -> cont 60#;
	Tk _ TLocalDate -> cont 61#;
	Tk _ TLocalDateTime -> cont 62#;
	Tk _ TZonedTime -> cont 63#;
	Tk _ TZonedDateTime -> cont 64#;
	Tk _ TUTCDateTime -> cont 65#;
	Tk _ TPosixTime -> cont 66#;
	Tk _ TOptional -> cont 67#;
	_ -> happyError' (tk:tks)
	}

happyError_ 68# tk tks = happyError' tks
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
