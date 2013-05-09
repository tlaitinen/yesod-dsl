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

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40
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
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33
	| HappyAbsSyn34 t34
	| HappyAbsSyn35 t35
	| HappyAbsSyn36 t36
	| HappyAbsSyn37 t37
	| HappyAbsSyn38 t38
	| HappyAbsSyn39 t39
	| HappyAbsSyn40 t40

action_0 (4#) = happyGoto action_3
action_0 (5#) = happyGoto action_2
action_0 x = happyTcHack x happyReduce_2

action_1 (5#) = happyGoto action_2
action_1 x = happyTcHack x happyFail

action_2 (41#) = happyShift action_6
action_2 (6#) = happyGoto action_4
action_2 (7#) = happyGoto action_5
action_2 x = happyTcHack x happyReduce_5

action_3 (98#) = happyAccept
action_3 x = happyTcHack x happyFail

action_4 x = happyTcHack x happyReduce_3

action_5 (42#) = happyShift action_12
action_5 (44#) = happyShift action_13
action_5 (45#) = happyShift action_14
action_5 (8#) = happyGoto action_8
action_5 (9#) = happyGoto action_9
action_5 (11#) = happyGoto action_10
action_5 (25#) = happyGoto action_11
action_5 x = happyTcHack x happyReduce_1

action_6 (64#) = happyShift action_7
action_6 x = happyTcHack x happyFail

action_7 (52#) = happyShift action_18
action_7 x = happyTcHack x happyFail

action_8 x = happyTcHack x happyReduce_6

action_9 x = happyTcHack x happyReduce_9

action_10 x = happyTcHack x happyReduce_7

action_11 x = happyTcHack x happyReduce_8

action_12 (49#) = happyShift action_17
action_12 x = happyTcHack x happyFail

action_13 (49#) = happyShift action_16
action_13 x = happyTcHack x happyFail

action_14 (49#) = happyShift action_15
action_14 x = happyTcHack x happyFail

action_15 (54#) = happyShift action_22
action_15 x = happyTcHack x happyFail

action_16 (61#) = happyShift action_21
action_16 (23#) = happyGoto action_20
action_16 x = happyTcHack x happyReduce_45

action_17 (53#) = happyShift action_19
action_17 x = happyTcHack x happyFail

action_18 x = happyTcHack x happyReduce_4

action_19 (49#) = happyShift action_28
action_19 (10#) = happyGoto action_27
action_19 x = happyTcHack x happyFail

action_20 (54#) = happyShift action_26
action_20 x = happyTcHack x happyFail

action_21 (49#) = happyShift action_25
action_21 (24#) = happyGoto action_24
action_21 x = happyTcHack x happyFail

action_22 (26#) = happyGoto action_23
action_22 x = happyTcHack x happyReduce_50

action_23 (48#) = happyShift action_35
action_23 (27#) = happyGoto action_33
action_23 (32#) = happyGoto action_34
action_23 x = happyTcHack x happyReduce_63

action_24 (60#) = happyShift action_32
action_24 x = happyTcHack x happyReduce_46

action_25 x = happyTcHack x happyReduce_47

action_26 (26#) = happyGoto action_31
action_26 x = happyTcHack x happyReduce_50

action_27 (43#) = happyShift action_29
action_27 (52#) = happyShift action_30
action_27 x = happyTcHack x happyFail

action_28 x = happyTcHack x happyReduce_11

action_29 (49#) = happyShift action_44
action_29 x = happyTcHack x happyFail

action_30 x = happyTcHack x happyReduce_10

action_31 (48#) = happyShift action_35
action_31 (27#) = happyGoto action_33
action_31 (32#) = happyGoto action_43
action_31 x = happyTcHack x happyReduce_63

action_32 (49#) = happyShift action_42
action_32 x = happyTcHack x happyFail

action_33 (52#) = happyShift action_41
action_33 x = happyTcHack x happyFail

action_34 (46#) = happyShift action_39
action_34 (55#) = happyShift action_40
action_34 (33#) = happyGoto action_38
action_34 x = happyTcHack x happyFail

action_35 (76#) = happyShift action_37
action_35 (40#) = happyGoto action_36
action_35 x = happyTcHack x happyReduce_85

action_36 (49#) = happyShift action_49
action_36 (65#) = happyShift action_50
action_36 (66#) = happyShift action_51
action_36 (67#) = happyShift action_52
action_36 (68#) = happyShift action_53
action_36 (69#) = happyShift action_54
action_36 (70#) = happyShift action_55
action_36 (71#) = happyShift action_56
action_36 (72#) = happyShift action_57
action_36 (73#) = happyShift action_58
action_36 (74#) = happyShift action_59
action_36 (75#) = happyShift action_60
action_36 (39#) = happyGoto action_48
action_36 x = happyTcHack x happyFail

action_37 x = happyTcHack x happyReduce_86

action_38 (52#) = happyShift action_47
action_38 x = happyTcHack x happyFail

action_39 (49#) = happyShift action_46
action_39 x = happyTcHack x happyFail

action_40 x = happyTcHack x happyReduce_49

action_41 x = happyTcHack x happyReduce_51

action_42 x = happyTcHack x happyReduce_48

action_43 (46#) = happyShift action_39
action_43 (33#) = happyGoto action_38
action_43 (34#) = happyGoto action_45
action_43 x = happyTcHack x happyReduce_66

action_44 x = happyTcHack x happyReduce_12

action_45 (96#) = happyShift action_70
action_45 (35#) = happyGoto action_68
action_45 (36#) = happyGoto action_69
action_45 x = happyTcHack x happyReduce_69

action_46 (48#) = happyShift action_67
action_46 (38#) = happyGoto action_66
action_46 x = happyTcHack x happyReduce_72

action_47 x = happyTcHack x happyReduce_64

action_48 (47#) = happyShift action_64
action_48 (97#) = happyShift action_65
action_48 (28#) = happyGoto action_61
action_48 (29#) = happyGoto action_62
action_48 (30#) = happyGoto action_63
action_48 x = happyTcHack x happyReduce_54

action_49 x = happyTcHack x happyReduce_53

action_50 x = happyTcHack x happyReduce_74

action_51 x = happyTcHack x happyReduce_75

action_52 x = happyTcHack x happyReduce_76

action_53 x = happyTcHack x happyReduce_77

action_54 x = happyTcHack x happyReduce_78

action_55 x = happyTcHack x happyReduce_79

action_56 x = happyTcHack x happyReduce_80

action_57 x = happyTcHack x happyReduce_81

action_58 x = happyTcHack x happyReduce_82

action_59 x = happyTcHack x happyReduce_83

action_60 x = happyTcHack x happyReduce_84

action_61 x = happyTcHack x happyReduce_52

action_62 (47#) = happyShift action_64
action_62 (97#) = happyShift action_65
action_62 (30#) = happyGoto action_79
action_62 x = happyTcHack x happyReduce_55

action_63 x = happyTcHack x happyReduce_56

action_64 (48#) = happyShift action_78
action_64 x = happyTcHack x happyFail

action_65 (64#) = happyShift action_77
action_65 x = happyTcHack x happyFail

action_66 x = happyTcHack x happyReduce_65

action_67 (48#) = happyShift action_67
action_67 (38#) = happyGoto action_76
action_67 x = happyTcHack x happyReduce_72

action_68 (52#) = happyShift action_75
action_68 x = happyTcHack x happyFail

action_69 (47#) = happyShift action_74
action_69 (12#) = happyGoto action_72
action_69 (37#) = happyGoto action_73
action_69 x = happyTcHack x happyReduce_14

action_70 (49#) = happyShift action_71
action_70 x = happyTcHack x happyFail

action_71 x = happyTcHack x happyReduce_68

action_72 (55#) = happyShift action_83
action_72 (77#) = happyShift action_84
action_72 (78#) = happyShift action_85
action_72 (79#) = happyShift action_86
action_72 (80#) = happyShift action_87
action_72 (88#) = happyShift action_88
action_72 (13#) = happyGoto action_82
action_72 x = happyTcHack x happyFail

action_73 (52#) = happyShift action_81
action_73 x = happyTcHack x happyFail

action_74 (48#) = happyShift action_80
action_74 x = happyTcHack x happyFail

action_75 x = happyTcHack x happyReduce_67

action_76 x = happyTcHack x happyReduce_73

action_77 x = happyTcHack x happyReduce_59

action_78 x = happyTcHack x happyReduce_58

action_79 x = happyTcHack x happyReduce_57

action_80 x = happyTcHack x happyReduce_71

action_81 x = happyTcHack x happyReduce_70

action_82 x = happyTcHack x happyReduce_15

action_83 x = happyTcHack x happyReduce_13

action_84 (54#) = happyShift action_90
action_84 (63#) = happyShift action_95
action_84 (17#) = happyGoto action_94
action_84 x = happyTcHack x happyFail

action_85 (54#) = happyShift action_90
action_85 (17#) = happyGoto action_93
action_85 x = happyTcHack x happyFail

action_86 (54#) = happyShift action_90
action_86 (17#) = happyGoto action_92
action_86 x = happyTcHack x happyFail

action_87 (54#) = happyShift action_90
action_87 (17#) = happyGoto action_91
action_87 x = happyTcHack x happyFail

action_88 (54#) = happyShift action_90
action_88 (17#) = happyGoto action_89
action_88 x = happyTcHack x happyFail

action_89 x = happyTcHack x happyReduce_21

action_90 (18#) = happyGoto action_97
action_90 x = happyTcHack x happyReduce_28

action_91 x = happyTcHack x happyReduce_20

action_92 x = happyTcHack x happyReduce_19

action_93 x = happyTcHack x happyReduce_18

action_94 x = happyTcHack x happyReduce_16

action_95 (48#) = happyShift action_96
action_95 x = happyTcHack x happyFail

action_96 (86#) = happyShift action_112
action_96 (14#) = happyGoto action_110
action_96 (15#) = happyGoto action_111
action_96 x = happyTcHack x happyFail

action_97 (55#) = happyShift action_99
action_97 (81#) = happyShift action_100
action_97 (82#) = happyShift action_101
action_97 (83#) = happyShift action_102
action_97 (84#) = happyShift action_103
action_97 (85#) = happyShift action_104
action_97 (89#) = happyShift action_105
action_97 (90#) = happyShift action_106
action_97 (91#) = happyShift action_107
action_97 (94#) = happyShift action_108
action_97 (95#) = happyShift action_109
action_97 (19#) = happyGoto action_98
action_97 x = happyTcHack x happyFail

action_98 (52#) = happyShift action_126
action_98 x = happyTcHack x happyFail

action_99 x = happyTcHack x happyReduce_27

action_100 x = happyTcHack x happyReduce_30

action_101 (48#) = happyShift action_125
action_101 x = happyTcHack x happyFail

action_102 (48#) = happyShift action_124
action_102 x = happyTcHack x happyFail

action_103 (48#) = happyShift action_123
action_103 x = happyTcHack x happyFail

action_104 (48#) = happyShift action_122
action_104 x = happyTcHack x happyFail

action_105 x = happyTcHack x happyReduce_35

action_106 (64#) = happyShift action_121
action_106 x = happyTcHack x happyFail

action_107 (48#) = happyShift action_120
action_107 (20#) = happyGoto action_118
action_107 (21#) = happyGoto action_119
action_107 x = happyTcHack x happyFail

action_108 (48#) = happyShift action_117
action_108 x = happyTcHack x happyFail

action_109 (48#) = happyShift action_116
action_109 x = happyTcHack x happyFail

action_110 (54#) = happyShift action_90
action_110 (86#) = happyShift action_112
action_110 (15#) = happyGoto action_114
action_110 (17#) = happyGoto action_115
action_110 x = happyTcHack x happyFail

action_111 x = happyTcHack x happyReduce_22

action_112 (49#) = happyShift action_113
action_112 x = happyTcHack x happyFail

action_113 (87#) = happyShift action_132
action_113 x = happyTcHack x happyFail

action_114 x = happyTcHack x happyReduce_23

action_115 x = happyTcHack x happyReduce_17

action_116 x = happyTcHack x happyReduce_39

action_117 x = happyTcHack x happyReduce_38

action_118 (48#) = happyShift action_120
action_118 (21#) = happyGoto action_131
action_118 x = happyTcHack x happyReduce_37

action_119 x = happyTcHack x happyReduce_40

action_120 (92#) = happyShift action_129
action_120 (93#) = happyShift action_130
action_120 (22#) = happyGoto action_128
action_120 x = happyTcHack x happyFail

action_121 (48#) = happyShift action_67
action_121 (38#) = happyGoto action_127
action_121 x = happyTcHack x happyReduce_72

action_122 x = happyTcHack x happyReduce_32

action_123 x = happyTcHack x happyReduce_31

action_124 x = happyTcHack x happyReduce_34

action_125 x = happyTcHack x happyReduce_33

action_126 x = happyTcHack x happyReduce_29

action_127 x = happyTcHack x happyReduce_36

action_128 x = happyTcHack x happyReduce_42

action_129 x = happyTcHack x happyReduce_43

action_130 x = happyTcHack x happyReduce_44

action_131 x = happyTcHack x happyReduce_41

action_132 (49#) = happyShift action_134
action_132 (16#) = happyGoto action_133
action_132 x = happyTcHack x happyFail

action_133 (53#) = happyShift action_136
action_133 x = happyTcHack x happyFail

action_134 (62#) = happyShift action_135
action_134 x = happyTcHack x happyReduce_25

action_135 (48#) = happyShift action_138
action_135 x = happyTcHack x happyFail

action_136 (49#) = happyShift action_134
action_136 (16#) = happyGoto action_137
action_136 x = happyTcHack x happyFail

action_137 x = happyTcHack x happyReduce_24

action_138 x = happyTcHack x happyReduce_26

happyReduce_1 = happySpecReduce_2  4# happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (DbModule happy_var_1 (getEntities happy_var_2) 
                                        (getClasses happy_var_2)
                                        (getEnums happy_var_2)
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
happyReduction_7 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn8
		 (EntityDef happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8# happyReduction_8
happyReduction_8 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn8
		 (ClassDef happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  8# happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (EnumDef happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happyReduce 5# 9# happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tk _ (TUpperId happy_var_2))) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (DbEnum (mkLoc happy_var_1) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  10# happyReduction_11
happyReduction_11 (HappyTerminal (Tk _ (TUpperId happy_var_1)))
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  10# happyReduction_12
happyReduction_12 (HappyTerminal (Tk _ (TUpperId happy_var_3)))
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_3 : happy_var_1
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 10# 11# happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_9) `HappyStk`
	(HappyAbsSyn36  happy_var_8) `HappyStk`
	(HappyAbsSyn34  happy_var_7) `HappyStk`
	(HappyAbsSyn32  happy_var_6) `HappyStk`
	(HappyAbsSyn26  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	(HappyTerminal (Tk _ (TUpperId happy_var_2))) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Entity (mkLoc happy_var_1) happy_var_2 happy_var_3 happy_var_5 happy_var_6 happy_var_7 happy_var_8 happy_var_9
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_0  12# happyReduction_14
happyReduction_14  =  HappyAbsSyn12
		 ([]
	)

happyReduce_15 = happySpecReduce_2  12# happyReduction_15
happyReduction_15 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_2 : happy_var_1
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  13# happyReduction_16
happyReduction_16 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Service GetService happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 5# 13# happyReduction_17
happyReduction_17 ((HappyAbsSyn17  happy_var_5) `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	(HappyTerminal (Tk _ (TLowerId happy_var_3))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Service (GetServiceNested happy_var_3 happy_var_4) happy_var_5
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_2  13# happyReduction_18
happyReduction_18 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Service PutService happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  13# happyReduction_19
happyReduction_19 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Service PostService happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  13# happyReduction_20
happyReduction_20 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Service DeleteService happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  13# happyReduction_21
happyReduction_21 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Service ValidateService happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  14# happyReduction_22
happyReduction_22 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  14# happyReduction_23
happyReduction_23 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_2 : happy_var_1
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 6# 15# happyReduction_24
happyReduction_24 ((HappyAbsSyn16  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tk _ (TUpperId happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Join happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_1  16# happyReduction_25
happyReduction_25 (HappyTerminal (Tk _ (TUpperId happy_var_1)))
	 =  HappyAbsSyn16
		 (FieldPathId happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  16# happyReduction_26
happyReduction_26 (HappyTerminal (Tk _ (TLowerId happy_var_3)))
	_
	(HappyTerminal (Tk _ (TUpperId happy_var_1)))
	 =  HappyAbsSyn16
		 (FieldPathNormal happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  17# happyReduction_27
happyReduction_27 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_0  18# happyReduction_28
happyReduction_28  =  HappyAbsSyn18
		 ([]
	)

happyReduce_29 = happySpecReduce_3  18# happyReduction_29
happyReduction_29 _
	(HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_2 : happy_var_1
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  19# happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn19
		 (PublicService
	)

happyReduce_31 = happySpecReduce_2  19# happyReduction_31
happyReduction_31 (HappyTerminal (Tk _ (TLowerId happy_var_2)))
	_
	 =  HappyAbsSyn19
		 (ServicePreHook happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  19# happyReduction_32
happyReduction_32 (HappyTerminal (Tk _ (TLowerId happy_var_2)))
	_
	 =  HappyAbsSyn19
		 (ServicePostHook happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  19# happyReduction_33
happyReduction_33 (HappyTerminal (Tk _ (TLowerId happy_var_2)))
	_
	 =  HappyAbsSyn19
		 (ServicePreTransform happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_2  19# happyReduction_34
happyReduction_34 (HappyTerminal (Tk _ (TLowerId happy_var_2)))
	_
	 =  HappyAbsSyn19
		 (ServicePostTransform happy_var_2
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  19# happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn19
		 (ServiceDefaultFilterSort
	)

happyReduce_36 = happySpecReduce_3  19# happyReduction_36
happyReduction_36 (HappyAbsSyn38  happy_var_3)
	(HappyTerminal (Tk _ (TString happy_var_2)))
	_
	 =  HappyAbsSyn19
		 (ServiceTextSearchFilter happy_var_2 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  19# happyReduction_37
happyReduction_37 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (ServiceSortBy happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_2  19# happyReduction_38
happyReduction_38 (HappyTerminal (Tk _ (TLowerId happy_var_2)))
	_
	 =  HappyAbsSyn19
		 (ServiceFilter happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  19# happyReduction_39
happyReduction_39 (HappyTerminal (Tk _ (TLowerId happy_var_2)))
	_
	 =  HappyAbsSyn19
		 (ServiceSelectOpts happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  20# happyReduction_40
happyReduction_40 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 ([happy_var_1]
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  20# happyReduction_41
happyReduction_41 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_2 : happy_var_1
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_2  21# happyReduction_42
happyReduction_42 (HappyAbsSyn22  happy_var_2)
	(HappyTerminal (Tk _ (TLowerId happy_var_1)))
	 =  HappyAbsSyn21
		 ((happy_var_1, happy_var_2)
	)
happyReduction_42 _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  22# happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn22
		 (SortAsc
	)

happyReduce_44 = happySpecReduce_1  22# happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn22
		 (SortDesc
	)

happyReduce_45 = happySpecReduce_0  23# happyReduction_45
happyReduction_45  =  HappyAbsSyn23
		 ([]
	)

happyReduce_46 = happySpecReduce_2  23# happyReduction_46
happyReduction_46 (HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  24# happyReduction_47
happyReduction_47 (HappyTerminal (Tk _ (TUpperId happy_var_1)))
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  24# happyReduction_48
happyReduction_48 (HappyTerminal (Tk _ (TUpperId happy_var_3)))
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_3 : happy_var_1
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happyReduce 6# 25# happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn32  happy_var_5) `HappyStk`
	(HappyAbsSyn26  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tk _ (TUpperId happy_var_2))) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (Class (mkLoc happy_var_1) happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_0  26# happyReduction_50
happyReduction_50  =  HappyAbsSyn26
		 ([]
	)

happyReduce_51 = happySpecReduce_3  26# happyReduction_51
happyReduction_51 _
	(HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_2 : happy_var_1
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happyReduce 4# 27# happyReduction_52
happyReduction_52 ((HappyAbsSyn28  happy_var_4) `HappyStk`
	(HappyAbsSyn39  happy_var_3) `HappyStk`
	(HappyAbsSyn40  happy_var_2) `HappyStk`
	(HappyTerminal (Tk _ (TLowerId happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Field happy_var_2 happy_var_1 (NormalField (tokenType happy_var_3) happy_var_4)
	) `HappyStk` happyRest

happyReduce_53 = happySpecReduce_3  27# happyReduction_53
happyReduction_53 (HappyTerminal (Tk _ (TUpperId happy_var_3)))
	(HappyAbsSyn40  happy_var_2)
	(HappyTerminal (Tk _ (TLowerId happy_var_1)))
	 =  HappyAbsSyn27
		 (Field happy_var_2 happy_var_1 (EntityField happy_var_3)
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_0  28# happyReduction_54
happyReduction_54  =  HappyAbsSyn28
		 ([]
	)

happyReduce_55 = happySpecReduce_1  28# happyReduction_55
happyReduction_55 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  29# happyReduction_56
happyReduction_56 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 ([happy_var_1]
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_2  29# happyReduction_57
happyReduction_57 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_2 : happy_var_1
	)
happyReduction_57 _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  30# happyReduction_58
happyReduction_58 (HappyTerminal (Tk _ (TLowerId happy_var_2)))
	_
	 =  HappyAbsSyn30
		 (FieldCheck happy_var_2
	)
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_2  30# happyReduction_59
happyReduction_59 (HappyTerminal (Tk _ (TString happy_var_2)))
	_
	 =  HappyAbsSyn30
		 (FieldDefault happy_var_2
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  31# happyReduction_60
happyReduction_60 (HappyTerminal (Tk _ (TString happy_var_1)))
	 =  HappyAbsSyn31
		 (StringValue happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  31# happyReduction_61
happyReduction_61 (HappyTerminal (Tk _ (TInt happy_var_1)))
	 =  HappyAbsSyn31
		 (IntValue happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  31# happyReduction_62
happyReduction_62 (HappyTerminal (Tk _ (TFloat happy_var_1)))
	 =  HappyAbsSyn31
		 (FloatValue happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_0  32# happyReduction_63
happyReduction_63  =  HappyAbsSyn32
		 ([]
	)

happyReduce_64 = happySpecReduce_3  32# happyReduction_64
happyReduction_64 _
	(HappyAbsSyn33  happy_var_2)
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_2 : happy_var_1
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  33# happyReduction_65
happyReduction_65 (HappyAbsSyn38  happy_var_3)
	(HappyTerminal (Tk _ (TUpperId happy_var_2)))
	_
	 =  HappyAbsSyn33
		 (Unique happy_var_2 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_0  34# happyReduction_66
happyReduction_66  =  HappyAbsSyn34
		 ([]
	)

happyReduce_67 = happySpecReduce_3  34# happyReduction_67
happyReduction_67 _
	(HappyAbsSyn35  happy_var_2)
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_2 : happy_var_1
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_2  35# happyReduction_68
happyReduction_68 (HappyTerminal (Tk _ (TUpperId happy_var_2)))
	_
	 =  HappyAbsSyn35
		 (happy_var_2
	)
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_0  36# happyReduction_69
happyReduction_69  =  HappyAbsSyn36
		 ([]
	)

happyReduce_70 = happySpecReduce_3  36# happyReduction_70
happyReduction_70 _
	(HappyAbsSyn37  happy_var_2)
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_2 : happy_var_1
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_2  37# happyReduction_71
happyReduction_71 (HappyTerminal (Tk _ (TLowerId happy_var_2)))
	_
	 =  HappyAbsSyn37
		 (happy_var_2
	)
happyReduction_71 _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_0  38# happyReduction_72
happyReduction_72  =  HappyAbsSyn38
		 ([]
	)

happyReduce_73 = happySpecReduce_2  38# happyReduction_73
happyReduction_73 (HappyAbsSyn38  happy_var_2)
	(HappyTerminal (Tk _ (TLowerId happy_var_1)))
	 =  HappyAbsSyn38
		 (happy_var_1 : happy_var_2
	)
happyReduction_73 _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  39# happyReduction_74
happyReduction_74 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  39# happyReduction_75
happyReduction_75 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  39# happyReduction_76
happyReduction_76 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  39# happyReduction_77
happyReduction_77 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  39# happyReduction_78
happyReduction_78 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  39# happyReduction_79
happyReduction_79 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  39# happyReduction_80
happyReduction_80 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  39# happyReduction_81
happyReduction_81 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  39# happyReduction_82
happyReduction_82 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  39# happyReduction_83
happyReduction_83 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  39# happyReduction_84
happyReduction_84 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_0  40# happyReduction_85
happyReduction_85  =  HappyAbsSyn40
		 (False
	)

happyReduce_86 = happySpecReduce_1  40# happyReduction_86
happyReduction_86 _
	 =  HappyAbsSyn40
		 (True
	)

happyNewToken action sts stk [] =
	action 98# 98# notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Tk _ TImport -> cont 41#;
	Tk _ TEnum -> cont 42#;
	Tk _ TPipe -> cont 43#;
	Tk _ TEntity -> cont 44#;
	Tk _ TClass -> cont 45#;
	Tk _ TUnique -> cont 46#;
	Tk _ TCheck -> cont 47#;
	Tk _ (TLowerId happy_dollar_dollar) -> cont 48#;
	Tk _ (TUpperId happy_dollar_dollar) -> cont 49#;
	Tk _ (TInt happy_dollar_dollar) -> cont 50#;
	Tk _ (TFloat happy_dollar_dollar) -> cont 51#;
	Tk _ TSemicolon -> cont 52#;
	Tk _ TEquals -> cont 53#;
	Tk _ TLBrace -> cont 54#;
	Tk _ TRBrace -> cont 55#;
	Tk _ TLParen -> cont 56#;
	Tk _ TRParen -> cont 57#;
	Tk _ TLBrack -> cont 58#;
	Tk _ TRBrack -> cont 59#;
	Tk _ TComma -> cont 60#;
	Tk _ TColon -> cont 61#;
	Tk _ TDot -> cont 62#;
	Tk _ TSlash -> cont 63#;
	Tk _ (TString happy_dollar_dollar) -> cont 64#;
	Tk _ TWord32 -> cont 65#;
	Tk _ TWord64 -> cont 66#;
	Tk _ TInt32 -> cont 67#;
	Tk _ TInt64 -> cont 68#;
	Tk _ TText -> cont 69#;
	Tk _ TBool -> cont 70#;
	Tk _ TDouble -> cont 71#;
	Tk _ TTime -> cont 72#;
	Tk _ TDate -> cont 73#;
	Tk _ TDateTime -> cont 74#;
	Tk _ TZonedTime -> cont 75#;
	Tk _ TMaybe -> cont 76#;
	Tk _ TGet -> cont 77#;
	Tk _ TPut -> cont 78#;
	Tk _ TPost -> cont 79#;
	Tk _ TDelete -> cont 80#;
	Tk _ TPublic -> cont 81#;
	Tk _ TPreTransform -> cont 82#;
	Tk _ TPostTransform -> cont 83#;
	Tk _ TPreHook -> cont 84#;
	Tk _ TPostHook -> cont 85#;
	Tk _ TJoin -> cont 86#;
	Tk _ TOn -> cont 87#;
	Tk _ TValidate -> cont 88#;
	Tk _ TDefaultFilterSort -> cont 89#;
	Tk _ TTextSearchFilter -> cont 90#;
	Tk _ TSortBy -> cont 91#;
	Tk _ TAsc -> cont 92#;
	Tk _ TDesc -> cont 93#;
	Tk _ TFilter -> cont 94#;
	Tk _ TSelectOpts -> cont 95#;
	Tk _ TDeriving -> cont 96#;
	Tk _ TDefault -> cont 97#;
	_ -> happyError' (tk:tks)
	}

happyError_ 98# tk tks = happyError' tks
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
