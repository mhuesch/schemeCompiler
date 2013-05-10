{-# OPTIONS_GHC -w #-}
module L3.Parser where


import Control.Monad.Error

import L3.Token
import L3.Grammar

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13
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

action_0 (14) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (14) = happyShift action_2
action_1 _ = happyFail

action_2 (14) = happyShift action_9
action_2 (27) = happyShift action_10
action_2 (28) = happyShift action_11
action_2 (29) = happyShift action_12
action_2 (7) = happyGoto action_4
action_2 (8) = happyGoto action_5
action_2 (9) = happyGoto action_6
action_2 (10) = happyGoto action_7
action_2 (12) = happyGoto action_8
action_2 _ = happyFail

action_3 (38) = happyAccept
action_3 _ = happyFail

action_4 (5) = happyGoto action_33
action_4 _ = happyReduce_2

action_5 _ = happyReduce_7

action_6 _ = happyReduce_29

action_7 _ = happyReduce_26

action_8 _ = happyReduce_28

action_9 (16) = happyShift action_14
action_9 (17) = happyShift action_15
action_9 (18) = happyShift action_16
action_9 (19) = happyShift action_17
action_9 (20) = happyShift action_18
action_9 (21) = happyShift action_19
action_9 (22) = happyShift action_20
action_9 (23) = happyShift action_21
action_9 (24) = happyShift action_22
action_9 (25) = happyShift action_23
action_9 (26) = happyShift action_24
action_9 (27) = happyShift action_10
action_9 (28) = happyShift action_11
action_9 (29) = happyShift action_12
action_9 (30) = happyShift action_25
action_9 (31) = happyShift action_26
action_9 (32) = happyShift action_27
action_9 (33) = happyShift action_28
action_9 (34) = happyShift action_29
action_9 (35) = happyShift action_30
action_9 (36) = happyShift action_31
action_9 (37) = happyShift action_32
action_9 (9) = happyGoto action_6
action_9 (10) = happyGoto action_13
action_9 (12) = happyGoto action_8
action_9 _ = happyFail

action_10 _ = happyReduce_30

action_11 _ = happyReduce_33

action_12 _ = happyReduce_27

action_13 (11) = happyGoto action_56
action_13 _ = happyReduce_31

action_14 (14) = happyShift action_55
action_14 _ = happyFail

action_15 (27) = happyShift action_10
action_15 (28) = happyShift action_11
action_15 (29) = happyShift action_12
action_15 (9) = happyGoto action_6
action_15 (10) = happyGoto action_54
action_15 (12) = happyGoto action_8
action_15 _ = happyFail

action_16 (27) = happyShift action_10
action_16 (28) = happyShift action_11
action_16 (29) = happyShift action_12
action_16 (9) = happyGoto action_6
action_16 (10) = happyGoto action_53
action_16 (12) = happyGoto action_8
action_16 _ = happyFail

action_17 (27) = happyShift action_10
action_17 (28) = happyShift action_11
action_17 (29) = happyShift action_12
action_17 (9) = happyGoto action_6
action_17 (10) = happyGoto action_52
action_17 (12) = happyGoto action_8
action_17 _ = happyFail

action_18 (11) = happyGoto action_51
action_18 _ = happyReduce_31

action_19 (27) = happyShift action_10
action_19 (28) = happyShift action_11
action_19 (29) = happyShift action_12
action_19 (9) = happyGoto action_6
action_19 (10) = happyGoto action_50
action_19 (12) = happyGoto action_8
action_19 _ = happyFail

action_20 (27) = happyShift action_10
action_20 (28) = happyShift action_11
action_20 (29) = happyShift action_12
action_20 (9) = happyGoto action_6
action_20 (10) = happyGoto action_49
action_20 (12) = happyGoto action_8
action_20 _ = happyFail

action_21 (27) = happyShift action_10
action_21 (28) = happyShift action_11
action_21 (29) = happyShift action_12
action_21 (9) = happyGoto action_6
action_21 (10) = happyGoto action_48
action_21 (12) = happyGoto action_8
action_21 _ = happyFail

action_22 (29) = happyShift action_12
action_22 (9) = happyGoto action_47
action_22 _ = happyFail

action_23 (27) = happyShift action_10
action_23 (28) = happyShift action_11
action_23 (29) = happyShift action_12
action_23 (9) = happyGoto action_6
action_23 (10) = happyGoto action_46
action_23 (12) = happyGoto action_8
action_23 _ = happyFail

action_24 (27) = happyShift action_10
action_24 (28) = happyShift action_11
action_24 (29) = happyShift action_12
action_24 (9) = happyGoto action_6
action_24 (10) = happyGoto action_45
action_24 (12) = happyGoto action_8
action_24 _ = happyFail

action_25 (27) = happyShift action_10
action_25 (28) = happyShift action_11
action_25 (29) = happyShift action_12
action_25 (9) = happyGoto action_6
action_25 (10) = happyGoto action_44
action_25 (12) = happyGoto action_8
action_25 _ = happyFail

action_26 (27) = happyShift action_10
action_26 (28) = happyShift action_11
action_26 (29) = happyShift action_12
action_26 (9) = happyGoto action_6
action_26 (10) = happyGoto action_43
action_26 (12) = happyGoto action_8
action_26 _ = happyFail

action_27 (27) = happyShift action_10
action_27 (28) = happyShift action_11
action_27 (29) = happyShift action_12
action_27 (9) = happyGoto action_6
action_27 (10) = happyGoto action_42
action_27 (12) = happyGoto action_8
action_27 _ = happyFail

action_28 (27) = happyShift action_10
action_28 (28) = happyShift action_11
action_28 (29) = happyShift action_12
action_28 (9) = happyGoto action_6
action_28 (10) = happyGoto action_41
action_28 (12) = happyGoto action_8
action_28 _ = happyFail

action_29 (27) = happyShift action_10
action_29 (28) = happyShift action_11
action_29 (29) = happyShift action_12
action_29 (9) = happyGoto action_6
action_29 (10) = happyGoto action_40
action_29 (12) = happyGoto action_8
action_29 _ = happyFail

action_30 (27) = happyShift action_10
action_30 (28) = happyShift action_11
action_30 (29) = happyShift action_12
action_30 (9) = happyGoto action_6
action_30 (10) = happyGoto action_39
action_30 (12) = happyGoto action_8
action_30 _ = happyFail

action_31 (27) = happyShift action_10
action_31 (28) = happyShift action_11
action_31 (29) = happyShift action_12
action_31 (9) = happyGoto action_6
action_31 (10) = happyGoto action_38
action_31 (12) = happyGoto action_8
action_31 _ = happyFail

action_32 (27) = happyShift action_10
action_32 (28) = happyShift action_11
action_32 (29) = happyShift action_12
action_32 (9) = happyGoto action_6
action_32 (10) = happyGoto action_37
action_32 (12) = happyGoto action_8
action_32 _ = happyFail

action_33 (14) = happyShift action_35
action_33 (15) = happyShift action_36
action_33 (6) = happyGoto action_34
action_33 _ = happyFail

action_34 _ = happyReduce_3

action_35 (29) = happyShift action_12
action_35 (9) = happyGoto action_78
action_35 _ = happyFail

action_36 _ = happyReduce_1

action_37 (27) = happyShift action_10
action_37 (28) = happyShift action_11
action_37 (29) = happyShift action_12
action_37 (9) = happyGoto action_6
action_37 (10) = happyGoto action_77
action_37 (12) = happyGoto action_8
action_37 _ = happyFail

action_38 (27) = happyShift action_10
action_38 (28) = happyShift action_11
action_38 (29) = happyShift action_12
action_38 (9) = happyGoto action_6
action_38 (10) = happyGoto action_76
action_38 (12) = happyGoto action_8
action_38 _ = happyFail

action_39 (27) = happyShift action_10
action_39 (28) = happyShift action_11
action_39 (29) = happyShift action_12
action_39 (9) = happyGoto action_6
action_39 (10) = happyGoto action_75
action_39 (12) = happyGoto action_8
action_39 _ = happyFail

action_40 (27) = happyShift action_10
action_40 (28) = happyShift action_11
action_40 (29) = happyShift action_12
action_40 (9) = happyGoto action_6
action_40 (10) = happyGoto action_74
action_40 (12) = happyGoto action_8
action_40 _ = happyFail

action_41 (27) = happyShift action_10
action_41 (28) = happyShift action_11
action_41 (29) = happyShift action_12
action_41 (9) = happyGoto action_6
action_41 (10) = happyGoto action_73
action_41 (12) = happyGoto action_8
action_41 _ = happyFail

action_42 (27) = happyShift action_10
action_42 (28) = happyShift action_11
action_42 (29) = happyShift action_12
action_42 (9) = happyGoto action_6
action_42 (10) = happyGoto action_72
action_42 (12) = happyGoto action_8
action_42 _ = happyFail

action_43 (15) = happyShift action_71
action_43 _ = happyFail

action_44 (15) = happyShift action_70
action_44 _ = happyFail

action_45 (15) = happyShift action_69
action_45 _ = happyFail

action_46 (15) = happyShift action_68
action_46 _ = happyFail

action_47 (27) = happyShift action_10
action_47 (28) = happyShift action_11
action_47 (29) = happyShift action_12
action_47 (9) = happyGoto action_6
action_47 (10) = happyGoto action_67
action_47 (12) = happyGoto action_8
action_47 _ = happyFail

action_48 (15) = happyShift action_66
action_48 _ = happyFail

action_49 (27) = happyShift action_10
action_49 (28) = happyShift action_11
action_49 (29) = happyShift action_12
action_49 (9) = happyGoto action_6
action_49 (10) = happyGoto action_65
action_49 (12) = happyGoto action_8
action_49 _ = happyFail

action_50 (27) = happyShift action_10
action_50 (28) = happyShift action_11
action_50 (29) = happyShift action_12
action_50 (9) = happyGoto action_6
action_50 (10) = happyGoto action_64
action_50 (12) = happyGoto action_8
action_50 _ = happyFail

action_51 (15) = happyShift action_63
action_51 (27) = happyShift action_10
action_51 (28) = happyShift action_11
action_51 (29) = happyShift action_12
action_51 (9) = happyGoto action_6
action_51 (10) = happyGoto action_57
action_51 (12) = happyGoto action_8
action_51 _ = happyFail

action_52 (27) = happyShift action_10
action_52 (28) = happyShift action_11
action_52 (29) = happyShift action_12
action_52 (9) = happyGoto action_6
action_52 (10) = happyGoto action_62
action_52 (12) = happyGoto action_8
action_52 _ = happyFail

action_53 (15) = happyShift action_61
action_53 _ = happyFail

action_54 (14) = happyShift action_9
action_54 (27) = happyShift action_10
action_54 (28) = happyShift action_11
action_54 (29) = happyShift action_12
action_54 (7) = happyGoto action_60
action_54 (8) = happyGoto action_5
action_54 (9) = happyGoto action_6
action_54 (10) = happyGoto action_7
action_54 (12) = happyGoto action_8
action_54 _ = happyFail

action_55 (14) = happyShift action_59
action_55 _ = happyFail

action_56 (15) = happyShift action_58
action_56 (27) = happyShift action_10
action_56 (28) = happyShift action_11
action_56 (29) = happyShift action_12
action_56 (9) = happyGoto action_6
action_56 (10) = happyGoto action_57
action_56 (12) = happyGoto action_8
action_56 _ = happyFail

action_57 _ = happyReduce_32

action_58 _ = happyReduce_16

action_59 (28) = happyShift action_11
action_59 (12) = happyGoto action_91
action_59 _ = happyFail

action_60 (14) = happyShift action_9
action_60 (27) = happyShift action_10
action_60 (28) = happyShift action_11
action_60 (29) = happyShift action_12
action_60 (7) = happyGoto action_90
action_60 (8) = happyGoto action_5
action_60 (9) = happyGoto action_6
action_60 (10) = happyGoto action_7
action_60 (12) = happyGoto action_8
action_60 _ = happyFail

action_61 _ = happyReduce_22

action_62 (15) = happyShift action_89
action_62 _ = happyFail

action_63 _ = happyReduce_18

action_64 (15) = happyShift action_88
action_64 _ = happyFail

action_65 (27) = happyShift action_10
action_65 (28) = happyShift action_11
action_65 (29) = happyShift action_12
action_65 (9) = happyGoto action_6
action_65 (10) = happyGoto action_87
action_65 (12) = happyGoto action_8
action_65 _ = happyFail

action_66 _ = happyReduce_21

action_67 (15) = happyShift action_86
action_67 _ = happyFail

action_68 _ = happyReduce_24

action_69 _ = happyReduce_25

action_70 _ = happyReduce_14

action_71 _ = happyReduce_15

action_72 (15) = happyShift action_85
action_72 _ = happyFail

action_73 (15) = happyShift action_84
action_73 _ = happyFail

action_74 (15) = happyShift action_83
action_74 _ = happyFail

action_75 (15) = happyShift action_82
action_75 _ = happyFail

action_76 (15) = happyShift action_81
action_76 _ = happyFail

action_77 (15) = happyShift action_80
action_77 _ = happyFail

action_78 (14) = happyShift action_79
action_78 _ = happyFail

action_79 (13) = happyGoto action_96
action_79 _ = happyReduce_34

action_80 _ = happyReduce_13

action_81 _ = happyReduce_12

action_82 _ = happyReduce_11

action_83 _ = happyReduce_10

action_84 _ = happyReduce_9

action_85 _ = happyReduce_8

action_86 _ = happyReduce_23

action_87 (15) = happyShift action_95
action_87 _ = happyFail

action_88 _ = happyReduce_19

action_89 _ = happyReduce_17

action_90 (15) = happyShift action_94
action_90 _ = happyFail

action_91 (14) = happyShift action_93
action_91 (27) = happyShift action_10
action_91 (28) = happyShift action_11
action_91 (29) = happyShift action_12
action_91 (8) = happyGoto action_92
action_91 (9) = happyGoto action_6
action_91 (10) = happyGoto action_7
action_91 (12) = happyGoto action_8
action_91 _ = happyFail

action_92 (15) = happyShift action_99
action_92 _ = happyFail

action_93 (18) = happyShift action_16
action_93 (19) = happyShift action_17
action_93 (20) = happyShift action_18
action_93 (21) = happyShift action_19
action_93 (22) = happyShift action_20
action_93 (23) = happyShift action_21
action_93 (24) = happyShift action_22
action_93 (25) = happyShift action_23
action_93 (26) = happyShift action_24
action_93 (27) = happyShift action_10
action_93 (28) = happyShift action_11
action_93 (29) = happyShift action_12
action_93 (30) = happyShift action_25
action_93 (31) = happyShift action_26
action_93 (32) = happyShift action_27
action_93 (33) = happyShift action_28
action_93 (34) = happyShift action_29
action_93 (35) = happyShift action_30
action_93 (36) = happyShift action_31
action_93 (37) = happyShift action_32
action_93 (9) = happyGoto action_6
action_93 (10) = happyGoto action_13
action_93 (12) = happyGoto action_8
action_93 _ = happyFail

action_94 _ = happyReduce_6

action_95 _ = happyReduce_20

action_96 (15) = happyShift action_98
action_96 (28) = happyShift action_11
action_96 (12) = happyGoto action_97
action_96 _ = happyFail

action_97 _ = happyReduce_35

action_98 (14) = happyShift action_9
action_98 (27) = happyShift action_10
action_98 (28) = happyShift action_11
action_98 (29) = happyShift action_12
action_98 (7) = happyGoto action_101
action_98 (8) = happyGoto action_5
action_98 (9) = happyGoto action_6
action_98 (10) = happyGoto action_7
action_98 (12) = happyGoto action_8
action_98 _ = happyFail

action_99 (15) = happyShift action_100
action_99 _ = happyFail

action_100 (14) = happyShift action_9
action_100 (27) = happyShift action_10
action_100 (28) = happyShift action_11
action_100 (29) = happyShift action_12
action_100 (7) = happyGoto action_103
action_100 (8) = happyGoto action_5
action_100 (9) = happyGoto action_6
action_100 (10) = happyGoto action_7
action_100 (12) = happyGoto action_8
action_100 _ = happyFail

action_101 (15) = happyShift action_102
action_101 _ = happyFail

action_102 _ = happyReduce_4

action_103 (15) = happyShift action_104
action_103 _ = happyFail

action_104 _ = happyReduce_5

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (L3Program happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 : happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 7 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (L3Function happy_var_2 (reverse happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 10 7 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_6) `HappyStk`
	(HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (L3Let happy_var_5 happy_var_6 happy_var_9
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 6 7 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (L3If happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (L3Ed happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyReduce 5 8 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3Binop L3Add happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 5 8 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3Binop L3Sub happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 5 8 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3Binop L3Mult happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 5 8 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3Binop L3LessThan happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 5 8 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3Binop L3LessThanEqual happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 5 8 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3Binop L3Equal happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 4 8 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3Predicate L3IsA happy_var_3
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 4 8 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3Predicate L3IsNumber happy_var_3
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 4 8 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3Apply happy_var_2 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 5 8 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3NewArray happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 4 8 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3NewTuple (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 5 8 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3Aref happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 6 8 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3Aset happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 4 8 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3Alen happy_var_3
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 4 8 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3Print happy_var_3
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 5 8 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3MakeClosure happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 4 8 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3ClosureProc happy_var_3
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 4 8 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (L3ClosureVars happy_var_3
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_1  8 happyReduction_26
happyReduction_26 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (L3Dv happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  9 happyReduction_27
happyReduction_27 (HappyTerminal (TLab happy_var_1))
	 =  HappyAbsSyn9
		 (L3Label happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  10 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn10
		 (L3Vx happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  10 happyReduction_29
happyReduction_29 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 (L3Vlab happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  10 happyReduction_30
happyReduction_30 (HappyTerminal (TInt happy_var_1))
	 =  HappyAbsSyn10
		 (L3Vnum happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_0  11 happyReduction_31
happyReduction_31  =  HappyAbsSyn11
		 ([]
	)

happyReduce_32 = happySpecReduce_2  11 happyReduction_32
happyReduction_32 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_2 : happy_var_1
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  12 happyReduction_33
happyReduction_33 (HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn12
		 (L3X happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_0  13 happyReduction_34
happyReduction_34  =  HappyAbsSyn13
		 ([]
	)

happyReduce_35 = happySpecReduce_2  13 happyReduction_35
happyReduction_35 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_1
	)
happyReduction_35 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 38 38 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TOpen -> cont 14;
	TClose -> cont 15;
	TLet -> cont 16;
	TIf -> cont 17;
	TPrint -> cont 18;
	TNewArray -> cont 19;
	TNewTuple -> cont 20;
	TAref -> cont 21;
	TAset -> cont 22;
	TAlen -> cont 23;
	TMakeClosure -> cont 24;
	TClosureProc -> cont 25;
	TClosureVars -> cont 26;
	TInt happy_dollar_dollar -> cont 27;
	TVar happy_dollar_dollar -> cont 28;
	TLab happy_dollar_dollar -> cont 29;
	TIsA -> cont 30;
	TIsNumber -> cont 31;
	TAdd -> cont 32;
	TSub -> cont 33;
	TMult -> cont 34;
	TLT -> cont 35;
	TLTE -> cont 36;
	TEq -> cont 37;
	_ -> happyError' (tk:tks)
	}

happyError_ 38 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => E a -> (a -> E b) -> E b
happyThen = (thenE)
happyReturn :: () => a -> E a
happyReturn = (returnE)
happyThen1 m k tks = (thenE) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> E a
happyReturn1 = \a tks -> (returnE) a
happyError' :: () => [(Token)] -> E a
happyError' = parseError

calc tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data E a = Ok a
         | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = case m of 
    Ok a -> k a
    Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = case m of
    Ok a -> Ok a
    Failed e -> k e


parseError :: [Token] -> E a
parseError tokens = failE $ "Parse error. Tokens: " ++ show tokens


dropEnd :: [Token] -> [Token]
dropEnd [] = []
dropEnd (t:ts) = case t of
    TOpen -> t : matchClose 1 ts
    _ -> t : dropEnd ts

matchClose :: Int -> [Token] -> [Token]
matchClose _ [] = []
matchClose 0 ts = []
matchClose i (t:ts) = case t of
    TOpen -> t : matchClose (i + 1) ts
    TClose -> t : matchClose (i - 1) ts
    _ -> t : matchClose i ts

readProg = calc . dropEnd . lexer
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

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







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
