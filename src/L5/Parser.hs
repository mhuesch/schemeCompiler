{-# OPTIONS_GHC -w #-}
module L5.Parser where


import Control.Monad.Error

import L5.Token
import L5.Grammar

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11
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

action_0 (12) = happyShift action_7
action_0 (19) = happyShift action_8
action_0 (20) = happyShift action_9
action_0 (22) = happyShift action_10
action_0 (23) = happyShift action_11
action_0 (24) = happyShift action_12
action_0 (25) = happyShift action_13
action_0 (26) = happyShift action_14
action_0 (27) = happyShift action_15
action_0 (28) = happyShift action_16
action_0 (29) = happyShift action_17
action_0 (30) = happyShift action_18
action_0 (31) = happyShift action_19
action_0 (32) = happyShift action_20
action_0 (33) = happyShift action_21
action_0 (34) = happyShift action_22
action_0 (4) = happyGoto action_23
action_0 (5) = happyGoto action_2
action_0 (7) = happyGoto action_3
action_0 (8) = happyGoto action_4
action_0 (9) = happyGoto action_5
action_0 (10) = happyGoto action_6
action_0 _ = happyFail

action_1 (12) = happyShift action_7
action_1 (19) = happyShift action_8
action_1 (20) = happyShift action_9
action_1 (22) = happyShift action_10
action_1 (23) = happyShift action_11
action_1 (24) = happyShift action_12
action_1 (25) = happyShift action_13
action_1 (26) = happyShift action_14
action_1 (27) = happyShift action_15
action_1 (28) = happyShift action_16
action_1 (29) = happyShift action_17
action_1 (30) = happyShift action_18
action_1 (31) = happyShift action_19
action_1 (32) = happyShift action_20
action_1 (33) = happyShift action_21
action_1 (34) = happyShift action_22
action_1 (5) = happyGoto action_2
action_1 (7) = happyGoto action_3
action_1 (8) = happyGoto action_4
action_1 (9) = happyGoto action_5
action_1 (10) = happyGoto action_6
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 _ = happyReduce_10

action_4 _ = happyReduce_14

action_5 _ = happyReduce_15

action_6 _ = happyReduce_3

action_7 (12) = happyShift action_7
action_7 (14) = happyShift action_25
action_7 (15) = happyShift action_26
action_7 (16) = happyShift action_27
action_7 (17) = happyShift action_28
action_7 (18) = happyShift action_29
action_7 (19) = happyShift action_8
action_7 (20) = happyShift action_9
action_7 (21) = happyShift action_30
action_7 (22) = happyShift action_10
action_7 (23) = happyShift action_11
action_7 (24) = happyShift action_12
action_7 (25) = happyShift action_13
action_7 (26) = happyShift action_14
action_7 (27) = happyShift action_15
action_7 (28) = happyShift action_16
action_7 (29) = happyShift action_17
action_7 (30) = happyShift action_18
action_7 (31) = happyShift action_19
action_7 (32) = happyShift action_20
action_7 (33) = happyShift action_21
action_7 (34) = happyShift action_22
action_7 (5) = happyGoto action_24
action_7 (7) = happyGoto action_3
action_7 (8) = happyGoto action_4
action_7 (9) = happyGoto action_5
action_7 (10) = happyGoto action_6
action_7 _ = happyFail

action_8 _ = happyReduce_16

action_9 _ = happyReduce_17

action_10 _ = happyReduce_18

action_11 _ = happyReduce_19

action_12 _ = happyReduce_20

action_13 _ = happyReduce_11

action_14 _ = happyReduce_29

action_15 _ = happyReduce_28

action_16 _ = happyReduce_27

action_17 _ = happyReduce_21

action_18 _ = happyReduce_22

action_19 _ = happyReduce_23

action_20 _ = happyReduce_24

action_21 _ = happyReduce_25

action_22 _ = happyReduce_26

action_23 (35) = happyAccept
action_23 _ = happyFail

action_24 (6) = happyGoto action_37
action_24 _ = happyReduce_12

action_25 (12) = happyShift action_36
action_25 _ = happyFail

action_26 (12) = happyShift action_35
action_26 _ = happyFail

action_27 (12) = happyShift action_34
action_27 _ = happyFail

action_28 (12) = happyShift action_7
action_28 (19) = happyShift action_8
action_28 (20) = happyShift action_9
action_28 (22) = happyShift action_10
action_28 (23) = happyShift action_11
action_28 (24) = happyShift action_12
action_28 (25) = happyShift action_13
action_28 (26) = happyShift action_14
action_28 (27) = happyShift action_15
action_28 (28) = happyShift action_16
action_28 (29) = happyShift action_17
action_28 (30) = happyShift action_18
action_28 (31) = happyShift action_19
action_28 (32) = happyShift action_20
action_28 (33) = happyShift action_21
action_28 (34) = happyShift action_22
action_28 (5) = happyGoto action_33
action_28 (7) = happyGoto action_3
action_28 (8) = happyGoto action_4
action_28 (9) = happyGoto action_5
action_28 (10) = happyGoto action_6
action_28 _ = happyFail

action_29 (12) = happyShift action_7
action_29 (19) = happyShift action_8
action_29 (20) = happyShift action_9
action_29 (22) = happyShift action_10
action_29 (23) = happyShift action_11
action_29 (24) = happyShift action_12
action_29 (25) = happyShift action_13
action_29 (26) = happyShift action_14
action_29 (27) = happyShift action_15
action_29 (28) = happyShift action_16
action_29 (29) = happyShift action_17
action_29 (30) = happyShift action_18
action_29 (31) = happyShift action_19
action_29 (32) = happyShift action_20
action_29 (33) = happyShift action_21
action_29 (34) = happyShift action_22
action_29 (5) = happyGoto action_32
action_29 (7) = happyGoto action_3
action_29 (8) = happyGoto action_4
action_29 (9) = happyGoto action_5
action_29 (10) = happyGoto action_6
action_29 _ = happyFail

action_30 (6) = happyGoto action_31
action_30 _ = happyReduce_12

action_31 (12) = happyShift action_7
action_31 (13) = happyShift action_45
action_31 (19) = happyShift action_8
action_31 (20) = happyShift action_9
action_31 (22) = happyShift action_10
action_31 (23) = happyShift action_11
action_31 (24) = happyShift action_12
action_31 (25) = happyShift action_13
action_31 (26) = happyShift action_14
action_31 (27) = happyShift action_15
action_31 (28) = happyShift action_16
action_31 (29) = happyShift action_17
action_31 (30) = happyShift action_18
action_31 (31) = happyShift action_19
action_31 (32) = happyShift action_20
action_31 (33) = happyShift action_21
action_31 (34) = happyShift action_22
action_31 (5) = happyGoto action_38
action_31 (7) = happyGoto action_3
action_31 (8) = happyGoto action_4
action_31 (9) = happyGoto action_5
action_31 (10) = happyGoto action_6
action_31 _ = happyFail

action_32 (12) = happyShift action_7
action_32 (19) = happyShift action_8
action_32 (20) = happyShift action_9
action_32 (22) = happyShift action_10
action_32 (23) = happyShift action_11
action_32 (24) = happyShift action_12
action_32 (25) = happyShift action_13
action_32 (26) = happyShift action_14
action_32 (27) = happyShift action_15
action_32 (28) = happyShift action_16
action_32 (29) = happyShift action_17
action_32 (30) = happyShift action_18
action_32 (31) = happyShift action_19
action_32 (32) = happyShift action_20
action_32 (33) = happyShift action_21
action_32 (34) = happyShift action_22
action_32 (5) = happyGoto action_44
action_32 (7) = happyGoto action_3
action_32 (8) = happyGoto action_4
action_32 (9) = happyGoto action_5
action_32 (10) = happyGoto action_6
action_32 _ = happyFail

action_33 (12) = happyShift action_7
action_33 (19) = happyShift action_8
action_33 (20) = happyShift action_9
action_33 (22) = happyShift action_10
action_33 (23) = happyShift action_11
action_33 (24) = happyShift action_12
action_33 (25) = happyShift action_13
action_33 (26) = happyShift action_14
action_33 (27) = happyShift action_15
action_33 (28) = happyShift action_16
action_33 (29) = happyShift action_17
action_33 (30) = happyShift action_18
action_33 (31) = happyShift action_19
action_33 (32) = happyShift action_20
action_33 (33) = happyShift action_21
action_33 (34) = happyShift action_22
action_33 (5) = happyGoto action_43
action_33 (7) = happyGoto action_3
action_33 (8) = happyGoto action_4
action_33 (9) = happyGoto action_5
action_33 (10) = happyGoto action_6
action_33 _ = happyFail

action_34 (12) = happyShift action_42
action_34 _ = happyFail

action_35 (12) = happyShift action_41
action_35 _ = happyFail

action_36 (11) = happyGoto action_40
action_36 _ = happyReduce_30

action_37 (12) = happyShift action_7
action_37 (13) = happyShift action_39
action_37 (19) = happyShift action_8
action_37 (20) = happyShift action_9
action_37 (22) = happyShift action_10
action_37 (23) = happyShift action_11
action_37 (24) = happyShift action_12
action_37 (25) = happyShift action_13
action_37 (26) = happyShift action_14
action_37 (27) = happyShift action_15
action_37 (28) = happyShift action_16
action_37 (29) = happyShift action_17
action_37 (30) = happyShift action_18
action_37 (31) = happyShift action_19
action_37 (32) = happyShift action_20
action_37 (33) = happyShift action_21
action_37 (34) = happyShift action_22
action_37 (5) = happyGoto action_38
action_37 (7) = happyGoto action_3
action_37 (8) = happyGoto action_4
action_37 (9) = happyGoto action_5
action_37 (10) = happyGoto action_6
action_37 _ = happyFail

action_38 _ = happyReduce_13

action_39 _ = happyReduce_9

action_40 (13) = happyShift action_51
action_40 (26) = happyShift action_14
action_40 (10) = happyGoto action_50
action_40 _ = happyFail

action_41 (26) = happyShift action_14
action_41 (10) = happyGoto action_49
action_41 _ = happyFail

action_42 (26) = happyShift action_14
action_42 (10) = happyGoto action_48
action_42 _ = happyFail

action_43 (12) = happyShift action_7
action_43 (19) = happyShift action_8
action_43 (20) = happyShift action_9
action_43 (22) = happyShift action_10
action_43 (23) = happyShift action_11
action_43 (24) = happyShift action_12
action_43 (25) = happyShift action_13
action_43 (26) = happyShift action_14
action_43 (27) = happyShift action_15
action_43 (28) = happyShift action_16
action_43 (29) = happyShift action_17
action_43 (30) = happyShift action_18
action_43 (31) = happyShift action_19
action_43 (32) = happyShift action_20
action_43 (33) = happyShift action_21
action_43 (34) = happyShift action_22
action_43 (5) = happyGoto action_47
action_43 (7) = happyGoto action_3
action_43 (8) = happyGoto action_4
action_43 (9) = happyGoto action_5
action_43 (10) = happyGoto action_6
action_43 _ = happyFail

action_44 (13) = happyShift action_46
action_44 _ = happyFail

action_45 _ = happyReduce_7

action_46 _ = happyReduce_8

action_47 (13) = happyShift action_55
action_47 _ = happyFail

action_48 (12) = happyShift action_7
action_48 (19) = happyShift action_8
action_48 (20) = happyShift action_9
action_48 (22) = happyShift action_10
action_48 (23) = happyShift action_11
action_48 (24) = happyShift action_12
action_48 (25) = happyShift action_13
action_48 (26) = happyShift action_14
action_48 (27) = happyShift action_15
action_48 (28) = happyShift action_16
action_48 (29) = happyShift action_17
action_48 (30) = happyShift action_18
action_48 (31) = happyShift action_19
action_48 (32) = happyShift action_20
action_48 (33) = happyShift action_21
action_48 (34) = happyShift action_22
action_48 (5) = happyGoto action_54
action_48 (7) = happyGoto action_3
action_48 (8) = happyGoto action_4
action_48 (9) = happyGoto action_5
action_48 (10) = happyGoto action_6
action_48 _ = happyFail

action_49 (12) = happyShift action_7
action_49 (19) = happyShift action_8
action_49 (20) = happyShift action_9
action_49 (22) = happyShift action_10
action_49 (23) = happyShift action_11
action_49 (24) = happyShift action_12
action_49 (25) = happyShift action_13
action_49 (26) = happyShift action_14
action_49 (27) = happyShift action_15
action_49 (28) = happyShift action_16
action_49 (29) = happyShift action_17
action_49 (30) = happyShift action_18
action_49 (31) = happyShift action_19
action_49 (32) = happyShift action_20
action_49 (33) = happyShift action_21
action_49 (34) = happyShift action_22
action_49 (5) = happyGoto action_53
action_49 (7) = happyGoto action_3
action_49 (8) = happyGoto action_4
action_49 (9) = happyGoto action_5
action_49 (10) = happyGoto action_6
action_49 _ = happyFail

action_50 _ = happyReduce_31

action_51 (12) = happyShift action_7
action_51 (19) = happyShift action_8
action_51 (20) = happyShift action_9
action_51 (22) = happyShift action_10
action_51 (23) = happyShift action_11
action_51 (24) = happyShift action_12
action_51 (25) = happyShift action_13
action_51 (26) = happyShift action_14
action_51 (27) = happyShift action_15
action_51 (28) = happyShift action_16
action_51 (29) = happyShift action_17
action_51 (30) = happyShift action_18
action_51 (31) = happyShift action_19
action_51 (32) = happyShift action_20
action_51 (33) = happyShift action_21
action_51 (34) = happyShift action_22
action_51 (5) = happyGoto action_52
action_51 (7) = happyGoto action_3
action_51 (8) = happyGoto action_4
action_51 (9) = happyGoto action_5
action_51 (10) = happyGoto action_6
action_51 _ = happyFail

action_52 (13) = happyShift action_58
action_52 _ = happyFail

action_53 (13) = happyShift action_57
action_53 _ = happyFail

action_54 (13) = happyShift action_56
action_54 _ = happyFail

action_55 _ = happyReduce_6

action_56 (13) = happyShift action_60
action_56 _ = happyFail

action_57 (13) = happyShift action_59
action_57 _ = happyFail

action_58 _ = happyReduce_2

action_59 (12) = happyShift action_7
action_59 (19) = happyShift action_8
action_59 (20) = happyShift action_9
action_59 (22) = happyShift action_10
action_59 (23) = happyShift action_11
action_59 (24) = happyShift action_12
action_59 (25) = happyShift action_13
action_59 (26) = happyShift action_14
action_59 (27) = happyShift action_15
action_59 (28) = happyShift action_16
action_59 (29) = happyShift action_17
action_59 (30) = happyShift action_18
action_59 (31) = happyShift action_19
action_59 (32) = happyShift action_20
action_59 (33) = happyShift action_21
action_59 (34) = happyShift action_22
action_59 (5) = happyGoto action_62
action_59 (7) = happyGoto action_3
action_59 (8) = happyGoto action_4
action_59 (9) = happyGoto action_5
action_59 (10) = happyGoto action_6
action_59 _ = happyFail

action_60 (12) = happyShift action_7
action_60 (19) = happyShift action_8
action_60 (20) = happyShift action_9
action_60 (22) = happyShift action_10
action_60 (23) = happyShift action_11
action_60 (24) = happyShift action_12
action_60 (25) = happyShift action_13
action_60 (26) = happyShift action_14
action_60 (27) = happyShift action_15
action_60 (28) = happyShift action_16
action_60 (29) = happyShift action_17
action_60 (30) = happyShift action_18
action_60 (31) = happyShift action_19
action_60 (32) = happyShift action_20
action_60 (33) = happyShift action_21
action_60 (34) = happyShift action_22
action_60 (5) = happyGoto action_61
action_60 (7) = happyGoto action_3
action_60 (8) = happyGoto action_4
action_60 (9) = happyGoto action_5
action_60 (10) = happyGoto action_6
action_60 _ = happyFail

action_61 (13) = happyShift action_64
action_61 _ = happyFail

action_62 (13) = happyShift action_63
action_62 _ = happyFail

action_63 _ = happyReduce_4

action_64 _ = happyReduce_5

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (L5Program happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happyReduce 7 5 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (L5Lambda (reverse happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn5
		 (L5Ex happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happyReduce 10 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (L5Let happy_var_5 happy_var_6 happy_var_9
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 10 5 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (L5LetRec happy_var_5 happy_var_6 happy_var_9
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 6 5 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (L5If happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 4 5 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (L5NewTuple (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 5 5 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (L5Begin happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 4 5 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (L5Apply happy_var_2 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (L5Primitive happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  5 happyReduction_11
happyReduction_11 (HappyTerminal (TInt happy_var_1))
	 =  HappyAbsSyn5
		 (L5Enum happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  6 happyReduction_12
happyReduction_12  =  HappyAbsSyn6
		 ([]
	)

happyReduce_13 = happySpecReduce_2  6 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_2 : happy_var_1
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  7 happyReduction_14
happyReduction_14 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (L5pb happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  7 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (L5pp happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  7 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn7
		 (L5Print
	)

happyReduce_17 = happySpecReduce_1  7 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn7
		 (L5NewArray
	)

happyReduce_18 = happySpecReduce_1  7 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn7
		 (L5Aref
	)

happyReduce_19 = happySpecReduce_1  7 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn7
		 (L5Aset
	)

happyReduce_20 = happySpecReduce_1  7 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn7
		 (L5Alen
	)

happyReduce_21 = happySpecReduce_1  8 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn8
		 (L5Add
	)

happyReduce_22 = happySpecReduce_1  8 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn8
		 (L5Sub
	)

happyReduce_23 = happySpecReduce_1  8 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn8
		 (L5Mult
	)

happyReduce_24 = happySpecReduce_1  8 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn8
		 (L5LT
	)

happyReduce_25 = happySpecReduce_1  8 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn8
		 (L5LTE
	)

happyReduce_26 = happySpecReduce_1  8 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn8
		 (L5Eq
	)

happyReduce_27 = happySpecReduce_1  9 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn9
		 (L5IsNumber
	)

happyReduce_28 = happySpecReduce_1  9 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn9
		 (L5IsA
	)

happyReduce_29 = happySpecReduce_1  10 happyReduction_29
happyReduction_29 (HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn10
		 (L5X $ '_':happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_0  11 happyReduction_30
happyReduction_30  =  HappyAbsSyn11
		 ([]
	)

happyReduce_31 = happySpecReduce_2  11 happyReduction_31
happyReduction_31 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_2 : happy_var_1
	)
happyReduction_31 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 35 35 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TOpen -> cont 12;
	TClose -> cont 13;
	TLambda -> cont 14;
	TLet -> cont 15;
	TLetRec -> cont 16;
	TIf -> cont 17;
	TBegin -> cont 18;
	TPrint -> cont 19;
	TNewArray -> cont 20;
	TNewTuple -> cont 21;
	TAref -> cont 22;
	TAset -> cont 23;
	TAlen -> cont 24;
	TInt happy_dollar_dollar -> cont 25;
	TVar happy_dollar_dollar -> cont 26;
	TIsA -> cont 27;
	TIsNumber -> cont 28;
	TAdd -> cont 29;
	TSub -> cont 30;
	TMult -> cont 31;
	TLT -> cont 32;
	TLTE -> cont 33;
	TEq -> cont 34;
	_ -> happyError' (tk:tks)
	}

happyError_ 35 tk tks = happyError' tks
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
