{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.API.Parse
    ( parseAPI
    ) where

import           Data.API.Types
import           Data.API.Scan
import           Data.Char
import           Data.String
import qualified Data.CaseInsensitive       as CI
import           Text.Printf
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.18.9

data HappyAbsSyn 
	= HappyTerminal (PToken)
	| HappyErrorToken Int
	| HappyAbsSyn5 (API)
	| HappyAbsSyn6 ([Thing])
	| HappyAbsSyn7 (Thing)
	| HappyAbsSyn8 (APINode)
	| HappyAbsSyn9 (Spec)
	| HappyAbsSyn10 (Conversion)
	| HappyAbsSyn11 (Vrn)
	| HappyAbsSyn12 (MDComment)
	| HappyAbsSyn13 ([MDComment])
	| HappyAbsSyn14 (Prefix)
	| HappyAbsSyn15 (SpecRecord)
	| HappyAbsSyn16 (SpecUnion)
	| HappyAbsSyn17 ([(FieldName,(APIType,MDComment))])
	| HappyAbsSyn19 (SpecEnum)
	| HappyAbsSyn20 ([FieldName])
	| HappyAbsSyn21 (APIType)
	| HappyAbsSyn22 (BasicType)
	| HappyAbsSyn23 (FieldName)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Happy_GHC_Exts.Int# 
	-> (PToken)
	-> HappyState (PToken) (HappyStk HappyAbsSyn -> [(PToken)] -> m HappyAbsSyn)
	-> [HappyState (PToken) (HappyStk HappyAbsSyn -> [(PToken)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(PToken)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69 :: () => Happy_GHC_Exts.Int# -> ({-HappyReduction (HappyIdentity) = -}
	   Happy_GHC_Exts.Int# 
	-> (PToken)
	-> HappyState (PToken) (HappyStk HappyAbsSyn -> [(PToken)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (PToken) (HappyStk HappyAbsSyn -> [(PToken)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(PToken)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Happy_GHC_Exts.Int# 
	-> (PToken)
	-> HappyState (PToken) (HappyStk HappyAbsSyn -> [(PToken)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (PToken) (HappyStk HappyAbsSyn -> [(PToken)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(PToken)] -> (HappyIdentity) HappyAbsSyn)

action_0 (5#) = happyGoto action_6
action_0 (6#) = happyGoto action_3
action_0 x = happyTcHack x happyReduce_4

action_1 (25#) = happyShift action_5
action_1 (18#) = happyGoto action_4
action_1 x = happyTcHack x happyFail

action_2 (6#) = happyGoto action_3
action_2 x = happyTcHack x happyFail

action_3 (24#) = happyShift action_10
action_3 x = happyTcHack x happyReduce_2

action_4 (25#) = happyShift action_9
action_4 (45#) = happyAccept
action_4 x = happyTcHack x happyFail

action_5 (43#) = happyShift action_8
action_5 (23#) = happyGoto action_7
action_5 x = happyTcHack x happyFail

action_6 (45#) = happyAccept
action_6 x = happyTcHack x happyFail

action_7 (29#) = happyShift action_18
action_7 x = happyTcHack x happyFail

action_8 x = happyTcHack x happyReduce_37

action_9 (43#) = happyShift action_8
action_9 (23#) = happyGoto action_17
action_9 x = happyTcHack x happyFail

action_10 (43#) = happyShift action_16
action_10 (7#) = happyGoto action_11
action_10 (8#) = happyGoto action_12
action_10 (12#) = happyGoto action_13
action_10 (13#) = happyGoto action_14
action_10 (14#) = happyGoto action_15
action_10 x = happyTcHack x happyReduce_18

action_11 x = happyTcHack x happyReduce_3

action_12 x = happyTcHack x happyReduce_5

action_13 x = happyTcHack x happyReduce_6

action_14 (41#) = happyShift action_30
action_14 x = happyTcHack x happyReduce_16

action_15 (28#) = happyShift action_29
action_15 x = happyTcHack x happyFail

action_16 x = happyTcHack x happyReduce_19

action_17 (29#) = happyShift action_28
action_17 x = happyTcHack x happyFail

action_18 (26#) = happyShift action_21
action_18 (31#) = happyShift action_22
action_18 (35#) = happyShift action_23
action_18 (36#) = happyShift action_24
action_18 (37#) = happyShift action_25
action_18 (38#) = happyShift action_26
action_18 (42#) = happyShift action_27
action_18 (21#) = happyGoto action_19
action_18 (22#) = happyGoto action_20
action_18 x = happyTcHack x happyFail

action_19 (12#) = happyGoto action_35
action_19 (13#) = happyGoto action_14
action_19 x = happyTcHack x happyReduce_18

action_20 x = happyTcHack x happyReduce_32

action_21 (26#) = happyShift action_21
action_21 (31#) = happyShift action_22
action_21 (35#) = happyShift action_23
action_21 (36#) = happyShift action_24
action_21 (37#) = happyShift action_25
action_21 (38#) = happyShift action_26
action_21 (42#) = happyShift action_27
action_21 (21#) = happyGoto action_34
action_21 (22#) = happyGoto action_20
action_21 x = happyTcHack x happyFail

action_22 (26#) = happyShift action_21
action_22 (31#) = happyShift action_22
action_22 (35#) = happyShift action_23
action_22 (36#) = happyShift action_24
action_22 (37#) = happyShift action_25
action_22 (38#) = happyShift action_26
action_22 (42#) = happyShift action_27
action_22 (21#) = happyGoto action_33
action_22 (22#) = happyGoto action_20
action_22 x = happyTcHack x happyFail

action_23 x = happyTcHack x happyReduce_36

action_24 x = happyTcHack x happyReduce_35

action_25 x = happyTcHack x happyReduce_33

action_26 x = happyTcHack x happyReduce_34

action_27 x = happyTcHack x happyReduce_31

action_28 (26#) = happyShift action_21
action_28 (31#) = happyShift action_22
action_28 (35#) = happyShift action_23
action_28 (36#) = happyShift action_24
action_28 (37#) = happyShift action_25
action_28 (38#) = happyShift action_26
action_28 (42#) = happyShift action_27
action_28 (21#) = happyGoto action_32
action_28 (22#) = happyGoto action_20
action_28 x = happyTcHack x happyFail

action_29 (42#) = happyShift action_31
action_29 x = happyTcHack x happyFail

action_30 x = happyTcHack x happyReduce_17

action_31 (12#) = happyGoto action_38
action_31 (13#) = happyGoto action_14
action_31 x = happyTcHack x happyReduce_18

action_32 (12#) = happyGoto action_37
action_32 (13#) = happyGoto action_14
action_32 x = happyTcHack x happyReduce_18

action_33 x = happyTcHack x happyReduce_29

action_34 (27#) = happyShift action_36
action_34 x = happyTcHack x happyFail

action_35 x = happyTcHack x happyReduce_25

action_36 x = happyTcHack x happyReduce_30

action_37 x = happyTcHack x happyReduce_24

action_38 (30#) = happyShift action_39
action_38 x = happyTcHack x happyFail

action_39 (26#) = happyShift action_21
action_39 (31#) = happyShift action_22
action_39 (35#) = happyShift action_23
action_39 (36#) = happyShift action_24
action_39 (37#) = happyShift action_25
action_39 (38#) = happyShift action_26
action_39 (39#) = happyShift action_47
action_39 (40#) = happyShift action_48
action_39 (42#) = happyShift action_27
action_39 (43#) = happyShift action_8
action_39 (9#) = happyGoto action_40
action_39 (15#) = happyGoto action_41
action_39 (16#) = happyGoto action_42
action_39 (19#) = happyGoto action_43
action_39 (20#) = happyGoto action_44
action_39 (21#) = happyGoto action_45
action_39 (22#) = happyGoto action_20
action_39 (23#) = happyGoto action_46
action_39 x = happyTcHack x happyFail

action_40 (34#) = happyShift action_54
action_40 (10#) = happyGoto action_53
action_40 x = happyTcHack x happyReduce_13

action_41 x = happyTcHack x happyReduce_8

action_42 x = happyTcHack x happyReduce_9

action_43 x = happyTcHack x happyReduce_10

action_44 (25#) = happyShift action_52
action_44 x = happyTcHack x happyReduce_26

action_45 x = happyTcHack x happyReduce_11

action_46 x = happyTcHack x happyReduce_28

action_47 (43#) = happyShift action_8
action_47 (17#) = happyGoto action_50
action_47 (23#) = happyGoto action_51
action_47 x = happyTcHack x happyFail

action_48 (25#) = happyShift action_5
action_48 (18#) = happyGoto action_49
action_48 x = happyTcHack x happyFail

action_49 (25#) = happyShift action_9
action_49 x = happyTcHack x happyReduce_21

action_50 (43#) = happyShift action_8
action_50 (23#) = happyGoto action_60
action_50 x = happyTcHack x happyReduce_20

action_51 (29#) = happyShift action_59
action_51 x = happyTcHack x happyFail

action_52 (43#) = happyShift action_8
action_52 (23#) = happyGoto action_58
action_52 x = happyTcHack x happyFail

action_53 (33#) = happyShift action_57
action_53 (11#) = happyGoto action_56
action_53 x = happyTcHack x happyReduce_15

action_54 (43#) = happyShift action_8
action_54 (23#) = happyGoto action_55
action_54 x = happyTcHack x happyFail

action_55 (32#) = happyShift action_65
action_55 x = happyTcHack x happyFail

action_56 (12#) = happyGoto action_64
action_56 (13#) = happyGoto action_14
action_56 x = happyTcHack x happyReduce_18

action_57 (44#) = happyShift action_63
action_57 x = happyTcHack x happyFail

action_58 x = happyTcHack x happyReduce_27

action_59 (26#) = happyShift action_21
action_59 (31#) = happyShift action_22
action_59 (35#) = happyShift action_23
action_59 (36#) = happyShift action_24
action_59 (37#) = happyShift action_25
action_59 (38#) = happyShift action_26
action_59 (42#) = happyShift action_27
action_59 (21#) = happyGoto action_62
action_59 (22#) = happyGoto action_20
action_59 x = happyTcHack x happyFail

action_60 (29#) = happyShift action_61
action_60 x = happyTcHack x happyFail

action_61 (26#) = happyShift action_21
action_61 (31#) = happyShift action_22
action_61 (35#) = happyShift action_23
action_61 (36#) = happyShift action_24
action_61 (37#) = happyShift action_25
action_61 (38#) = happyShift action_26
action_61 (42#) = happyShift action_27
action_61 (21#) = happyGoto action_68
action_61 (22#) = happyGoto action_20
action_61 x = happyTcHack x happyFail

action_62 (12#) = happyGoto action_67
action_62 (13#) = happyGoto action_14
action_62 x = happyTcHack x happyReduce_18

action_63 x = happyTcHack x happyReduce_14

action_64 x = happyTcHack x happyReduce_7

action_65 (43#) = happyShift action_8
action_65 (23#) = happyGoto action_66
action_65 x = happyTcHack x happyFail

action_66 x = happyTcHack x happyReduce_12

action_67 x = happyTcHack x happyReduce_23

action_68 (12#) = happyGoto action_69
action_68 (13#) = happyGoto action_14
action_68 x = happyTcHack x happyReduce_18

action_69 x = happyTcHack x happyReduce_22

happyReduce_2 = happySpecReduce_1  5# happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (reverse happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  6# happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_3 : happy_var_1
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  6# happyReduction_4
happyReduction_4  =  HappyAbsSyn6
		 ([]
	)

happyReduce_5 = happySpecReduce_1  7# happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (ThNode    happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7# happyReduction_6
happyReduction_6 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn7
		 (ThComment happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happyReduce 9# 8# happyReduction_7
happyReduction_7 ((HappyAbsSyn12  happy_var_9) `HappyStk`
	(HappyAbsSyn11  happy_var_8) `HappyStk`
	(HappyAbsSyn10  happy_var_7) `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	(HappyTerminal ((,) _ (TypeIden happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (APINode (TypeName happy_var_3) happy_var_4 happy_var_1 happy_var_6 happy_var_7 happy_var_8 happy_var_9
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  9# happyReduction_8
happyReduction_8 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn9
		 (SpRecord  $ happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  9# happyReduction_9
happyReduction_9 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn9
		 (SpUnion   $ happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  9# happyReduction_10
happyReduction_10 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn9
		 (SpEnum    $ happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  9# happyReduction_11
happyReduction_11 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn9
		 (sp_type   $ happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happyReduce 4# 10# happyReduction_12
happyReduction_12 ((HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Just (happy_var_2,happy_var_4)
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_0  10# happyReduction_13
happyReduction_13  =  HappyAbsSyn10
		 (Nothing
	)

happyReduce_14 = happySpecReduce_2  11# happyReduction_14
happyReduction_14 (HappyTerminal ((,) _ (Intg     happy_var_2)))
	_
	 =  HappyAbsSyn11
		 (Vrn happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  11# happyReduction_15
happyReduction_15  =  HappyAbsSyn11
		 (0
	)

happyReduce_16 = happySpecReduce_1  12# happyReduction_16
happyReduction_16 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (unlines $ reverse happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  13# happyReduction_17
happyReduction_17 (HappyTerminal ((,) _ (Comment  happy_var_2)))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_1
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  13# happyReduction_18
happyReduction_18  =  HappyAbsSyn13
		 ([]
	)

happyReduce_19 = happySpecReduce_1  14# happyReduction_19
happyReduction_19 (HappyTerminal ((,) _ (VarIden  happy_var_1)))
	 =  HappyAbsSyn14
		 (CI.mk happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  15# happyReduction_20
happyReduction_20 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (SpecRecord $ reverse happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  16# happyReduction_21
happyReduction_21 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (SpecUnion  $ reverse happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 5# 17# happyReduction_22
happyReduction_22 ((HappyAbsSyn12  happy_var_5) `HappyStk`
	(HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((happy_var_2,(happy_var_4,happy_var_5)) : happy_var_1
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 4# 17# happyReduction_23
happyReduction_23 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ([(happy_var_1,(happy_var_3,happy_var_4))]
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 6# 18# happyReduction_24
happyReduction_24 ((HappyAbsSyn12  happy_var_6) `HappyStk`
	(HappyAbsSyn21  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((happy_var_3,(happy_var_5,happy_var_6)) : happy_var_1
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 5# 18# happyReduction_25
happyReduction_25 ((HappyAbsSyn12  happy_var_5) `HappyStk`
	(HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ([(happy_var_2,(happy_var_4,happy_var_5))]
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_1  19# happyReduction_26
happyReduction_26 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (SpecEnum $ reverse happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  20# happyReduction_27
happyReduction_27 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_3 : happy_var_1
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  20# happyReduction_28
happyReduction_28 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn20
		 ([happy_var_1]
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  21# happyReduction_29
happyReduction_29 (HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (TyMaybe            happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  21# happyReduction_30
happyReduction_30 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (TyList             happy_var_2
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  21# happyReduction_31
happyReduction_31 (HappyTerminal ((,) _ (TypeIden happy_var_1)))
	 =  HappyAbsSyn21
		 (TyName  $ TypeName happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  21# happyReduction_32
happyReduction_32 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (TyBasic            happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  22# happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn22
		 (BTstring
	)

happyReduce_34 = happySpecReduce_1  22# happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn22
		 (BTbinary
	)

happyReduce_35 = happySpecReduce_1  22# happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn22
		 (BTbool
	)

happyReduce_36 = happySpecReduce_1  22# happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn22
		 (BTint
	)

happyReduce_37 = happySpecReduce_1  23# happyReduction_37
happyReduction_37 (HappyTerminal ((,) _ (VarIden  happy_var_1)))
	 =  HappyAbsSyn23
		 (FieldName happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 45# 45# notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	(,) _ Semi -> cont 24#;
	(,) _ Bar -> cont 25#;
	(,) _ Bra -> cont 26#;
	(,) _ Ket -> cont 27#;
	(,) _ ColCol -> cont 28#;
	(,) _ Colon -> cont 29#;
	(,) _ Equals -> cont 30#;
	(,) _ Query -> cont 31#;
	(,) _ Comma -> cont 32#;
	(,) _ Version -> cont 33#;
	(,) _ With -> cont 34#;
	(,) _ Integer -> cont 35#;
	(,) _ Boolean -> cont 36#;
	(,) _ String -> cont 37#;
	(,) _ Binary -> cont 38#;
	(,) _ Record -> cont 39#;
	(,) _ Union -> cont 40#;
	(,) _ (Comment  happy_dollar_dollar) -> cont 41#;
	(,) _ (TypeIden happy_dollar_dollar) -> cont 42#;
	(,) _ (VarIden  happy_dollar_dollar) -> cont 43#;
	(,) _ (Intg     happy_dollar_dollar) -> cont 44#;
	_ -> happyError' (tk:tks)
	}

happyError_ 45# tk tks = happyError' tks
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
happyError' :: () => [(PToken)] -> HappyIdentity a
happyError' = HappyIdentity . happyError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

parse_elt tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [PToken] -> a
happyError tks = error $ printf "Syntax error at %s: %s\n" loc $ show (take 5 tks)
  where
    loc = case tks of
            []                    -> "<EOF>"
            (AlexPn ad ln cn,_):_ -> printf "line %d, column %d (@%d)" ln cn ad

parseAPI :: String -> API
parseAPI = parse . scan

sp_type :: APIType -> Spec
sp_type ty =
    case ty of
      TyBasic bty -> SpNewtype $ SpecNewtype bty
      TyList  _   -> SpSynonym ty
      TyMaybe _   -> SpSynonym ty
      TyName  _   -> SpSynonym ty
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
