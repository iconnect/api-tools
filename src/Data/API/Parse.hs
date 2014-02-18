{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}


module Data.API.Parse
    ( parseAPI
    , parseAPIWithChangelog
    , api
    , apiWithChangelog
    ) where

import           Data.API.Types
import           Data.API.Scan
import           Data.API.Changes
import           Data.Char
import           Data.String
import qualified Data.Text                  as T
import qualified Data.CaseInsensitive       as CI
import qualified Data.Version               as V
import           Distribution.Text (simpleParse)
import           Language.Haskell.TH.Quote
import           Text.Printf
import           Text.Regex

-- parser produced by Happy Version 1.19.2

data HappyAbsSyn 
	= HappyTerminal (PToken)
	| HappyErrorToken Int
	| HappyAbsSyn5 (APIWithChangelog)
	| HappyAbsSyn6 (API)
	| HappyAbsSyn7 ([Thing])
	| HappyAbsSyn8 (Thing)
	| HappyAbsSyn9 (APINode)
	| HappyAbsSyn10 (Spec)
	| HappyAbsSyn11 (Conversion)
	| HappyAbsSyn12 (MDComment)
	| HappyAbsSyn13 ([MDComment])
	| HappyAbsSyn14 (Prefix)
	| HappyAbsSyn15 (SpecRecord)
	| HappyAbsSyn16 (SpecUnion)
	| HappyAbsSyn17 ([(FieldName, FieldType)])
	| HappyAbsSyn18 (FieldType)
	| HappyAbsSyn19 (Bool)
	| HappyAbsSyn20 ([(FieldName,(APIType,MDComment))])
	| HappyAbsSyn21 (SpecEnum)
	| HappyAbsSyn22 ([(FieldName,MDComment)])
	| HappyAbsSyn23 (SpecNewtype)
	| HappyAbsSyn24 (Maybe Filter)
	| HappyAbsSyn25 (Filter)
	| HappyAbsSyn26 (RegEx)
	| HappyAbsSyn27 (APIType)
	| HappyAbsSyn28 (Maybe DefaultValue)
	| HappyAbsSyn29 (BasicType)
	| HappyAbsSyn30 (FieldName)
	| HappyAbsSyn31 (String)
	| HappyAbsSyn32 (TypeName)
	| HappyAbsSyn33 (APIChangelog)
	| HappyAbsSyn34 (V.Version)
	| HappyAbsSyn35 (VersionExtra)
	| HappyAbsSyn36 ([APIChange])
	| HappyAbsSyn37 ([[APIChange]])
	| HappyAbsSyn39 ([FieldChange])
	| HappyAbsSyn42 (DefaultValue)
	| HappyAbsSyn43 ([UnionChange])
	| HappyAbsSyn45 ([EnumChange])
	| HappyAbsSyn47 (MigrationTag)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
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
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
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
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (PToken)
	-> HappyState (PToken) (HappyStk HappyAbsSyn -> [(PToken)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (PToken) (HappyStk HappyAbsSyn -> [(PToken)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(PToken)] -> (HappyIdentity) HappyAbsSyn)

action_0 (6) = happyGoto action_6
action_0 (7) = happyGoto action_4
action_0 _ = happyReduce_5

action_1 (5) = happyGoto action_5
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 _ = happyReduce_5

action_2 (6) = happyGoto action_3
action_2 (7) = happyGoto action_4
action_2 _ = happyFail

action_3 (70) = happyShift action_8
action_3 _ = happyFail

action_4 (48) = happyShift action_7
action_4 _ = happyReduce_3

action_5 (90) = happyAccept
action_5 _ = happyFail

action_6 (90) = happyAccept
action_6 _ = happyFail

action_7 (75) = happyShift action_18
action_7 (76) = happyShift action_19
action_7 (77) = happyShift action_20
action_7 (79) = happyShift action_21
action_7 (84) = happyShift action_22
action_7 (8) = happyGoto action_13
action_7 (9) = happyGoto action_14
action_7 (12) = happyGoto action_15
action_7 (13) = happyGoto action_10
action_7 (14) = happyGoto action_16
action_7 (31) = happyGoto action_17
action_7 _ = happyReduce_18

action_8 (58) = happyShift action_12
action_8 (12) = happyGoto action_9
action_8 (13) = happyGoto action_10
action_8 (33) = happyGoto action_11
action_8 _ = happyReduce_18

action_9 (48) = happyShift action_28
action_9 _ = happyFail

action_10 (82) = happyShift action_27
action_10 _ = happyReduce_16

action_11 _ = happyReduce_2

action_12 (86) = happyShift action_26
action_12 (34) = happyGoto action_24
action_12 (35) = happyGoto action_25
action_12 _ = happyFail

action_13 _ = happyReduce_4

action_14 _ = happyReduce_6

action_15 _ = happyReduce_7

action_16 (52) = happyShift action_23
action_16 _ = happyFail

action_17 _ = happyReduce_19

action_18 _ = happyReduce_57

action_19 _ = happyReduce_58

action_20 _ = happyReduce_59

action_21 _ = happyReduce_60

action_22 _ = happyReduce_56

action_23 (83) = happyShift action_32
action_23 _ = happyFail

action_24 _ = happyReduce_63

action_25 (36) = happyGoto action_30
action_25 (37) = happyGoto action_31
action_25 _ = happyReduce_69

action_26 (90) = happyReduce_65
action_26 _ = happyReduce_66

action_27 _ = happyReduce_17

action_28 (58) = happyShift action_12
action_28 (12) = happyGoto action_9
action_28 (13) = happyGoto action_10
action_28 (33) = happyGoto action_29
action_28 _ = happyReduce_18

action_29 _ = happyReduce_64

action_30 (48) = happyShift action_41
action_30 _ = happyFail

action_31 (71) = happyShift action_35
action_31 (72) = happyShift action_36
action_31 (73) = happyShift action_37
action_31 (74) = happyShift action_38
action_31 (78) = happyShift action_39
action_31 (82) = happyShift action_40
action_31 (38) = happyGoto action_34
action_31 _ = happyReduce_67

action_32 (12) = happyGoto action_33
action_32 (13) = happyGoto action_10
action_32 _ = happyReduce_18

action_33 (53) = happyShift action_53
action_33 _ = happyFail

action_34 _ = happyReduce_68

action_35 (83) = happyShift action_50
action_35 (32) = happyGoto action_52
action_35 _ = happyFail

action_36 (83) = happyShift action_50
action_36 (32) = happyGoto action_51
action_36 _ = happyFail

action_37 (83) = happyShift action_50
action_37 (32) = happyGoto action_49
action_37 _ = happyFail

action_38 (66) = happyShift action_46
action_38 (67) = happyShift action_47
action_38 (68) = happyShift action_48
action_38 _ = happyFail

action_39 (66) = happyShift action_44
action_39 (83) = happyShift action_45
action_39 (47) = happyGoto action_43
action_39 _ = happyFail

action_40 _ = happyReduce_78

action_41 (58) = happyShift action_12
action_41 (12) = happyGoto action_9
action_41 (13) = happyGoto action_10
action_41 (33) = happyGoto action_42
action_41 _ = happyReduce_18

action_42 _ = happyReduce_62

action_43 _ = happyReduce_77

action_44 (83) = happyShift action_50
action_44 (32) = happyGoto action_79
action_44 _ = happyFail

action_45 _ = happyReduce_107

action_46 (83) = happyShift action_50
action_46 (32) = happyGoto action_78
action_46 _ = happyFail

action_47 (83) = happyShift action_50
action_47 (32) = happyGoto action_77
action_47 _ = happyFail

action_48 (83) = happyShift action_50
action_48 (32) = happyGoto action_76
action_48 _ = happyFail

action_49 (79) = happyShift action_75
action_49 _ = happyFail

action_50 _ = happyReduce_61

action_51 _ = happyReduce_71

action_52 (50) = happyShift action_61
action_52 (54) = happyShift action_62
action_52 (60) = happyShift action_63
action_52 (61) = happyShift action_64
action_52 (62) = happyShift action_65
action_52 (63) = happyShift action_66
action_52 (64) = happyShift action_67
action_52 (65) = happyShift action_68
action_52 (66) = happyShift action_69
action_52 (67) = happyShift action_70
action_52 (68) = happyShift action_71
action_52 (69) = happyShift action_72
action_52 (83) = happyShift action_73
action_52 (10) = happyGoto action_74
action_52 (15) = happyGoto action_55
action_52 (16) = happyGoto action_56
action_52 (21) = happyGoto action_57
action_52 (23) = happyGoto action_58
action_52 (27) = happyGoto action_59
action_52 (29) = happyGoto action_60
action_52 _ = happyFail

action_53 (50) = happyShift action_61
action_53 (54) = happyShift action_62
action_53 (60) = happyShift action_63
action_53 (61) = happyShift action_64
action_53 (62) = happyShift action_65
action_53 (63) = happyShift action_66
action_53 (64) = happyShift action_67
action_53 (65) = happyShift action_68
action_53 (66) = happyShift action_69
action_53 (67) = happyShift action_70
action_53 (68) = happyShift action_71
action_53 (69) = happyShift action_72
action_53 (83) = happyShift action_73
action_53 (10) = happyGoto action_54
action_53 (15) = happyGoto action_55
action_53 (16) = happyGoto action_56
action_53 (21) = happyGoto action_57
action_53 (23) = happyGoto action_58
action_53 (27) = happyGoto action_59
action_53 (29) = happyGoto action_60
action_53 _ = happyFail

action_54 (59) = happyShift action_105
action_54 (11) = happyGoto action_104
action_54 _ = happyReduce_15

action_55 _ = happyReduce_9

action_56 _ = happyReduce_10

action_57 _ = happyReduce_11

action_58 _ = happyReduce_12

action_59 _ = happyReduce_13

action_60 _ = happyReduce_46

action_61 (50) = happyShift action_61
action_61 (54) = happyShift action_62
action_61 (60) = happyShift action_63
action_61 (61) = happyShift action_64
action_61 (62) = happyShift action_65
action_61 (63) = happyShift action_66
action_61 (64) = happyShift action_67
action_61 (65) = happyShift action_68
action_61 (83) = happyShift action_73
action_61 (27) = happyGoto action_103
action_61 (29) = happyGoto action_60
action_61 _ = happyFail

action_62 (50) = happyShift action_61
action_62 (54) = happyShift action_62
action_62 (60) = happyShift action_63
action_62 (61) = happyShift action_64
action_62 (62) = happyShift action_65
action_62 (63) = happyShift action_66
action_62 (64) = happyShift action_67
action_62 (65) = happyShift action_68
action_62 (83) = happyShift action_73
action_62 (27) = happyGoto action_102
action_62 (29) = happyGoto action_60
action_62 _ = happyFail

action_63 _ = happyReduce_53

action_64 _ = happyReduce_52

action_65 _ = happyReduce_54

action_66 _ = happyReduce_50

action_67 _ = happyReduce_51

action_68 _ = happyReduce_47

action_69 (75) = happyShift action_18
action_69 (76) = happyShift action_19
action_69 (77) = happyShift action_20
action_69 (79) = happyShift action_21
action_69 (84) = happyShift action_22
action_69 (17) = happyGoto action_99
action_69 (30) = happyGoto action_100
action_69 (31) = happyGoto action_101
action_69 _ = happyFail

action_70 (49) = happyShift action_98
action_70 (20) = happyGoto action_97
action_70 _ = happyFail

action_71 (49) = happyShift action_96
action_71 (22) = happyGoto action_95
action_71 _ = happyFail

action_72 (60) = happyShift action_63
action_72 (61) = happyShift action_64
action_72 (62) = happyShift action_65
action_72 (63) = happyShift action_66
action_72 (64) = happyShift action_67
action_72 (29) = happyGoto action_94
action_72 _ = happyFail

action_73 _ = happyReduce_45

action_74 _ = happyReduce_70

action_75 (83) = happyShift action_50
action_75 (32) = happyGoto action_93
action_75 _ = happyFail

action_76 (77) = happyShift action_91
action_76 (82) = happyShift action_92
action_76 (45) = happyGoto action_89
action_76 (46) = happyGoto action_90
action_76 _ = happyFail

action_77 (77) = happyShift action_87
action_77 (82) = happyShift action_88
action_77 (43) = happyGoto action_85
action_77 (44) = happyGoto action_86
action_77 _ = happyFail

action_78 (76) = happyShift action_83
action_78 (82) = happyShift action_84
action_78 (39) = happyGoto action_81
action_78 (40) = happyGoto action_82
action_78 _ = happyFail

action_79 (83) = happyShift action_45
action_79 (47) = happyGoto action_80
action_79 _ = happyFail

action_80 _ = happyReduce_76

action_81 (76) = happyShift action_83
action_81 (82) = happyShift action_84
action_81 (40) = happyGoto action_128
action_81 _ = happyReduce_73

action_82 _ = happyReduce_80

action_83 (71) = happyShift action_124
action_83 (72) = happyShift action_125
action_83 (73) = happyShift action_126
action_83 (74) = happyShift action_127
action_83 _ = happyFail

action_84 _ = happyReduce_85

action_85 (77) = happyShift action_87
action_85 (82) = happyShift action_88
action_85 (44) = happyGoto action_123
action_85 _ = happyReduce_74

action_86 _ = happyReduce_96

action_87 (71) = happyShift action_120
action_87 (72) = happyShift action_121
action_87 (73) = happyShift action_122
action_87 _ = happyFail

action_88 _ = happyReduce_100

action_89 (77) = happyShift action_91
action_89 (82) = happyShift action_92
action_89 (46) = happyGoto action_119
action_89 _ = happyReduce_75

action_90 _ = happyReduce_102

action_91 (71) = happyShift action_116
action_91 (72) = happyShift action_117
action_91 (73) = happyShift action_118
action_91 _ = happyFail

action_92 _ = happyReduce_106

action_93 _ = happyReduce_72

action_94 (49) = happyShift action_115
action_94 (24) = happyGoto action_114
action_94 _ = happyReduce_34

action_95 (49) = happyShift action_113
action_95 _ = happyReduce_29

action_96 (75) = happyShift action_18
action_96 (76) = happyShift action_19
action_96 (77) = happyShift action_20
action_96 (79) = happyShift action_21
action_96 (84) = happyShift action_22
action_96 (30) = happyGoto action_112
action_96 (31) = happyGoto action_101
action_96 _ = happyFail

action_97 (49) = happyShift action_111
action_97 _ = happyReduce_21

action_98 (75) = happyShift action_18
action_98 (76) = happyShift action_19
action_98 (77) = happyShift action_20
action_98 (79) = happyShift action_21
action_98 (84) = happyShift action_22
action_98 (30) = happyGoto action_110
action_98 (31) = happyGoto action_101
action_98 _ = happyFail

action_99 (75) = happyShift action_18
action_99 (76) = happyShift action_19
action_99 (77) = happyShift action_20
action_99 (79) = happyShift action_21
action_99 (84) = happyShift action_22
action_99 (30) = happyGoto action_109
action_99 (31) = happyGoto action_101
action_99 _ = happyReduce_20

action_100 (52) = happyShift action_108
action_100 _ = happyFail

action_101 _ = happyReduce_55

action_102 _ = happyReduce_43

action_103 (51) = happyShift action_107
action_103 _ = happyFail

action_104 _ = happyReduce_8

action_105 (75) = happyShift action_18
action_105 (76) = happyShift action_19
action_105 (77) = happyShift action_20
action_105 (79) = happyShift action_21
action_105 (84) = happyShift action_22
action_105 (30) = happyGoto action_106
action_105 (31) = happyGoto action_101
action_105 _ = happyFail

action_106 (55) = happyShift action_151
action_106 _ = happyFail

action_107 _ = happyReduce_44

action_108 (50) = happyShift action_61
action_108 (54) = happyShift action_62
action_108 (60) = happyShift action_63
action_108 (61) = happyShift action_64
action_108 (62) = happyShift action_65
action_108 (63) = happyShift action_66
action_108 (64) = happyShift action_67
action_108 (65) = happyShift action_68
action_108 (83) = happyShift action_73
action_108 (18) = happyGoto action_149
action_108 (27) = happyGoto action_150
action_108 (29) = happyGoto action_60
action_108 _ = happyFail

action_109 (52) = happyShift action_148
action_109 _ = happyFail

action_110 (52) = happyShift action_147
action_110 _ = happyFail

action_111 (75) = happyShift action_18
action_111 (76) = happyShift action_19
action_111 (77) = happyShift action_20
action_111 (79) = happyShift action_21
action_111 (84) = happyShift action_22
action_111 (30) = happyGoto action_146
action_111 (31) = happyGoto action_101
action_111 _ = happyFail

action_112 (12) = happyGoto action_145
action_112 (13) = happyGoto action_10
action_112 _ = happyReduce_18

action_113 (75) = happyShift action_18
action_113 (76) = happyShift action_19
action_113 (77) = happyShift action_20
action_113 (79) = happyShift action_21
action_113 (84) = happyShift action_22
action_113 (30) = happyGoto action_144
action_113 (31) = happyGoto action_101
action_113 _ = happyFail

action_114 _ = happyReduce_32

action_115 (56) = happyShift action_141
action_115 (57) = happyShift action_142
action_115 (86) = happyShift action_143
action_115 (25) = happyGoto action_139
action_115 (26) = happyGoto action_140
action_115 _ = happyFail

action_116 (75) = happyShift action_18
action_116 (76) = happyShift action_19
action_116 (77) = happyShift action_20
action_116 (79) = happyShift action_21
action_116 (84) = happyShift action_22
action_116 (30) = happyGoto action_138
action_116 (31) = happyGoto action_101
action_116 _ = happyFail

action_117 (75) = happyShift action_18
action_117 (76) = happyShift action_19
action_117 (77) = happyShift action_20
action_117 (79) = happyShift action_21
action_117 (84) = happyShift action_22
action_117 (30) = happyGoto action_137
action_117 (31) = happyGoto action_101
action_117 _ = happyFail

action_118 (75) = happyShift action_18
action_118 (76) = happyShift action_19
action_118 (77) = happyShift action_20
action_118 (79) = happyShift action_21
action_118 (84) = happyShift action_22
action_118 (30) = happyGoto action_136
action_118 (31) = happyGoto action_101
action_118 _ = happyFail

action_119 _ = happyReduce_101

action_120 (75) = happyShift action_18
action_120 (76) = happyShift action_19
action_120 (77) = happyShift action_20
action_120 (79) = happyShift action_21
action_120 (84) = happyShift action_22
action_120 (30) = happyGoto action_135
action_120 (31) = happyGoto action_101
action_120 _ = happyFail

action_121 (75) = happyShift action_18
action_121 (76) = happyShift action_19
action_121 (77) = happyShift action_20
action_121 (79) = happyShift action_21
action_121 (84) = happyShift action_22
action_121 (30) = happyGoto action_134
action_121 (31) = happyGoto action_101
action_121 _ = happyFail

action_122 (75) = happyShift action_18
action_122 (76) = happyShift action_19
action_122 (77) = happyShift action_20
action_122 (79) = happyShift action_21
action_122 (84) = happyShift action_22
action_122 (30) = happyGoto action_133
action_122 (31) = happyGoto action_101
action_122 _ = happyFail

action_123 _ = happyReduce_95

action_124 (75) = happyShift action_18
action_124 (76) = happyShift action_19
action_124 (77) = happyShift action_20
action_124 (79) = happyShift action_21
action_124 (84) = happyShift action_22
action_124 (30) = happyGoto action_132
action_124 (31) = happyGoto action_101
action_124 _ = happyFail

action_125 (75) = happyShift action_18
action_125 (76) = happyShift action_19
action_125 (77) = happyShift action_20
action_125 (79) = happyShift action_21
action_125 (84) = happyShift action_22
action_125 (30) = happyGoto action_131
action_125 (31) = happyGoto action_101
action_125 _ = happyFail

action_126 (75) = happyShift action_18
action_126 (76) = happyShift action_19
action_126 (77) = happyShift action_20
action_126 (79) = happyShift action_21
action_126 (84) = happyShift action_22
action_126 (30) = happyGoto action_130
action_126 (31) = happyGoto action_101
action_126 _ = happyFail

action_127 (75) = happyShift action_18
action_127 (76) = happyShift action_19
action_127 (77) = happyShift action_20
action_127 (79) = happyShift action_21
action_127 (84) = happyShift action_22
action_127 (30) = happyGoto action_129
action_127 (31) = happyGoto action_101
action_127 _ = happyFail

action_128 _ = happyReduce_79

action_129 (52) = happyShift action_168
action_129 _ = happyFail

action_130 (79) = happyShift action_167
action_130 _ = happyFail

action_131 _ = happyReduce_82

action_132 (52) = happyShift action_166
action_132 _ = happyFail

action_133 (79) = happyShift action_165
action_133 _ = happyFail

action_134 _ = happyReduce_98

action_135 (52) = happyShift action_164
action_135 _ = happyFail

action_136 (79) = happyShift action_163
action_136 _ = happyFail

action_137 _ = happyReduce_104

action_138 _ = happyReduce_103

action_139 _ = happyReduce_33

action_140 _ = happyReduce_35

action_141 (85) = happyShift action_161
action_141 (89) = happyShift action_162
action_141 _ = happyFail

action_142 (85) = happyShift action_159
action_142 (89) = happyShift action_160
action_142 _ = happyFail

action_143 _ = happyReduce_42

action_144 (12) = happyGoto action_158
action_144 (13) = happyGoto action_10
action_144 _ = happyReduce_18

action_145 _ = happyReduce_31

action_146 (52) = happyShift action_157
action_146 _ = happyFail

action_147 (50) = happyShift action_61
action_147 (54) = happyShift action_62
action_147 (60) = happyShift action_63
action_147 (61) = happyShift action_64
action_147 (62) = happyShift action_65
action_147 (63) = happyShift action_66
action_147 (64) = happyShift action_67
action_147 (65) = happyShift action_68
action_147 (83) = happyShift action_73
action_147 (27) = happyGoto action_156
action_147 (29) = happyGoto action_60
action_147 _ = happyFail

action_148 (50) = happyShift action_61
action_148 (54) = happyShift action_62
action_148 (60) = happyShift action_63
action_148 (61) = happyShift action_64
action_148 (62) = happyShift action_65
action_148 (63) = happyShift action_66
action_148 (64) = happyShift action_67
action_148 (65) = happyShift action_68
action_148 (83) = happyShift action_73
action_148 (18) = happyGoto action_155
action_148 (27) = happyGoto action_150
action_148 (29) = happyGoto action_60
action_148 _ = happyFail

action_149 _ = happyReduce_23

action_150 (81) = happyShift action_154
action_150 (19) = happyGoto action_153
action_150 _ = happyReduce_26

action_151 (75) = happyShift action_18
action_151 (76) = happyShift action_19
action_151 (77) = happyShift action_20
action_151 (79) = happyShift action_21
action_151 (84) = happyShift action_22
action_151 (30) = happyGoto action_152
action_151 (31) = happyGoto action_101
action_151 _ = happyFail

action_152 _ = happyReduce_14

action_153 (50) = happyShift action_181
action_153 (80) = happyShift action_182
action_153 (85) = happyShift action_183
action_153 (86) = happyShift action_184
action_153 (87) = happyShift action_185
action_153 (88) = happyShift action_186
action_153 (89) = happyShift action_187
action_153 (28) = happyGoto action_179
action_153 (42) = happyGoto action_180
action_153 _ = happyReduce_49

action_154 _ = happyReduce_25

action_155 _ = happyReduce_22

action_156 (12) = happyGoto action_178
action_156 (13) = happyGoto action_10
action_156 _ = happyReduce_18

action_157 (50) = happyShift action_61
action_157 (54) = happyShift action_62
action_157 (60) = happyShift action_63
action_157 (61) = happyShift action_64
action_157 (62) = happyShift action_65
action_157 (63) = happyShift action_66
action_157 (64) = happyShift action_67
action_157 (65) = happyShift action_68
action_157 (83) = happyShift action_73
action_157 (27) = happyGoto action_177
action_157 (29) = happyGoto action_60
action_157 _ = happyFail

action_158 _ = happyReduce_30

action_159 (55) = happyShift action_176
action_159 _ = happyReduce_36

action_160 (55) = happyShift action_175
action_160 _ = happyReduce_39

action_161 _ = happyReduce_38

action_162 _ = happyReduce_41

action_163 (75) = happyShift action_18
action_163 (76) = happyShift action_19
action_163 (77) = happyShift action_20
action_163 (79) = happyShift action_21
action_163 (84) = happyShift action_22
action_163 (30) = happyGoto action_174
action_163 (31) = happyGoto action_101
action_163 _ = happyFail

action_164 (50) = happyShift action_61
action_164 (54) = happyShift action_62
action_164 (60) = happyShift action_63
action_164 (61) = happyShift action_64
action_164 (62) = happyShift action_65
action_164 (63) = happyShift action_66
action_164 (64) = happyShift action_67
action_164 (65) = happyShift action_68
action_164 (83) = happyShift action_73
action_164 (27) = happyGoto action_173
action_164 (29) = happyGoto action_60
action_164 _ = happyFail

action_165 (75) = happyShift action_18
action_165 (76) = happyShift action_19
action_165 (77) = happyShift action_20
action_165 (79) = happyShift action_21
action_165 (84) = happyShift action_22
action_165 (30) = happyGoto action_172
action_165 (31) = happyGoto action_101
action_165 _ = happyFail

action_166 (50) = happyShift action_61
action_166 (54) = happyShift action_62
action_166 (60) = happyShift action_63
action_166 (61) = happyShift action_64
action_166 (62) = happyShift action_65
action_166 (63) = happyShift action_66
action_166 (64) = happyShift action_67
action_166 (65) = happyShift action_68
action_166 (83) = happyShift action_73
action_166 (27) = happyGoto action_171
action_166 (29) = happyGoto action_60
action_166 _ = happyFail

action_167 (75) = happyShift action_18
action_167 (76) = happyShift action_19
action_167 (77) = happyShift action_20
action_167 (79) = happyShift action_21
action_167 (84) = happyShift action_22
action_167 (30) = happyGoto action_170
action_167 (31) = happyGoto action_101
action_167 _ = happyFail

action_168 (50) = happyShift action_61
action_168 (54) = happyShift action_62
action_168 (60) = happyShift action_63
action_168 (61) = happyShift action_64
action_168 (62) = happyShift action_65
action_168 (63) = happyShift action_66
action_168 (64) = happyShift action_67
action_168 (65) = happyShift action_68
action_168 (83) = happyShift action_73
action_168 (27) = happyGoto action_169
action_168 (29) = happyGoto action_60
action_168 _ = happyFail

action_169 (78) = happyShift action_195
action_169 _ = happyFail

action_170 _ = happyReduce_83

action_171 (75) = happyShift action_194
action_171 (41) = happyGoto action_193
action_171 _ = happyReduce_87

action_172 _ = happyReduce_99

action_173 _ = happyReduce_97

action_174 _ = happyReduce_105

action_175 (56) = happyShift action_192
action_175 _ = happyFail

action_176 (56) = happyShift action_191
action_176 _ = happyFail

action_177 (12) = happyGoto action_190
action_177 (13) = happyGoto action_10
action_177 _ = happyReduce_18

action_178 _ = happyReduce_28

action_179 (12) = happyGoto action_189
action_179 (13) = happyGoto action_10
action_179 _ = happyReduce_18

action_180 _ = happyReduce_48

action_181 (51) = happyShift action_188
action_181 _ = happyFail

action_182 _ = happyReduce_89

action_183 _ = happyReduce_93

action_184 _ = happyReduce_90

action_185 _ = happyReduce_91

action_186 _ = happyReduce_92

action_187 _ = happyReduce_94

action_188 _ = happyReduce_88

action_189 _ = happyReduce_24

action_190 _ = happyReduce_27

action_191 (85) = happyShift action_199
action_191 _ = happyFail

action_192 (89) = happyShift action_198
action_192 _ = happyFail

action_193 _ = happyReduce_81

action_194 (50) = happyShift action_181
action_194 (80) = happyShift action_182
action_194 (85) = happyShift action_183
action_194 (86) = happyShift action_184
action_194 (87) = happyShift action_185
action_194 (88) = happyShift action_186
action_194 (89) = happyShift action_187
action_194 (42) = happyGoto action_197
action_194 _ = happyFail

action_195 (83) = happyShift action_45
action_195 (47) = happyGoto action_196
action_195 _ = happyFail

action_196 _ = happyReduce_84

action_197 _ = happyReduce_86

action_198 _ = happyReduce_40

action_199 _ = happyReduce_37

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ((happy_var_1, happy_var_3)
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (reverse happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  7 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_3 : happy_var_1
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  7 happyReduction_5
happyReduction_5  =  HappyAbsSyn7
		 ([]
	)

happyReduce_6 = happySpecReduce_1  8 happyReduction_6
happyReduction_6 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (ThNode    happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn8
		 (ThComment happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyReduce 7 9 happyReduction_8
happyReduction_8 ((HappyAbsSyn11  happy_var_7) `HappyStk`
	(HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	(HappyTerminal ((,) _ (TypeIden happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (APINode (TypeName happy_var_3) happy_var_4 happy_var_1 happy_var_6 happy_var_7
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  10 happyReduction_9
happyReduction_9 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn10
		 (SpRecord  happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  10 happyReduction_10
happyReduction_10 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn10
		 (SpUnion   happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn10
		 (SpEnum    happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn10
		 (SpNewtype happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn10
		 (SpSynonym happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 11 happyReduction_14
happyReduction_14 ((HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Just (happy_var_2,happy_var_4)
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_0  11 happyReduction_15
happyReduction_15  =  HappyAbsSyn11
		 (Nothing
	)

happyReduce_16 = happySpecReduce_1  12 happyReduction_16
happyReduction_16 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (unlines $ reverse happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  13 happyReduction_17
happyReduction_17 (HappyTerminal ((,) _ (Comment  happy_var_2)))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_1
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  13 happyReduction_18
happyReduction_18  =  HappyAbsSyn13
		 ([]
	)

happyReduce_19 = happySpecReduce_1  14 happyReduction_19
happyReduction_19 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn14
		 (CI.mk happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  15 happyReduction_20
happyReduction_20 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (SpecRecord $ reverse happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  16 happyReduction_21
happyReduction_21 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (SpecUnion  $ reverse happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 4 17 happyReduction_22
happyReduction_22 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((happy_var_2,happy_var_4) : happy_var_1
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_3  17 happyReduction_23
happyReduction_23 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn17
		 ([(happy_var_1,happy_var_3)]
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 4 18 happyReduction_24
happyReduction_24 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	(HappyAbsSyn27  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (FieldType happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_1  19 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn19
		 (True
	)

happyReduce_26 = happySpecReduce_0  19 happyReduction_26
happyReduction_26  =  HappyAbsSyn19
		 (False
	)

happyReduce_27 = happyReduce 6 20 happyReduction_27
happyReduction_27 ((HappyAbsSyn12  happy_var_6) `HappyStk`
	(HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 ((happy_var_3,(happy_var_5,happy_var_6)) : happy_var_1
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 5 20 happyReduction_28
happyReduction_28 ((HappyAbsSyn12  happy_var_5) `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 ([(happy_var_2,(happy_var_4,happy_var_5))]
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_2  21 happyReduction_29
happyReduction_29 (HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (SpecEnum $ reverse happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happyReduce 4 22 happyReduction_30
happyReduction_30 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 ((happy_var_3,happy_var_4) : happy_var_1
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_3  22 happyReduction_31
happyReduction_31 (HappyAbsSyn12  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	_
	 =  HappyAbsSyn22
		 ([(happy_var_2,happy_var_3)]
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  23 happyReduction_32
happyReduction_32 (HappyAbsSyn24  happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (SpecNewtype happy_var_2 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  24 happyReduction_33
happyReduction_33 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (Just happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_0  24 happyReduction_34
happyReduction_34  =  HappyAbsSyn24
		 (Nothing
	)

happyReduce_35 = happySpecReduce_1  25 happyReduction_35
happyReduction_35 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 (FtrStrg happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  25 happyReduction_36
happyReduction_36 (HappyTerminal ((,) _ (Intg     happy_var_2)))
	_
	 =  HappyAbsSyn25
		 (FtrIntg $ IntRange (Just happy_var_2) Nothing
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happyReduce 5 25 happyReduction_37
happyReduction_37 ((HappyTerminal ((,) _ (Intg     happy_var_5))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((,) _ (Intg     happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (FtrIntg $ IntRange (Just happy_var_2) (Just happy_var_5)
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_2  25 happyReduction_38
happyReduction_38 (HappyTerminal ((,) _ (Intg     happy_var_2)))
	_
	 =  HappyAbsSyn25
		 (FtrIntg $ IntRange Nothing   (Just happy_var_2)
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  25 happyReduction_39
happyReduction_39 (HappyTerminal ((,) _ (UTCTIME  happy_var_2)))
	_
	 =  HappyAbsSyn25
		 (FtrUTC  $ UTCRange (Just happy_var_2) Nothing
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happyReduce 5 25 happyReduction_40
happyReduction_40 ((HappyTerminal ((,) _ (UTCTIME  happy_var_5))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((,) _ (UTCTIME  happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (FtrUTC  $ UTCRange (Just happy_var_2) (Just happy_var_5)
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_2  25 happyReduction_41
happyReduction_41 (HappyTerminal ((,) _ (UTCTIME  happy_var_2)))
	_
	 =  HappyAbsSyn25
		 (FtrUTC  $ UTCRange Nothing   (Just happy_var_2)
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  26 happyReduction_42
happyReduction_42 (HappyTerminal ((,) _ (Strg     happy_var_1)))
	 =  HappyAbsSyn26
		 (RegEx (T.pack happy_var_1) $ mkRegexWithOpts happy_var_1 False True
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_2  27 happyReduction_43
happyReduction_43 (HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (TyMaybe            happy_var_2
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  27 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (TyList             happy_var_2
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  27 happyReduction_45
happyReduction_45 (HappyTerminal ((,) _ (TypeIden happy_var_1)))
	 =  HappyAbsSyn27
		 (TyName   (TypeName happy_var_1)
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  27 happyReduction_46
happyReduction_46 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn27
		 (TyBasic            happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  27 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn27
		 (TyJSON
	)

happyReduce_48 = happySpecReduce_1  28 happyReduction_48
happyReduction_48 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn28
		 (Just happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_0  28 happyReduction_49
happyReduction_49  =  HappyAbsSyn28
		 (Nothing
	)

happyReduce_50 = happySpecReduce_1  29 happyReduction_50
happyReduction_50 _
	 =  HappyAbsSyn29
		 (BTstring
	)

happyReduce_51 = happySpecReduce_1  29 happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn29
		 (BTbinary
	)

happyReduce_52 = happySpecReduce_1  29 happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn29
		 (BTbool
	)

happyReduce_53 = happySpecReduce_1  29 happyReduction_53
happyReduction_53 _
	 =  HappyAbsSyn29
		 (BTint
	)

happyReduce_54 = happySpecReduce_1  29 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn29
		 (BTutc
	)

happyReduce_55 = happySpecReduce_1  30 happyReduction_55
happyReduction_55 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 (FieldName happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  31 happyReduction_56
happyReduction_56 (HappyTerminal ((,) _ (VarIden  happy_var_1)))
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  31 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn31
		 ("default"
	)

happyReduce_58 = happySpecReduce_1  31 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn31
		 ("field"
	)

happyReduce_59 = happySpecReduce_1  31 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn31
		 ("alternative"
	)

happyReduce_60 = happySpecReduce_1  31 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn31
		 ("to"
	)

happyReduce_61 = happySpecReduce_1  32 happyReduction_61
happyReduction_61 (HappyTerminal ((,) _ (TypeIden happy_var_1)))
	 =  HappyAbsSyn32
		 (TypeName happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happyReduce 5 33 happyReduction_62
happyReduction_62 ((HappyAbsSyn33  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_3) `HappyStk`
	(HappyAbsSyn35  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (ChangesUpTo happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_63 = happySpecReduce_2  33 happyReduction_63
happyReduction_63 (HappyAbsSyn34  happy_var_2)
	_
	 =  HappyAbsSyn33
		 (ChangesStart happy_var_2
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  33 happyReduction_64
happyReduction_64 (HappyAbsSyn33  happy_var_3)
	_
	_
	 =  HappyAbsSyn33
		 (happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  34 happyReduction_65
happyReduction_65 (HappyTerminal ((,) _ (Strg     happy_var_1)))
	 =  HappyAbsSyn34
		 (parseVer happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  35 happyReduction_66
happyReduction_66 (HappyTerminal ((,) _ (Strg     happy_var_1)))
	 =  HappyAbsSyn35
		 (parseVersionExtra happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  36 happyReduction_67
happyReduction_67 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 (concat (reverse happy_var_1)
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_2  37 happyReduction_68
happyReduction_68 (HappyAbsSyn36  happy_var_2)
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_2 : happy_var_1
	)
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_0  37 happyReduction_69
happyReduction_69  =  HappyAbsSyn37
		 ([]
	)

happyReduce_70 = happySpecReduce_3  38 happyReduction_70
happyReduction_70 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn36
		 ([ChAddType happy_var_2 (declNF happy_var_3)]
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_2  38 happyReduction_71
happyReduction_71 (HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn36
		 ([ChDeleteType happy_var_2]
	)
happyReduction_71 _ _  = notHappyAtAll 

happyReduce_72 = happyReduce 4 38 happyReduction_72
happyReduction_72 ((HappyAbsSyn32  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 ([ChRenameType happy_var_2 happy_var_4]
	) `HappyStk` happyRest

happyReduce_73 = happyReduce 4 38 happyReduction_73
happyReduction_73 ((HappyAbsSyn39  happy_var_4) `HappyStk`
	(HappyAbsSyn32  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 (map (fldChangeToAPIChange happy_var_3)   (reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_74 = happyReduce 4 38 happyReduction_74
happyReduction_74 ((HappyAbsSyn43  happy_var_4) `HappyStk`
	(HappyAbsSyn32  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 (map (unionChangeToAPIChange happy_var_3) (reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_75 = happyReduce 4 38 happyReduction_75
happyReduction_75 ((HappyAbsSyn45  happy_var_4) `HappyStk`
	(HappyAbsSyn32  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 (map (enumChangeToAPIChange happy_var_3)  (reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_76 = happyReduce 4 38 happyReduction_76
happyReduction_76 ((HappyAbsSyn47  happy_var_4) `HappyStk`
	(HappyAbsSyn32  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn36
		 ([ChCustomRecord happy_var_3 happy_var_4]
	) `HappyStk` happyRest

happyReduce_77 = happySpecReduce_2  38 happyReduction_77
happyReduction_77 (HappyAbsSyn47  happy_var_2)
	_
	 =  HappyAbsSyn36
		 ([ChCustomAll happy_var_2]
	)
happyReduction_77 _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  38 happyReduction_78
happyReduction_78 _
	 =  HappyAbsSyn36
		 ([]
	)

happyReduce_79 = happySpecReduce_2  39 happyReduction_79
happyReduction_79 (HappyAbsSyn39  happy_var_2)
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_2 ++ happy_var_1
	)
happyReduction_79 _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  39 happyReduction_80
happyReduction_80 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happyReduce 6 40 happyReduction_81
happyReduction_81 ((HappyAbsSyn28  happy_var_6) `HappyStk`
	(HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 ([FldChAdd happy_var_3 happy_var_5 happy_var_6]
	) `HappyStk` happyRest

happyReduce_82 = happySpecReduce_3  40 happyReduction_82
happyReduction_82 (HappyAbsSyn30  happy_var_3)
	_
	_
	 =  HappyAbsSyn39
		 ([FldChDelete happy_var_3]
	)
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happyReduce 5 40 happyReduction_83
happyReduction_83 ((HappyAbsSyn30  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 ([FldChRename happy_var_3 happy_var_5]
	) `HappyStk` happyRest

happyReduce_84 = happyReduce 7 40 happyReduction_84
happyReduction_84 ((HappyAbsSyn47  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 ([FldChChange happy_var_3 happy_var_5 happy_var_7]
	) `HappyStk` happyRest

happyReduce_85 = happySpecReduce_1  40 happyReduction_85
happyReduction_85 _
	 =  HappyAbsSyn39
		 ([]
	)

happyReduce_86 = happySpecReduce_2  41 happyReduction_86
happyReduction_86 (HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (Just happy_var_2
	)
happyReduction_86 _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_0  41 happyReduction_87
happyReduction_87  =  HappyAbsSyn28
		 (Nothing
	)

happyReduce_88 = happySpecReduce_2  42 happyReduction_88
happyReduction_88 _
	_
	 =  HappyAbsSyn42
		 (DefValList
	)

happyReduce_89 = happySpecReduce_1  42 happyReduction_89
happyReduction_89 _
	 =  HappyAbsSyn42
		 (DefValMaybe
	)

happyReduce_90 = happySpecReduce_1  42 happyReduction_90
happyReduction_90 (HappyTerminal ((,) _ (Strg     happy_var_1)))
	 =  HappyAbsSyn42
		 (DefValString (T.pack happy_var_1)
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_1  42 happyReduction_91
happyReduction_91 _
	 =  HappyAbsSyn42
		 (DefValBool True
	)

happyReduce_92 = happySpecReduce_1  42 happyReduction_92
happyReduction_92 _
	 =  HappyAbsSyn42
		 (DefValBool False
	)

happyReduce_93 = happySpecReduce_1  42 happyReduction_93
happyReduction_93 (HappyTerminal ((,) _ (Intg     happy_var_1)))
	 =  HappyAbsSyn42
		 (DefValInt happy_var_1
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  42 happyReduction_94
happyReduction_94 (HappyTerminal ((,) _ (UTCTIME  happy_var_1)))
	 =  HappyAbsSyn42
		 (DefValUtc happy_var_1
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_2  43 happyReduction_95
happyReduction_95 (HappyAbsSyn43  happy_var_2)
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn43
		 (happy_var_2 ++ happy_var_1
	)
happyReduction_95 _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  43 happyReduction_96
happyReduction_96 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn43
		 (happy_var_1
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happyReduce 5 44 happyReduction_97
happyReduction_97 ((HappyAbsSyn27  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn43
		 ([UnChAdd happy_var_3 happy_var_5]
	) `HappyStk` happyRest

happyReduce_98 = happySpecReduce_3  44 happyReduction_98
happyReduction_98 (HappyAbsSyn30  happy_var_3)
	_
	_
	 =  HappyAbsSyn43
		 ([UnChDelete happy_var_3]
	)
happyReduction_98 _ _ _  = notHappyAtAll 

happyReduce_99 = happyReduce 5 44 happyReduction_99
happyReduction_99 ((HappyAbsSyn30  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn43
		 ([UnChRename happy_var_3 happy_var_5]
	) `HappyStk` happyRest

happyReduce_100 = happySpecReduce_1  44 happyReduction_100
happyReduction_100 _
	 =  HappyAbsSyn43
		 ([]
	)

happyReduce_101 = happySpecReduce_2  45 happyReduction_101
happyReduction_101 (HappyAbsSyn45  happy_var_2)
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (happy_var_2 ++ happy_var_1
	)
happyReduction_101 _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  45 happyReduction_102
happyReduction_102 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (happy_var_1
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_3  46 happyReduction_103
happyReduction_103 (HappyAbsSyn30  happy_var_3)
	_
	_
	 =  HappyAbsSyn45
		 ([EnChAdd happy_var_3]
	)
happyReduction_103 _ _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_3  46 happyReduction_104
happyReduction_104 (HappyAbsSyn30  happy_var_3)
	_
	_
	 =  HappyAbsSyn45
		 ([EnChDelete happy_var_3]
	)
happyReduction_104 _ _ _  = notHappyAtAll 

happyReduce_105 = happyReduce 5 46 happyReduction_105
happyReduction_105 ((HappyAbsSyn30  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn45
		 ([EnChRename happy_var_3 happy_var_5]
	) `HappyStk` happyRest

happyReduce_106 = happySpecReduce_1  46 happyReduction_106
happyReduction_106 _
	 =  HappyAbsSyn45
		 ([]
	)

happyReduce_107 = happySpecReduce_1  47 happyReduction_107
happyReduction_107 (HappyTerminal ((,) _ (TypeIden happy_var_1)))
	 =  HappyAbsSyn47
		 (happy_var_1
	)
happyReduction_107 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 90 90 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	(,) _ Semi -> cont 48;
	(,) _ Bar -> cont 49;
	(,) _ Bra -> cont 50;
	(,) _ Ket -> cont 51;
	(,) _ ColCol -> cont 52;
	(,) _ Equals -> cont 53;
	(,) _ Query -> cont 54;
	(,) _ Comma -> cont 55;
	(,) _ LtEq -> cont 56;
	(,) _ GtEq -> cont 57;
	(,) _ Version -> cont 58;
	(,) _ With -> cont 59;
	(,) _ Integer -> cont 60;
	(,) _ Boolean -> cont 61;
	(,) _ UTC -> cont 62;
	(,) _ String -> cont 63;
	(,) _ BInary -> cont 64;
	(,) _ Json -> cont 65;
	(,) _ Record -> cont 66;
	(,) _ Union -> cont 67;
	(,) _ Enum -> cont 68;
	(,) _ Basic -> cont 69;
	(,) _ Changes -> cont 70;
	(,) _ Added -> cont 71;
	(,) _ Removed -> cont 72;
	(,) _ Renamed -> cont 73;
	(,) _ Changed -> cont 74;
	(,) _ Default -> cont 75;
	(,) _ Field -> cont 76;
	(,) _ Alternative -> cont 77;
	(,) _ Migration -> cont 78;
	(,) _ To -> cont 79;
	(,) _ NOTHING -> cont 80;
	(,) _ Readonly -> cont 81;
	(,) _ (Comment  happy_dollar_dollar) -> cont 82;
	(,) _ (TypeIden happy_dollar_dollar) -> cont 83;
	(,) _ (VarIden  happy_dollar_dollar) -> cont 84;
	(,) _ (Intg     happy_dollar_dollar) -> cont 85;
	(,) _ (Strg     happy_dollar_dollar) -> cont 86;
	(,) _ TRUE -> cont 87;
	(,) _ FALSE -> cont 88;
	(,) _ (UTCTIME  happy_dollar_dollar) -> cont 89;
	_ -> happyError' (tk:tks)
	}

happyError_ 90 tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

parse_with_changelog tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [PToken] -> a
happyError tks = error $ printf "Syntax error at %s: %s\n" loc $ show (take 5 tks)
  where
    loc = case tks of
            []                    -> "<EOF>"
            (AlexPn ad ln cn,_):_ -> printf "line %d, column %d (@%d)" ln cn ad

parseAPI :: String -> API
parseAPI = parse . scan

parseAPIWithChangelog :: String -> APIWithChangelog
parseAPIWithChangelog = parse_with_changelog . scan


data FieldChange = FldChAdd FieldName APIType (Maybe DefaultValue)
                 | FldChDelete FieldName
                 | FldChRename FieldName FieldName
                 | FldChChange FieldName APIType MigrationTag

fldChangeToAPIChange :: TypeName -> FieldChange -> APIChange
fldChangeToAPIChange t (FldChAdd f ty def)  = ChAddField t f ty def
fldChangeToAPIChange t (FldChDelete f)      = ChDeleteField t f
fldChangeToAPIChange t (FldChRename f f')   = ChRenameField t f f'
fldChangeToAPIChange t (FldChChange f ty m) = ChChangeField t f ty m

data UnionChange = UnChAdd FieldName APIType
                 | UnChDelete FieldName
                 | UnChRename FieldName FieldName

unionChangeToAPIChange :: TypeName -> UnionChange -> APIChange
unionChangeToAPIChange t (UnChAdd f ty)    = ChAddUnionAlt t f ty
unionChangeToAPIChange t (UnChDelete f)    = ChDeleteUnionAlt t f
unionChangeToAPIChange t (UnChRename f f') = ChRenameUnionAlt t f f'

data EnumChange = EnChAdd FieldName
                | EnChDelete FieldName
                | EnChRename FieldName FieldName

enumChangeToAPIChange :: TypeName -> EnumChange -> APIChange
enumChangeToAPIChange t (EnChAdd f)       = ChAddEnumVal t f
enumChangeToAPIChange t (EnChDelete f)    = ChDeleteEnumVal t f
enumChangeToAPIChange t (EnChRename f f') = ChRenameEnumVal t f f'


parseVer :: String -> V.Version
parseVer x = case simpleParse x of
                 Just v -> v
                 Nothing -> error $ "Syntax error while parsing version " ++ x

parseVersionExtra :: String -> VersionExtra
parseVersionExtra "development" = DevVersion
parseVersionExtra s             = Release $ parseVer s


api :: QuasiQuoter
api =
    QuasiQuoter
        { quoteExp  = \s -> [| parseAPI s |]
        , quotePat  = error "api QuasiQuoter used in patten      context"
        , quoteType = error "api QuasiQuoter used in type        context"
        , quoteDec  = error "api QuasiQuoter used in declaration context"
        }

apiWithChangelog :: QuasiQuoter
apiWithChangelog =
    QuasiQuoter
        { quoteExp  = \s -> [| parseAPIWithChangelog s |]
        , quotePat  = error "apiWithChangelog QuasiQuoter used in patten      context"
        , quoteType = error "apiWithChangelog QuasiQuoter used in type        context"
        , quoteDec  = error "apiWithChangelog QuasiQuoter used in declaration context"
        }
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








{-# LINE 5 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

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

{-# LINE 155 "templates/GenericTemplate.hs" #-}

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
     let i = (case x of { HappyErrorToken (i) -> i }) in
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
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
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
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
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
