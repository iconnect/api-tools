{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains datatypes generated from the DSL description
-- of the api-tools API; they thus correspond to the types in
-- "Data.API.Types".
module Data.API.API.Gen where

import           Data.API.API.DSL
import           Data.API.Tools

import           Language.Haskell.TH
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Arbitrary as QC
import GHC.Generics

$(generate         apiAPI)

prop_genIsEqual :: (Eq a, Show a, QC.GSubterms (Rep a) a, QC.RecursivelyShrink (Rep a), Generic a, Shrinkable a)
                => a
                -> QC.Property
prop_genIsEqual a = QC.genericShrink a QC.=== shrinkable a

deriving instance Generic TypeRef -- no shrink
deriving instance Generic Field
deriving instance Generic Conversion
deriving instance Generic UTCRange
deriving instance Generic IntRange
deriving instance Generic RegularExpression
deriving instance Generic SpecNewtype
deriving instance Generic APINode
deriving instance Generic APIType
deriving instance Generic Spec
deriving instance Generic Filter
deriving instance Generic DefaultValue
deriving instance Generic BasicType

instance Shrinkable a => Shrinkable (Maybe a) where
  shrinkable (Just x) = Nothing : [ Just x' | x' <- shrinkable x ]
  shrinkable Nothing  = []
instance Shrinkable TypeRef -- no shrink
instance Shrinkable Field where
  shrinkable Field{..} =
    (Field <$> pure _fd_name <*> shrinkable _fd_type <*> pure _fd_readonly <*> pure _fd_default <*> pure _fd_comment) ++
    (Field <$> pure _fd_name <*> pure _fd_type <*> QC.shrink _fd_readonly <*> pure _fd_default <*> pure _fd_comment) ++
    (Field <$> pure _fd_name <*> pure _fd_type <*> pure _fd_readonly <*> shrinkable _fd_default <*> pure _fd_comment) ++
    (Field <$> pure _fd_name <*> pure _fd_type <*> pure _fd_readonly <*> pure _fd_default <*> QC.shrink _fd_comment)
instance Shrinkable Conversion -- no shrink
instance Shrinkable UTCRange where
  shrinkable (UTCRange x y)   = (UTCRange <$> QC.shrink x <*> pure y) ++
                                (UTCRange <$> pure x <*> QC.shrink y)
instance Shrinkable IntRange where
  shrinkable (IntRange x y)   = (IntRange <$> QC.shrink x <*> pure y) ++
                                (IntRange <$> pure x <*> QC.shrink y)
instance Shrinkable RegularExpression where
  shrinkable (RegularExpression e) = map RegularExpression (QC.shrink e)
instance Shrinkable SpecNewtype where
  shrinkable SpecNewtype{..} =
    (SpecNewtype <$> shrinkable _sn_type <*> pure _sn_filter) ++
    (SpecNewtype <$> pure _sn_type <*> shrinkable _sn_filter)
instance Shrinkable APINode where
  shrinkable APINode{..} =
    (APINode <$> QC.shrink _an_name  <*> pure _an_comment <*> pure _an_prefix <*> pure _an_spec <*> pure _an_convert) ++
    (APINode <$> pure _an_name <*> QC.shrink _an_comment <*> pure _an_prefix <*> pure _an_spec <*> pure _an_convert) ++
    (APINode <$> pure _an_name <*> pure _an_comment <*> QC.shrink _an_prefix <*> pure _an_spec <*> pure _an_convert) ++
    (APINode <$> pure _an_name <*> pure _an_comment <*> pure _an_prefix <*> shrinkable _an_spec <*> pure _an_convert) ++
    (APINode <$> pure _an_name <*> pure _an_comment <*> pure _an_prefix <*> pure _an_spec <*> shrinkable _an_convert)
instance Shrinkable APIType where -- OK
  shrinkable = \case
    TY_list  aty -> aty : (TY_list  <$> shrinkable aty)
    TY_maybe aty -> aty : (TY_maybe <$> shrinkable aty)
    TY_ref   tre -> TY_ref   <$> shrinkable tre
    TY_basic bt  -> TY_basic <$> shrinkable bt
    TY_json  i   -> TY_json <$> QC.shrink i
instance Shrinkable Spec where -- ok
  shrinkable = \case
    SP_newtype  sn -> SP_newtype <$> shrinkable sn
    SP_record   rc -> SP_record  <$> QC.shrinkList shrinkable rc
    SP_union    un -> SP_union   <$> QC.shrinkList shrinkable un
    SP_enum     en -> SP_enum    <$> QC.shrink en  -- rely on QC shrinking for Text
    SP_synonym  sy -> SP_synonym <$> shrinkable sy
instance Shrinkable Filter where
  shrinkable = \case
      FT_string  re -> FT_string  <$> shrinkable re
      FT_integer ir -> FT_integer <$> shrinkable ir
      FT_utc     ur -> FT_utc     <$> shrinkable ur
instance Shrinkable DefaultValue where
  shrinkable = \case
    DV_list    x -> DV_list    <$> QC.shrink x
    DV_maybe   x -> DV_maybe   <$> QC.shrink x
    DV_string  x -> DV_string  <$> QC.shrink x
    DV_boolean x -> DV_boolean <$> QC.shrink x
    DV_integer x -> DV_integer <$> QC.shrink x
    DV_utc     x -> DV_utc     <$> QC.shrink x
instance Shrinkable BasicType -- no shrink

$(generateAPITools apiAPI
                   [ enumTool
                   , jsonTool'
                   , cborTool
                   , deepSeqTool
                   , quickCheckTool
                   , lensTool
                   , safeCopyTool
                   , exampleTool
                   , samplesTool   (mkName "apiAPISamples")
                   , jsonTestsTool (mkName "apiAPITestsJSON")
                   , cborTestsTool (mkName "apiAPITestsCBOR")
                   , cborToJSONTestsTool 'apiAPI (mkName "apiAPITestsCBORToJSON")
                   , jsonToCBORTestsTool 'apiAPI (mkName "apiAPITestsJSONToCBOR")
                   ])
