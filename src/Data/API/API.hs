{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module converts an API specified with the DSL into a
-- JSON-encoded object so that it can be used in clients.
module Data.API.API
    ( apiAPI
    , extractAPI
    ) where

import           Data.API.API.DSL
import qualified Data.API.API.Gen               as D
import           Data.API.Types
import           Data.API.JSON

import           Data.Aeson
import qualified Data.CaseInsensitive           as CI
import qualified Data.Text                      as T
import           Control.Applicative
import           Text.Regex


-- | Take an API spec and generate a JSON description of the API

extractAPI :: API -> Value
extractAPI api = toJSON $ map convert [ an | ThNode an <- api ]

convert :: APINode -> D.APINode
convert (APINode{..}) =
    D.APINode
        { D._an_name    = T.pack $ _TypeName      anName
        , D._an_comment = T.pack                  anComment
        , D._an_prefix  = T.pack $ CI.original    anPrefix
        , D._an_spec    = convert_spec            anSpec
        , D._an_convert = fmap convert_conversion anConvert
        , D._an_version = _Vrn                    anVersion
        , D._an_log     = T.pack                  anLog
        }

convert_spec :: Spec -> D.Spec
convert_spec sp =
    case sp of
      SpNewtype sn -> D.SP_newtype $ convert_specnt            sn
      SpRecord  sr -> D.SP_record  $ convert_fields $ srFields sr
      SpUnion   su -> D.SP_union   $ convert_union  $ suFields su
      SpEnum    se -> D.SP_enum    $ convert_alts   $ seAlts   se
      SpSynonym ty -> D.SP_synonym $ convert_type              ty

convert_conversion :: (FieldName,FieldName) -> D.Conversion
convert_conversion (i,p) =
    D.Conversion
        { D._cv_injection  = T.pack $ _FieldName p
        , D._cv_projection = T.pack $ _FieldName i
        }

convert_specnt :: SpecNewtype -> D.SpecNewtype
convert_specnt sn =
    D.SpecNewtype
        { D._sn_type   = convert_basic   $  snType   sn
        , D._sn_filter = convert_filter <$> snFilter sn
        }

convert_filter :: Filter -> D.Filter
convert_filter ftr =
    case ftr of
      FtrStrg RegEx{..}    -> D.FT_string  $ D.RegularExpression re_text
      FtrIntg IntRange{..} -> D.FT_integer $ D.IntRange  ir_lo ir_hi
      FtrUTC  UTCRange{..} -> D.FT_utc     $ D.UTCRange  ur_lo ur_hi

convert_fields :: [(FieldName, FieldType)] -> [D.Field]
convert_fields al = map f al
  where
    f (fn,fty) =
        D.Field
            { D._fd_name     = T.pack $ _FieldName fn
            , D._fd_type     = convert_type $ ftType fty
            , D._fd_readonly = ftReadOnly fty
            , D._fd_default  = convert_default <$> ftDefault fty
            , D._fd_comment  = T.pack $ ftComment fty
            }

convert_union :: [(FieldName, (APIType, MDComment))] -> [D.Field]
convert_union al = map f al
  where
    f (fn,(ty,co)) =
        D.Field
            { D._fd_name     = T.pack $ _FieldName fn
            , D._fd_type     = convert_type ty
            , D._fd_readonly = False
            , D._fd_default  = Nothing
            , D._fd_comment  = T.pack co
            }

convert_alts :: [(FieldName,MDComment)] -> [T.Text]
convert_alts fns = map (T.pack . _FieldName . fst) fns

convert_type :: APIType -> D.APIType
convert_type ty0 =
    case ty0 of
      TyList  ty    -> D.TY_list  $ convert_type     ty
      TyMaybe ty    -> D.TY_maybe $ convert_type     ty
      TyName  tn    -> D.TY_ref   $ convert_ref      tn
      TyBasic bt    -> D.TY_basic $ convert_basic    bt
      TyJSON        -> D.TY_json    0

convert_ref :: TypeName -> D.TypeRef
convert_ref (TypeName tn) = D.TypeRef (T.pack tn)

convert_basic :: BasicType -> D.BasicType
convert_basic bt =
    case bt of
      BTstring -> D.BT_string
      BTbinary -> D.BT_binary
      BTbool   -> D.BT_boolean
      BTint    -> D.BT_integer
      BTutc    -> D.BT_utc

convert_default :: DefaultValue -> D.DefaultValue
convert_default DefValList       = D.DV_list    0
convert_default DefValMaybe      = D.DV_maybe   0
convert_default (DefValString s) = D.DV_string  s
convert_default (DefValBool   b) = D.DV_boolean b
convert_default (DefValInt    i) = D.DV_integer i
convert_default (DefValUtc    u) = D.DV_utc     u



-- | Generate an API spec from the JSON

instance FromJSONWithErrs Thing where
    parseJSONWithErrs v = (ThNode . unconvert) <$> parseJSONWithErrs v

unconvert :: D.APINode -> APINode
unconvert (D.APINode{..}) =
    APINode
        { anName    = TypeName $ T.unpack       _an_name
        , anComment = T.unpack                  _an_comment
        , anPrefix  = CI.mk $ T.unpack          _an_prefix
        , anSpec    = unconvert_spec            _an_spec
        , anConvert = fmap unconvert_conversion _an_convert
        , anVersion = Vrn                       _an_version
        , anLog     = T.unpack                  _an_log
        }

unconvert_spec :: D.Spec -> Spec
unconvert_spec sp =
    case sp of
      D.SP_newtype sn -> SpNewtype $ unconvert_specnt sn
      D.SP_record  sr -> SpRecord  $ SpecRecord $ unconvert_fields sr
      D.SP_union   su -> SpUnion   $ SpecUnion  $ unconvert_union su
      D.SP_enum    se -> SpEnum    $ SpecEnum   $ unconvert_alts   se
      D.SP_synonym ty -> SpSynonym $ unconvert_type   ty

unconvert_conversion :: D.Conversion -> (FieldName, FieldName)
unconvert_conversion c =
    ( FieldName $ T.unpack $ D._cv_injection  c
    , FieldName $ T.unpack $ D._cv_projection c
    )

unconvert_specnt :: D.SpecNewtype -> SpecNewtype
unconvert_specnt sn =
    SpecNewtype
        { snType   = unconvert_basic $    D._sn_type   sn
        , snFilter = unconvert_filter <$> D._sn_filter sn
        }

unconvert_filter :: D.Filter -> Filter
unconvert_filter ftr =
    case ftr of
      D.FT_string (D.RegularExpression re_text) -> FtrStrg $ RegEx re_text (mkRegexWithOpts (T.unpack re_text) False True)
      D.FT_integer (D.IntRange ir_lo ir_hi)     -> FtrIntg $ IntRange ir_lo ir_hi
      D.FT_utc (D.UTCRange ur_lo ur_hi)         -> FtrUTC $ UTCRange ur_lo ur_hi

unconvert_fields :: [D.Field] -> [(FieldName, FieldType)]
unconvert_fields al = map f al
  where
    f fld = ( FieldName $ T.unpack $ D._fd_name fld
            , FieldType { ftType     = unconvert_type $ D._fd_type fld
                        , ftReadOnly = D._fd_readonly fld
                        , ftDefault  = unconvert_default <$> D._fd_default fld
                        , ftComment  = T.unpack $ D._fd_comment fld
                        }
            )

unconvert_union :: [D.Field] -> [(FieldName, (APIType, MDComment))]
unconvert_union al = map f al
  where
    f fld = ( FieldName $ T.unpack $ D._fd_name fld
            , ( unconvert_type $ D._fd_type fld
              , T.unpack $ D._fd_comment fld
            ))

unconvert_alts :: [T.Text] -> [(FieldName,MDComment)]
unconvert_alts fns = map ((\x -> (x, "")) . FieldName . T.unpack) fns

unconvert_type :: D.APIType -> APIType
unconvert_type ty0 =
    case ty0 of
      D.TY_list  ty   -> TyList  $ unconvert_type  ty
      D.TY_maybe ty   -> TyMaybe $ unconvert_type  ty
      D.TY_ref   r    -> TyName  $ unconvert_ref r
      D.TY_basic bt   -> TyBasic $ unconvert_basic bt
      D.TY_json _     -> TyJSON

unconvert_ref :: D.TypeRef -> TypeName
unconvert_ref (D.TypeRef tn) = TypeName $ T.unpack tn

unconvert_basic :: D.BasicType -> BasicType
unconvert_basic bt =
    case bt of
      D.BT_string  -> BTstring
      D.BT_binary  -> BTbinary
      D.BT_boolean -> BTbool
      D.BT_integer -> BTint
      D.BT_utc     -> BTutc

unconvert_default :: D.DefaultValue -> DefaultValue
unconvert_default (D.DV_list    _) = DefValList
unconvert_default (D.DV_maybe   _) = DefValMaybe
unconvert_default (D.DV_string  s) = DefValString s
unconvert_default (D.DV_boolean b) = DefValBool   b
unconvert_default (D.DV_integer i) = DefValInt    i
unconvert_default (D.DV_utc     u) = DefValUtc    u
