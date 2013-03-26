{-# LANGUAGE RecordWildCards #-}

-- This module extracts an API specified with the DSL as a JSON-encoded
-- object so that it can be imported into the client's framework
-- for building the client wrappers. 

module Data.API.API (extractAPI) where

import qualified Data.API.API.Gen               as D
import           Data.API.Types
import           Data.Aeson
import qualified Data.CaseInsensitive           as CI
import qualified Data.Text                      as T
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set


-- | Take and API spec and generate the JSON

extractAPI :: API -> Value
extractAPI = toJSON . map convert  

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
      SpNewtype sn -> D.SP_newtype $ convert_basic  $ snType   sn
      SpRecord  sr -> D.SP_record  $ convert_fields $ srFields sr 
      SpUnion   su -> D.SP_union   $ convert_fields $ suFields su
      SpEnum    se -> D.SP_enum    $ convert_alts   $ seAlts   se
      SpSynonym ty -> D.SP_synonym $ convert_type              ty

convert_conversion :: (FieldName,FieldName) -> D.Conversion
convert_conversion (i,p) =
        D.Conversion 
            { D._cv_injection  = T.pack $ _FieldName p
            , D._cv_projection = T.pack $ _FieldName i
            }

convert_fields :: Map.Map FieldName (APIType,MDComment) -> [D.Field]
convert_fields mp = map f $ Map.toList mp
  where
    f (fn,(ty,co)) =
        D.Field
            { D._fd_name    = T.pack $ _FieldName fn
            , D._fd_type    = convert_type ty 
            , D._fd_comment = T.pack co
            }

convert_alts :: Set.Set FieldName -> [T.Text] 
convert_alts st = map (T.pack . _FieldName) $ Set.toList st

convert_type :: APIType -> D.APIType
convert_type ty0 =
    case ty0 of
      TyList  ty  -> D.TY_list  $ convert_type ty
      TyMaybe ty  -> D.TY_maybe $ convert_type ty
      TyName  tnm -> D.TY_name  $ T.pack $ _TypeName tnm
      TyBasic bt  -> D.TY_basic $ convert_basic bt

convert_basic :: BasicType -> D.BasicType
convert_basic bt =
    case bt of
      BTstring -> D.BT_string
      BTbinary -> D.BT_binary
      BTbool   -> D.BT_boolean
      BTint    -> D.BT_integer
