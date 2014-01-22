{-# LANGUAGE TemplateHaskell            #-}
module Data.API.Tools.Datatypes
    ( datatypesTool
    , type_nm
    , rep_type_nm
    , newtype_prj_nm
    , pref_con_nm
    , pref_field_nm
    ) where

import           Data.API.Tools.Combinators
import           Data.API.Types hiding (withUTC)

import           Data.Aeson
import qualified Data.CaseInsensitive           as CI
import           Data.Char
import           Data.String
import qualified Data.Text                      as T
import           Data.Time
import           Data.Typeable
import           Language.Haskell.TH


-- | Tool to generate datatypes and type synonyms corresponding to an API
datatypesTool :: APITool
datatypesTool = apiNodeTool $ apiSpecTool gen_sn_dt gen_sr_dt gen_su_dt gen_se_dt gen_sy


-- | Generate a type synonym definition
gen_sy :: APINode -> APIType -> Q [Dec]
gen_sy as ty = return [TySynD (type_nm as) [] $ mk_type ty]


-- | Generate a newtype definition, like this:
--
-- > newtype JobId = JobId { _JobId :: T.Text }
-- >     deriving (Show,IsString,Eq,Typeable)

gen_sn_dt :: APINode -> SpecNewtype -> Q [Dec]
gen_sn_dt as sn = return [NewtypeD [] nm [] c $ derive_leaf_nms ++ iss]
  where
    c   = RecC nm [(newtype_prj_nm as,NotStrict,mk_type $ TyBasic (snType sn))]

    nm  = rep_type_nm as

    iss = case snType sn of
            BTstring -> [''IsString]
            BTbinary -> []
            BTbool   -> []
            BTint    -> []
            BTutc    -> []



-- | Generate a record type definition, like this:
--
-- > data JobSpecId
-- >     = JobSpecId
-- >         { _jsi_id         :: JobId
-- >         , _jsi_input      :: JSInput
-- >         , _jsi_output     :: JSOutputStatus
-- >         , _jsi_pipelineId :: PipelineId
-- >         }
-- >     deriving (Show,Eq,Typeable)

gen_sr_dt :: APINode -> SpecRecord -> Q [Dec]
gen_sr_dt as sr = return [DataD [] nm [] cs derive_node_nms] -- [show_nm,eq_nm]
  where
    cs = [RecC nm [(pref_field_nm as fnm,IsStrict,mk_type (ftType fty)) |
                                                (fnm,fty)<-srFields sr]]

    nm = rep_type_nm as


-- | Generate a union type definition, like this:
--
-- > data Foo = F_Bar Int | F_Baz Bool
-- >     deriving (Show,Typeable)

gen_su_dt :: APINode -> SpecUnion -> Q [Dec]
gen_su_dt as su = return [DataD [] nm [] cs derive_node_nms] -- [show_nm,eq_nm]
  where
    cs = [NormalC (pref_con_nm as fnm) [(IsStrict,mk_type ty)] |
                                            (fnm,(ty,_))<-suFields su]

    nm = rep_type_nm as


-- | Generate an enum type definition, like this:
--
-- > data FrameRate
-- >     = FR_auto
-- >     | FR_10
-- >     | FR_15
-- >     | FR_23_97
-- >     | FR_24
-- >     | FR_25
-- >     | FR_29_97
-- >     | FR_30
-- >     | FR_60
-- >     deriving (Show,Eq,Ord,Bounded,Enum,Typeable)

gen_se_dt :: APINode -> SpecEnum -> Q [Dec]
gen_se_dt as se = return [DataD [] nm [] cs $ derive_leaf_nms ++ [''Bounded, ''Enum]]
  where
    cs = [NormalC (pref_con_nm as fnm) [] | (fnm,_) <- seAlts se ]

    nm = rep_type_nm as


mk_type :: APIType -> Type
mk_type ty =
    case ty of
      TyList  ty'  -> AppT ListT  $ mk_type ty'
      TyMaybe ty'  -> AppT (ConT ''Maybe) $ mk_type ty'
      TyName  nm   -> ConT  $ mkName $ _TypeName nm
      TyBasic bt   -> basic_type bt
      TyJSON       -> ConT ''Value

basic_type :: BasicType -> Type
basic_type bt =
    case bt of
      BTstring -> ConT ''T.Text
      BTbinary -> ConT ''Binary
      BTbool   -> ConT ''Bool
      BTint    -> ConT ''Int
      BTutc    -> ConT ''UTCTime


derive_leaf_nms :: [Name]
derive_leaf_nms = [''Show,''Eq,''Ord,''Typeable]

derive_node_nms :: [Name]
derive_node_nms = [''Show,''Eq,''Typeable]


-- | Name of the type corresponding to the API node, e.g. @JobId@
type_nm :: APINode -> Name
type_nm an = mkName $ _TypeName $ anName an

-- | Name of the representation type corresponding to the API node,
-- which differs from the 'type_nm' only if custom conversion
-- functions are specified.  This is also the name of the sole
-- constructor for newtypes and records.
rep_type_nm :: APINode -> Name
rep_type_nm an = mkName $ rep_type_s an

-- | Name of the single field in a newtype, prefixed by an underscore,
-- e.g. @_JobId@
newtype_prj_nm :: APINode -> Name
newtype_prj_nm an = mkName $ "_" ++ rep_type_s an

rep_type_s :: APINode -> String
rep_type_s an = f $ _TypeName $ anName an
  where
    f s = maybe s (const ("REP__"++s)) $ anConvert an

-- | Construct the name of a record field by attaching the
-- type-specific prefix, in lowercase, e.g. @_jsi_id@
pref_field_nm :: APINode -> FieldName -> Name
pref_field_nm as fnm = mkName $ pre ++ _FieldName fnm
  where
    pre = "_" ++ map toLower (CI.original $ anPrefix as) ++ "_"

-- | Construct the name of a union or enum constructor by attaching
-- the type-specific prefix, in uppercase, e.g. @FR_auto@
pref_con_nm :: APINode -> FieldName -> Name
pref_con_nm as fnm = mkName $ pre ++ _FieldName fnm
  where
    pre = map toUpper (CI.original $ anPrefix as) ++ "_"
