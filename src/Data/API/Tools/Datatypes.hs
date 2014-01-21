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
import           Data.API.Types hiding (withUTC, withBinary)
import           Data.API.JSON

import qualified Data.CaseInsensitive           as CI
import           Data.Char
import           Data.String
import qualified Data.Text                      as T
import           Data.Time
import           Data.Typeable
import           Language.Haskell.TH


datatypesTool :: APITool
datatypesTool = apiNodeTool gen


gen :: APINode -> Q [Dec]
gen an =
    case anSpec an of
      SpNewtype sn -> gen_sn_dt an sn
      SpRecord  sr -> gen_sr_dt an sr
      SpUnion   su -> gen_su_dt an su
      SpEnum    se -> gen_se_dt an se
      SpSynonym ty -> gen_sy an ty


gen_sy :: APINode -> APIType -> Q [Dec]
gen_sy as ty = return [TySynD nm [] $ mk_type ty]
  where
    nm = type_nm as

{-
-- a sample newtype definition, wrapper and test function
-- we are trying to generate something like this

newtype JobId = JobId { _JobId :: T.Text }
    deriving (Show,IsString,Eq,Typeable)

instance ToJSON JobId where
    toJSON = String . _JobId

instance FromJSONWithErrs JobId where
    parseJSONWithErrs = withText "JobId" (pure . JobId)
-}

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


{-
-- a sample record type definition, wrapper and test function
-- we are trying to generate something like this

data JobSpecId
    = JobSpecId
        { jsiId         :: JobId
        , jsiInput      :: JSInput
        , jsiOutput     :: JSOutputStatus
        , jsiPipelineId :: PipelineId
        }
    deriving (Show,Eq,Typeable)

instance FromJSONWithErrs JobSpecId where
     parseJSONWithErrs (Object v) =
        JobSpecId <$>
            v .: "Id"                               <*>
            v .: "Input"                            <*>
            v .: "Output"                           <*>
            v .: "PipelineId"
     parseJSONWithErrs v          = failWith $ expectedObject val

instance ToJSON JobSpecId where
     toJSON jsi@(JobSpecId _ _ _ _) =
        object
            [ "Id"         .= jsiId         jsi
            , "Input"      .= jsiInput      jsi
            , "Output"     .= jsiOutput     jsi
            , "PipelineId" .= jsiPipelineId jsi
            ]
-}

gen_sr_dt :: APINode -> SpecRecord -> Q [Dec]
gen_sr_dt as sr = return [DataD [] nm [] cs derive_node_nms] -- [show_nm,eq_nm]
  where
    cs = [RecC nm [(pref_field_nm as fnm,IsStrict,mk_type (ftType fty)) |
                                                (fnm,fty)<-srFields sr]]

    nm = rep_type_nm as


{-

-- a sample union type definition, wrapper and test function
-- we are trying to generate something like this

data Foo = Bar Int | Baz Bool
    deriving (Show,Typeable)

instance ToJSON Foo where
    toJSON (Bar x) = object [ "x" .= x ]
    toJSON (Baz y) = object [ "y" .= y ]

instance FromJSONWithErrs Foo where
    parseJSONWithErrs (Object v) = alternatives (failWith $ MissingAlt ["x", "y"])
        [ Bar <$> v .:: "x"
        , Baz <$> v .:: "y"
        ]
    parseJSONWithErrs val        = failWith $ expectedObject val
-}

gen_su_dt :: APINode -> SpecUnion -> Q [Dec]
gen_su_dt as su = return [DataD [] nm [] cs derive_node_nms] -- [show_nm,eq_nm]
  where
    cs = [NormalC (pref_con_nm as fnm) [(IsStrict,mk_type ty)] |
                                            (fnm,(ty,_))<-suFields su]

    nm = rep_type_nm as


{-

-- a sample enum type definition, wrapper and test function
-- we are trying to generate something like this

data FrameRate
    = FRauto
    | FR10
    | FR15
    | FR23_97
    | FR24
    | FR25
    | FR29_97
    | FR30
    | FR60
    deriving (Show,Eq,Ord,Bounded,Enum,Typeable)

_text_FrameRate :: FrameRate -> T.Text
_text_FrameRate fr =
        case fr of
          FRauto    -> "auto"
          FR10      -> "10"
          FR15      -> "15"
          FR23_97   -> "23.97"
          FR24      -> "24"
          FR25      -> "25"
          FR29_97   -> "29.97"
          FR30      -> "30"
          FR60      -> "60"

_map_FrameRate :: Map.Map T.Text FrameRate
_map_FrameRate = text_map _text_FrameRate

instance ToJSON FrameRate where
    toJSON    = String . _text_FrameRate

instance FromJSONWithErrs FrameRate where
    parseJSONWithErrs = jsonStrMap_p _map_FrameRate

-}

gen_se_dt :: APINode -> SpecEnum -> Q [Dec]

gen_se_dt as se = return [DataD [] nm [] cs $
                                derive_leaf_nms ++ [''Bounded,''Enum]] -- [show_nm,eq_nm,ord_nm,bounded_nm,enum_nm]
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


type_nm, rep_type_nm, newtype_prj_nm :: APINode -> Name
type_nm        an = mkName $              _TypeName $ anName an
rep_type_nm    an = mkName $        rep_type_s an
newtype_prj_nm an = mkName $ "_" ++ rep_type_s an


pref_field_nm :: APINode -> FieldName -> Name
pref_field_nm as fnm = mkName $ pre ++ _FieldName fnm
  where
    pre = "_" ++ map toLower (CI.original $ anPrefix as) ++ "_"

rep_type_s :: APINode -> String
rep_type_s an = f $ _TypeName $ anName an
  where
    f s = maybe s (const ("REP__"++s)) $ anConvert an

pref_con_nm :: APINode -> FieldName -> Name
pref_con_nm as fnm = mkName $ pre ++ _FieldName fnm
  where
    pre = map toUpper (CI.original $ anPrefix as) ++ "_"
