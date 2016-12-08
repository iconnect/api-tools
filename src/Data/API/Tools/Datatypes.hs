{-# LANGUAGE TemplateHaskell            #-}
module Data.API.Tools.Datatypes
    ( datatypesTool
    , datatypesTool'
    , defaultDerivedClasses
    , type_nm
    , rep_type_nm
    , nodeT
    , nodeRepT
    , nodeConE
    , nodeNewtypeConE
    , nodeFieldE
    , nodeFieldP
    , nodeAltConE
    , nodeAltConP
    , newtypeProjectionE
    ) where

import           Data.API.TH
import           Data.API.TH.Compat
import           Data.API.Tools.Combinators
import           Data.API.Types

import           Control.Applicative
import           Data.Aeson
import qualified Data.CaseInsensitive           as CI
import           Data.Char
import           Data.Maybe
import           Data.String
import qualified Data.Text                      as T
import           Data.Time
import           Data.Typeable
import           Language.Haskell.TH
import           Text.Regex
import           Prelude


-- | Tool to generate datatypes and type synonyms corresponding to an API
datatypesTool :: APITool
datatypesTool = datatypesTool' defaultDerivedClasses

-- | Tool to generate datatypes and type synonyms corresponding to an
-- API, where the function specifies the derived classes for each datatype.
datatypesTool' :: (APINode -> [Name]) -> APITool
datatypesTool' deriv = apiNodeTool $ apiSpecTool (mkTool     (gen_sn_dt deriv))
                                                 (simpleTool (gen_sr_dt deriv))
                                                 (simpleTool (gen_su_dt deriv))
                                                 (simpleTool (gen_se_dt deriv))
                                                 (simpleTool gen_sy)


-- | Generate a type synonym definition
gen_sy :: (APINode, APIType) -> Q [Dec]
gen_sy (as, ty) = return [TySynD (type_nm as) [] $ mk_type ty]


-- | Generate a newtype definition, like this:
--
-- > newtype JobId = JobId { _JobId :: T.Text }
-- >     deriving (Show,IsString,Eq,Typeable)
--
-- If a filter has been applied, and smart constructors are enabled,
-- instead generate this:
--
-- > newtype EmailAddress = UnsafeMkEmailAddress { _EmailAddress :: T.Text }
-- >     deriving (Show,Eq,Typeable)
-- > mkEmailAddress :: T.Text -> Maybe EmailAddress
-- > mkEmailAddress t = ... -- check filter

gen_sn_dt :: (APINode -> [Name]) -> ToolSettings -> (APINode, SpecNewtype) -> Q [Dec]
gen_sn_dt deriv ts (as, sn) = (nd :) <$> if smart then sc else return []
  where
    nd  = mkNewtypeD [] nm [] c (deriv as)
    c   = RecC (newtype_con_nm smart as) [(newtype_prj_nm as,annNotStrict,wrapped_ty)]
    wrapped_ty = mk_type $ TyBasic (snType sn)

    nm  = rep_type_nm as

    smart = newtypeSmartConstructors ts && isJust (snFilter sn)

    sc  = simpleSigD (newtype_smart_con_nm as) [t| $(return wrapped_ty) -> Maybe $(nodeRepT as) |] $
             case snFilter sn of
               Just (FtrStrg re) -> [| \ s -> if isJust (matchRegex (re_regex re) (T.unpack s))
                                                                   then Just ($nt_con s) else Nothing |]
               Just (FtrIntg ir) -> [| \ i -> if i `inIntRange` ir then Just ($nt_con i) else Nothing |]
               Just (FtrUTC  ur) -> [| \ u -> if u `inUTCRange` ur then Just ($nt_con u) else Nothing |]
               Nothing           -> [| Just . $nt_con |]

    nt_con = nodeNewtypeConE ts as sn



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

gen_sr_dt :: (APINode -> [Name]) -> (APINode, SpecRecord) -> Q [Dec]
gen_sr_dt deriv (as, sr) = return [mkDataD [] nm [] cs (deriv as)]
  where
    cs = [RecC nm [(pref_field_nm as fnm,annIsStrict,mk_type (ftType fty)) |
                                                (fnm,fty)<-srFields sr]]
    nm = rep_type_nm as


-- | Generate a union type definition, like this:
--
-- > data Foo = F_Bar Int | F_Baz Bool
-- >     deriving (Show,Typeable)

gen_su_dt :: (APINode -> [Name]) -> (APINode, SpecUnion) -> Q [Dec]
gen_su_dt deriv (as, su) = return [mkDataD [] nm [] cs (deriv as)]
  where
    cs = [NormalC (pref_con_nm as fnm) [(annIsStrict,mk_type ty)] |
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

gen_se_dt :: (APINode -> [Name]) -> (APINode, SpecEnum) -> Q [Dec]
gen_se_dt deriv (as, se) = return [mkDataD [] nm [] cs (deriv as)]
  where
    cs = [NormalC (pref_con_nm as fnm) [] | (fnm,_) <- seAlts se ]
    nm = rep_type_nm as


mk_type :: APIType -> Type
mk_type ty =
    case ty of
      TyList  ty'  -> AppT ListT  $ mk_type ty'
      TyMaybe ty'  -> AppT (ConT ''Maybe) $ mk_type ty'
      TyName  nm   -> ConT  $ mkNameText $ _TypeName nm
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


-- | Default names of classes for which to derive instances, depending
-- on the type of API node.
defaultDerivedClasses :: APINode -> [Name]
defaultDerivedClasses an = case anSpec an of
    SpNewtype sn -> case snType sn of
                      BTstring -> ''IsString : derive_leaf_nms
                      BTbinary -> derive_leaf_nms
                      BTbool   -> derive_leaf_nms
                      BTint    -> derive_leaf_nms
                      BTutc    -> derive_leaf_nms
    SpRecord  _  -> derive_node_nms
    SpUnion   _  -> derive_node_nms
    SpEnum    _  -> derive_leaf_nms ++ [''Bounded, ''Enum]
    SpSynonym _  -> []

derive_leaf_nms :: [Name]
derive_leaf_nms = [''Show,''Eq,''Ord,''Typeable]

derive_node_nms :: [Name]
derive_node_nms = [''Show,''Eq,''Typeable]


-- | Name of the type corresponding to the API node, e.g. @JobId@
type_nm :: APINode -> Name
type_nm an = mkName $ T.unpack $ _TypeName $ anName an

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

-- | Name of the constructor of a newtype, which will be same as the
-- representation type unless a smart constructor is requested, in
-- which case we just prefix it with "UnsafeMk".
newtype_con_nm :: Bool -> APINode -> Name
newtype_con_nm smart an | smart     = mkName $ "UnsafeMk" ++ rep_type_s an
                        | otherwise = mkName $               rep_type_s an

-- | Name of the smart constructor of a newtype, prefixed with "mk".
newtype_smart_con_nm :: APINode -> Name
newtype_smart_con_nm an = mkName $ "mk" ++ rep_type_s an

rep_type_s :: APINode -> String
rep_type_s an = f $ T.unpack $ _TypeName $ anName an
  where
    f s = maybe s (const ("REP__"++s)) $ anConvert an

-- | Construct the name of a record field by attaching the
-- type-specific prefix, in lowercase, e.g. @_jsi_id@
pref_field_nm :: APINode -> FieldName -> Name
pref_field_nm as fnm = mkName $ pre ++ T.unpack (_FieldName fnm)
  where
    pre = "_" ++ map toLower (CI.original $ anPrefix as) ++ "_"

-- | Construct the name of a union or enum constructor by attaching
-- the type-specific prefix, in uppercase, e.g. @FR_auto@
pref_con_nm :: APINode -> FieldName -> Name
pref_con_nm as fnm = mkName $ pre ++ T.unpack (_FieldName fnm)
  where
    pre = map toUpper (CI.original $ anPrefix as) ++ "_"


-- | The type corresponding to an API node
nodeT :: APINode -> TypeQ
nodeT = conT . type_nm

-- | The representation type corresponding to an API node
nodeRepT :: APINode -> TypeQ
nodeRepT = conT . rep_type_nm

-- | The constructor for a record API node
nodeConE :: APINode -> ExpQ
nodeConE = conE . rep_type_nm

-- | The constructor for a newtype, which might be renamed
nodeNewtypeConE :: ToolSettings -> APINode -> SpecNewtype -> ExpQ
nodeNewtypeConE ts an sn = conE $ newtype_con_nm (newtypeSmartConstructors ts && isJust (snFilter sn)) an

-- | A record field in an API node, as an expression
nodeFieldE :: APINode -> FieldName -> ExpQ
nodeFieldE an fnm = varE $ pref_field_nm an fnm

-- | A record field in an API node, as a pattern
nodeFieldP :: APINode -> FieldName -> PatQ
nodeFieldP an fnm = varP $ pref_field_nm an fnm

-- | A prefixed constructor for a union or enum, as an expression
nodeAltConE :: APINode -> FieldName -> ExpQ
nodeAltConE an fn = conE $ pref_con_nm an fn

-- | A prefixed constructor for a union or enum, as a pattern
nodeAltConP :: APINode -> FieldName -> [PatQ] -> PatQ
nodeAltConP an fn = conP (pref_con_nm an fn)

-- | The projection function from a newtype API node, as an epxression
newtypeProjectionE :: APINode -> ExpQ
newtypeProjectionE = varE . newtype_prj_nm
