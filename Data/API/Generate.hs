{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Data.API.Generate
    ( api
    , Binary(..)
    , generate
    , generateTools
    , generateSamples
    , Parser
    , Data.Map.Map
    , Data.Text.Text
    , Data.Typeable.Typeable
    , Test.QuickCheck.Gen
    , Test.QuickCheck.Arbitrary
    , Test.QuickCheck.arbitrary
    , Test.QuickCheck.oneof
    , Test.QuickCheck.elements
    , UTCTime
    , IsString
    , typeMismatch
    , ToJSON(..)
    , FromJSON(..)
    , Value(..)
    , Example(..)
    , object
    , (.:)
    , (.=)
    , mzero
    , (<*>)
    , mkInt
    , mkUTC
    , withText
    , withBool
    , withInt
    , withUTC
    , alternatives
    , genTextMap
    , jsonStrMap_p
    , mkBinary
    , withBinary
    , binary
    , ex_str_lit
    , ex_bin_lit
    , ex_boo_lit
    , ex_int_lit
    , ex_utc_lit
    ) where

import           Data.API.Types
import           Data.API.Parse
import qualified Data.Text
import qualified Data.Map
import qualified Data.Typeable
import qualified Test.QuickCheck
import           Control.Monad
import           Control.Applicative
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Quote
import           Data.Char
import           Data.String
import qualified Data.Text                      as T
import qualified Data.Map                       as Map
import qualified Data.CaseInsensitive           as CI
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Attoparsec.Number
import           Data.SafeCopy
import qualified Data.ByteString.Base64         as B64
import qualified Data.ByteString.Char8          as BC
import           Data.Time
import qualified Test.QuickCheck                as QC
import qualified Control.Lens                   as L
import           Safe


generate :: API -> Q [Dec]
generate api' = concat <$> mapM gen [an | ThNode an <- api']

generateTools :: API -> Q [Dec]
generateTools api' = concat <$> mapM gen_tools [an | ThNode an <- api']

generateSamples :: API -> String -> Q [Dec]
generateSamples api' nm_s = return 
    [ SigD nm $ AppT ListT $ AppT (AppT (TupleT 2) (ConT string_nm))
                                        (AppT (ConT gen_nm) (ConT value_nm))  
    , FunD nm $ [
        Clause 
            [] 
            (NormalB $ ListE [ gen_sample nd | ThNode nd <- api' ])
            []
        ]
    ]
  where
    nm = mkName nm_s
    
    gen_sample :: APINode -> Exp
    gen_sample an =
        TupE [ LitE (StringL tnm_s)
             , ap2 (VarE fmap_nm) (VarE to_json_nm) $
                    SigE (VarE arbitrary_fn_nm) (AppT (ConT gen_nm) (ConT tnm))
             ]
      where
        tnm   = mkName tnm_s
        tnm_s = _TypeName $ anName an

api :: QuasiQuoter
api =
    QuasiQuoter 
        { quoteExp  = \s -> [| parseAPI s |]
        , quotePat  = error "api QuasiQuoter used in patten      context"
        , quoteType = error "api QuasiQuoter used in type        context"
        , quoteDec  = error "api QuasiQuoter used in declaration context"
        }

instance QC.Arbitrary UTCTime where
    arbitrary = QC.elements
        [ mk "2010-01-01T00:00:00Z"
        , mk "2013-05-27T19:13:50Z"
        , mk "2011-07-20T22:04:00Z"
        , mk "2012-02-02T15:45:11Z"
        , mk "2009-11-12T20:57:54Z"
        , mk "2000-10-28T21:03:24Z"
        , mk "1965-03-10T09:23:01Z"
        ]
      where
        mk  = fromJustNote lab . parseUTC'
        
        lab = "Data.API.Generate.Arbitrary-UTCTime"


gen :: APINode -> Q [Dec]
gen an =
    case anSpec an of
      SpNewtype sn -> gen_sn an sn
      SpRecord  sr -> gen_sr an sr
      SpUnion   su -> gen_su an su
      SpEnum    se -> gen_se an se
      SpSynonym ty -> gen_sy an ty

gen_tools :: APINode -> Q [Dec]
gen_tools an | is_dt     = fmap concat $ sequence
                                [ L.makeLenses        nm
                                , deriveSafeCopy n be nm
                                ]
             | otherwise = return []
  where
    (n,be) =
        case anVersion an of
          Vrn i | i==0      -> (fromIntegral i,'base     )
                | otherwise -> (fromIntegral i,'extension)

    is_dt  =
        case anSpec an of 
          SpNewtype _ -> True
          SpRecord  _ -> True
          SpUnion   _ -> True
          SpEnum    _ -> True
          SpSynonym _ -> False

    nm     = rep_type_nm an

gen_sn :: APINode -> SpecNewtype -> Q [Dec]
gen_sn as sn = sequence $
    [ gen_sn_dt as sn
    , gen_sn_to as sn
    , gen_sn_fm as sn
    ] ++
    [ gen_in ij as    | Just (ij,_) <- [anConvert as]
    ] ++
    [ gen_pr pj as    | Just (_,pj) <- [anConvert as]
    ] ++
    [ gen_sn_ab as sn
    , gen_sn_ex as sn
    ]

gen_sr :: APINode -> SpecRecord -> Q [Dec] 
gen_sr as sr = sequence $
    [ gen_sr_dt as sr
    , gen_sr_to as sr
    , gen_sr_fm as sr
    ] ++
    [ gen_in ij as    | Just (ij,_) <- [anConvert as]
    ] ++
    [ gen_pr pj as    | Just (_,pj) <- [anConvert as]
    ] ++
    [ gen_sr_ab as sr
    , gen_sr_ex as sr
    ]

gen_su :: APINode -> SpecUnion -> Q [Dec] 
gen_su as su = sequence $
    [ gen_su_dt as su
    , gen_su_to as su
    , gen_su_fm as su
    ] ++
    [ gen_in ij as    | Just (ij,_) <- [anConvert as]
    ] ++
    [ gen_pr pj as    | Just (_,pj) <- [anConvert as]
    ] ++
    [ gen_su_ab as su
    , gen_su_ex as su   
    ]

gen_se :: APINode -> SpecEnum -> Q [Dec] 
gen_se as se = sequence $
    [ gen_se_dt     as se
    , gen_se_to     as se 
    , gen_se_fm     as se
    , gen_se_tx_sig as se
    , gen_se_tx     as se
    , gen_se_mp_sig as se
    , gen_se_mp     as se
    , gen_se_ex     as se
    ] ++
    [ gen_in ij as    | Just (ij,_) <- [anConvert as]
    ] ++
    [ gen_pr pj as    | Just (_,pj) <- [anConvert as]
    ] ++
    [ gen_se_ab as se
    ]

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

instance FromJSON JobId where
    parseJSON = withText "JobId" (return . JobId)
-}

gen_sn_dt, gen_sn_to, gen_sn_fm, gen_sn_ab, gen_sn_ex
                                         :: APINode -> SpecNewtype -> Q Dec

gen_sn_dt as sn = return $ NewtypeD [] nm [] c $ derive_leaf_nms ++ iss
  where
    c   = RecC nm [(newtype_prj_nm as,NotStrict,mk_type $ TyBasic $ snType sn)]
    
    nm  = rep_type_nm as

    iss = case snType sn of
            BTstring _ -> [is_string_cl_nm]
            BTbinary _ -> []
            BTbool   _ -> []
            BTint    _ -> []
            BTutc    _ -> []

gen_sn_to as sn = return $ InstanceD [] typ [FunD to_json_nm [Clause [] bdy []]]
  where
    typ = AppT (ConT to_json_cl_nm) $ ConT $ rep_type_nm as

    bdy = NormalB $ ap2 (VarE dot_nm) ine $ VarE $ newtype_prj_nm as

    ine = case snType sn of
            BTstring _ -> ConE string_con_nm
            BTbinary _ -> VarE mk_binary_nm
            BTbool   _ -> ConE bool_con_nm
            BTint    _ -> VarE mk_int_nm
            BTutc    _ -> VarE mk_utc_nm

gen_sn_fm as sn = return $ InstanceD [] typ [FunD parse_json_nm [cl]]
  where
    typ = AppT (ConT from_json_cl_nm) $ ConT $ rep_type_nm as

    cl  = Clause [] bdy []

    bdy = NormalB $ AppE 
                (AppE (VarE wnm) 
                            (LitE $ StringL $ _TypeName $ anName as)) $
                    AppE (AppE (VarE dot_nm) (VarE return_nm)) $ ConE tn

    tn  = rep_type_nm as
    
    wnm = case snType sn of
            BTstring _ -> with_text_nm
            BTbinary _ -> with_binary_nm
            BTbool   _ -> with_bool_nm
            BTint    _ -> with_int_nm
            BTutc    _ -> with_utc_nm

gen_sn_ab as _sn = return $ InstanceD [] typ [FunD arbitrary_nm [cl]]
  where
    typ = AppT (ConT arbitrary_cl_nm) $ ConT $ rep_type_nm as

    cl  = Clause [] bdy []

    bdy = NormalB $ ap2 (VarE fmap_nm) (ConE tn) $ VarE arbitrary_fn_nm

    tn  = rep_type_nm as

gen_sn_ex as sn = return $ InstanceD [] typ [FunD example_nm [cl]]
  where
    typ   = AppT (ConT example_cl_nm) $ ConT tn

    cl    = Clause [] bdy []

    bdy   = NormalB $ AppE (ConE $ rep_type_nm as) $ 
                                            example_basic_type $ snType sn

    tn    = rep_type_nm as

example_basic_type :: BasicType -> Exp
example_basic_type bt =
    case bt of
      BTstring mb -> maybe ex (f prep_ex_str ex_str_lit_nm) mb
      BTbinary mb -> maybe ex (f prep_ex_bin ex_bin_lit_nm) mb
      BTbool   mb -> maybe ex (f prep_ex_boo ex_boo_lit_nm) mb
      BTint    mb -> maybe ex (f prep_ex_int ex_int_lit_nm) mb
      BTutc    mb -> maybe ex (f prep_ex_utc ex_utc_lit_nm) mb
  where
    f pf lf_nm x = AppE (VarE lf_nm) (pf x)

    ex           = VarE example_fn_nm


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

instance FromJSON JobSpecId where
     parseJSON (Object v) = 
        JobSpecId <$>
            v .: "Id"                               <*>
            v .: "Input"                            <*>
            v .: "Output"                           <*>
            v .: "PipelineId"
     parseJSON _          = typeMismatch "JobSpecId" val

instance ToJSON JobSpecId where
     toJSON jsi@(JobSpecId _ _ _ _) =
        object
            [ "Id"         .= jsiId         jsi
            , "Input"      .= jsiInput      jsi
            , "Output"     .= jsiOutput     jsi
            , "PipelineId" .= jsiPipelineId jsi
            ]
-}

gen_sr_dt, gen_sr_to, gen_sr_fm, gen_sr_ab, gen_sr_ex :: 
                                            APINode -> SpecRecord -> Q Dec

gen_sr_dt as sr = return $ DataD [] nm [] cs derive_node_nms -- [show_nm,eq_nm]
  where
    cs = [RecC nm [(pref_field_nm as fnm,IsStrict,mk_type ty) | 
                                                (fnm,(ty,_))<-srFields sr]]

    nm = rep_type_nm as

gen_sr_to as sr = return $ InstanceD [] typ [fd]
  where
    typ = AppT (ConT to_json_cl_nm) $ ConT $ rep_type_nm as

    fd  = FunD to_json_nm [cl]

    cl  = Clause [VarP x_nm] bdy []

    bdy = NormalB $ AppE (VarE object_nm) $
            ListE [ AppE (AppE (VarE dot_eq_nm) 
                         (LitE $ StringL $ _FieldName fn)) $
                                AppE (VarE $ pre fn) (VarE x_nm) | fn<-fns ]  
  
    pre = pref_field_nm as
  
    fns = map fst $ srFields sr

gen_sr_fm as sr = return $ InstanceD [] typ [FunD parse_json_nm [cl,cl']]
  where
    typ = AppT (ConT from_json_cl_nm) $ ConT $ rep_type_nm as

    cl  = Clause [ConP object_con_nm [VarP x_nm]] bdy []

    cl' = Clause [WildP] (NormalB $ VarE mzero_nm) []

    bdy = NormalB $
            app (VarE fmap_nm) (VarE astar_nm) (ConE $ rep_type_nm as)
                [ AppE (AppE (VarE dot_co_nm) (VarE x_nm)) 
                        (LitE $ StringL $ _FieldName fn) | fn<-fns ]
                 
    fns = map fst $ srFields sr

gen_sr_ab as sr = return $ InstanceD [] typ [FunD arbitrary_nm [cl]]
  where
    typ   = AppT (ConT arbitrary_cl_nm) $ ConT tn

    cl    = Clause [] bdy []

    bdy   = NormalB $ app (VarE fmap_nm) (VarE astar_nm) (ConE tn) $
                replicate (length $ srFields sr) $ VarE arbitrary_fn_nm 

    tn    = rep_type_nm as

gen_sr_ex as sr = return $ InstanceD [] typ [FunD example_nm [cl]]
  where
    typ   = AppT (ConT example_cl_nm) $ ConT tn

    cl    = Clause [] bdy []

    bdy   = NormalB $ ap_l (ConE tn) $
                        replicate (length $ srFields sr) $ VarE example_fn_nm 

    tn    = rep_type_nm as


{-

-- a sample union type definition, wrapper and test function
-- we are trying to generate something like this

data Foo = Bar Int | Baz Bool
    deriving (Show,Typeable)

instance ToJSON Foo where
    toJSON (Bar x) = object [ "x" .= x ]
    toJSON (Baz y) = object [ "y" .= y ]

instance FromJSON Foo where
    parseJSON (Object v) = alternatives 
        [ Bar <$> v .: "x"
        , Baz <$> v .: "y"
        ]
    parseJSON val = typeMismatch "Foo" val 
-}

gen_su_dt, gen_su_to, gen_su_fm, gen_su_ab, gen_su_ex
                                             :: APINode -> SpecUnion -> Q Dec

gen_su_dt as su = return $ DataD [] nm [] cs derive_node_nms -- [show_nm,eq_nm]
  where
    cs = [NormalC (pref_con_nm as fnm) [(IsStrict,mk_type ty)] | 
                                            (fnm,(ty,_))<-suFields su]
    
    nm = rep_type_nm as

gen_su_to as su = return $ InstanceD [] typ [fd]
  where
    typ    = AppT (ConT to_json_cl_nm) $ ConT $ rep_type_nm as

    fd     = FunD to_json_nm $ map cl fns

    cl  fn = Clause [ConP (pre fn) [VarP x_nm]] (bdy fn) []

    bdy fn = NormalB $ AppE (VarE object_nm) $
                ListE [ AppE (AppE (VarE dot_eq_nm)
                         (LitE $ StringL $ _FieldName fn)) (VarE x_nm) ]  

    pre    = pref_con_nm as
  
    fns    = map fst $ suFields su

gen_su_fm as su = return $ InstanceD [] typ [FunD parse_json_nm [cl,cl']]
  where
    typ = AppT (ConT from_json_cl_nm) $ ConT $ rep_type_nm as

    cl  = Clause [ConP object_con_nm [VarP x_nm]] bdy []

    cl' = Clause [VarP x_nm] (NormalB $ oops as $ VarE x_nm) []

    bdy = NormalB $
            AppE (VarE alts_nm) $ ListE
                [ AppE (AppE (VarE fmap_nm) (ConE $ pref_con_nm as fn)) $
                        AppE (AppE (VarE dot_co_nm) (VarE x_nm)) $
                             LitE $ StringL $ _FieldName fn | fn<-fns ]
                 
    fns = map fst $ suFields su

    oops as_ e = AppE (AppE (VarE mismatch_nm)
                                (LitE $ StringL $ _TypeName $ anName as_)) e

gen_su_ab as su = return $ InstanceD [] typ [FunD arbitrary_nm [cl]]
  where
    typ = AppT (ConT arbitrary_cl_nm) $ ConT tn

    cl  = Clause [] bdy []

    bdy = NormalB $ if null ks then emp else prp
    
    emp = ConE tn
    
    prp = AppE (VarE oneof_nm) $
                ListE [ ap2 (VarE fmap_nm) (ConE k) 
                                            (VarE arbitrary_fn_nm) | k<- ks ]

    tn  = rep_type_nm as
    ks  = map (pref_con_nm as) $ map fst $ suFields su

gen_su_ex as su = return $ InstanceD [] typ [FunD example_nm [cl]]
  where
    typ   = AppT (ConT example_cl_nm) $ ConT tn

    cl    = Clause [] bdy []

    bdy   = NormalB $ AppE (ConE cn) $ VarE example_fn_nm 

    cn    = headNote dg ks
    tn    = rep_type_nm as
    
    ks  = map (pref_con_nm as) $ map fst $ suFields su

    dg    = "Data.API.Generate.gen_su_ex"
              


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

instance FromJSON FrameRate where
    parseJSON = jsonStrMap_p _map_FrameRate

-}

gen_se_dt, gen_se_to, gen_se_fm, gen_se_ab,
                gen_se_tx_sig, gen_se_tx, 
                gen_se_mp_sig, gen_se_mp,
                gen_se_ex :: APINode -> SpecEnum -> Q Dec

gen_se_dt as se = return $ DataD [] nm [] cs $
                                derive_leaf_nms ++ [bounded_nm,enum_nm] -- [show_nm,eq_nm,ord_nm,bounded_nm,enum_nm]
  where
    cs = [NormalC (pref_con_nm as fnm) [] | (fnm,_) <- seAlts se ]
    
    nm = rep_type_nm as

gen_se_to as _se = return $ InstanceD [] typ [FunD to_json_nm [cl]]
  where
    typ = AppT (ConT to_json_cl_nm) $ ConT $ rep_type_nm as

    cl  = Clause [] bdy []

    bdy = NormalB $ AppE (AppE (VarE dot_nm) (ConE string_con_nm)) $ 
                VarE $ txt_nm as

gen_se_fm as _se = return $ InstanceD [] typ [FunD parse_json_nm [cl]]
  where
    typ = AppT (ConT from_json_cl_nm) $ ConT $ rep_type_nm as

    cl  = Clause [] bdy []

    bdy = NormalB $ AppE (VarE json_str_map_id) (VarE $ map_nm as)

gen_se_tx_sig as _se = return $
        SigD (txt_nm as) $ AppT (AppT ArrowT $ ConT tnm) $ ConT text_nm
  where
    tnm = rep_type_nm as

gen_se_tx as se = return $ FunD (txt_nm as) [Clause [VarP x_nm] bdy []]
  where
    bdy    = NormalB $ 
                CaseE (VarE x_nm) [ Match (pt fnm) (bd fnm) [] | fnm<-fnms ]  

    fnms   = map fst $ seAlts se

    pt fnm = ConP (pref_con_nm as fnm) []

    bd fnm = NormalB $ LitE $ StringL $ _FieldName fnm

gen_se_mp_sig as _se = return $
        SigD (map_nm as) $ 
            AppT (AppT (ConT map_ty_nm) $ ConT text_nm) $ ConT tnm
  where
    tnm = rep_type_nm as

gen_se_mp as _se = return $ FunD (map_nm as) [Clause [] bdy []]
  where
    bdy    = NormalB $ AppE (VarE gen_text_map_id) (VarE $ txt_nm as)

gen_se_ab as se = return $ InstanceD [] typ [FunD arbitrary_nm [cl]]
  where
    typ = AppT (ConT arbitrary_cl_nm) $ ConT tn

    cl  = Clause [] bdy []

    bdy = NormalB $ if null ks then emp else prp
    
    emp = ConE tn
    
    prp = AppE (VarE elements_nm) $ ListE [ ConE k | k<- ks ]

    tn  = rep_type_nm as
    
    ks  = map (pref_con_nm as . fst) $ seAlts se

gen_se_ex as se = return $ InstanceD [] typ [FunD example_nm [cl]]
  where
    typ   = AppT (ConT example_cl_nm) $ ConT tn

    cl    = Clause [] bdy []

    bdy   = NormalB $ ConE cn 

    cn    = headNote dg ks
    tn    = rep_type_nm as
    
    ks    = map (pref_con_nm as . fst) $ seAlts se

    dg    = "Data.API.Generate.gen_se_ex"
              

gen_in, gen_pr :: FieldName -> APINode -> Q Dec

gen_in inj_fn an = return $ InstanceD [] typ [fd]
  where
    typ    = AppT (ConT from_json_cl_nm) $ ConT $ type_nm an

    fd     = FunD parse_json_nm [cl]

    cl     = Clause [VarP x_nm] bdy []

    bdy    = NormalB $ ap2 (VarE then_nm) 
                           (AppE (VarE parse_json_fn_nm) (VarE x_nm))
                           (VarE inj_nm)

    inj_nm = mkName $ _FieldName inj_fn

gen_pr prj_fn an = return $ InstanceD [] typ [fd]
  where
    typ    = AppT (ConT to_json_cl_nm) $ ConT $ type_nm an

    fd     = FunD to_json_nm [cl]

    cl     = Clause [] bdy []

    bdy    = NormalB $ ap2 (VarE dot_nm) (VarE to_json_fn_nm) (VarE prj_nm)


    prj_nm = mkName $ _FieldName prj_fn


type_nm, txt_nm, map_nm, rep_type_nm, newtype_prj_nm :: APINode -> Name
type_nm        an = mkName $              _TypeName $ anName an
txt_nm         an = mkName $ "_text_" ++ (_TypeName $ anName an)
map_nm         an = mkName $ "_map_"  ++ (_TypeName $ anName an)
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

mk_type :: APIType -> Type
mk_type ty =
    case ty of
      TyList  ty'  -> AppT ListT  $ mk_type ty'
      TyMaybe ty'  -> AppT (ConT maybe_nm) $ mk_type ty'
      TyName  nm _ -> ConT  $ mkName $ _TypeName nm
      TyBasic bt   -> basic_type bt
      TyJSON       -> ConT value_nm

basic_type :: BasicType -> Type
basic_type bt =
    case bt of
      BTstring _ -> ConT   text_nm
      BTbinary _ -> ConT $ binary_nm
      BTbool   _ -> ConT $ mkName "Bool"
      BTint    _ -> ConT $ mkName "Int"
      BTutc    _ -> ConT $ utc_time_nm


derive_leaf_nms :: [Name]
derive_leaf_nms = [show_nm,eq_nm,ord_nm,typeable_cl_nm]

derive_node_nms :: [Name]
derive_node_nms = [show_nm,eq_nm,typeable_cl_nm]

x_nm, string_nm, maybe_nm, eq_nm, ord_nm, show_nm, ord_nm, bounded_nm, enum_nm,
    fmap_nm, to_json_nm, parse_json_nm, return_nm, 
    arbitrary_nm, dot_nm, then_nm,
    true_nm, false_nm,
    utc_time_nm, map_ty_nm, text_nm, is_string_cl_nm, typeable_cl_nm, 
    to_json_cl_nm, from_json_cl_nm, to_json_fn_nm, parse_json_fn_nm,
    value_nm, gen_nm,
    arbitrary_cl_nm,
    object_nm, object_con_nm, string_con_nm, bool_con_nm,
    dot_eq_nm, dot_co_nm, mzero_nm, astar_nm, mismatch_nm, 
    with_text_nm, with_bool_nm, with_int_nm, with_utc_nm, mk_int_nm, mk_utc_nm,
    alts_nm, arbitrary_fn_nm, oneof_nm, elements_nm,
    gen_text_map_id, json_str_map_id,
    binary_nm, mk_binary_nm, with_binary_nm :: Name

x_nm                = mkName "x"
string_nm           = mkName "String"
maybe_nm            = mkName "Maybe"
show_nm             = mkName "Show"
eq_nm               = mkName "Eq"
ord_nm              = mkName "Ord"
bounded_nm          = mkName "Bounded"
enum_nm             = mkName "Enum"
fmap_nm             = mkName "fmap"
to_json_nm          = mkName "toJSON"
parse_json_nm       = mkName "parseJSON"
return_nm           = mkName "return"
arbitrary_nm        = mkName "arbitrary"
dot_nm              = mkName "."
then_nm             = mkName ">>="
true_nm             = mkName "True"
false_nm            = mkName "False"

utc_time_nm         = Name (mkOccName "UTCTime"     ) $ NameQ gn_mod
map_ty_nm           = Name (mkOccName "Map"         ) $ NameQ gn_mod
text_nm             = Name (mkOccName "Text"        ) $ NameQ gn_mod
is_string_cl_nm     = Name (mkOccName "IsString"    ) $ NameQ gn_mod
typeable_cl_nm      = Name (mkOccName "Typeable"    ) $ NameQ gn_mod
to_json_cl_nm       = Name (mkOccName "ToJSON"      ) $ NameQ gn_mod
from_json_cl_nm     = Name (mkOccName "FromJSON"    ) $ NameQ gn_mod
to_json_fn_nm       = Name (mkOccName "toJSON"      ) $ NameQ gn_mod
parse_json_fn_nm    = Name (mkOccName "parseJSON"   ) $ NameQ gn_mod
value_nm            = Name (mkOccName "Value"       ) $ NameQ gn_mod
gen_nm              = Name (mkOccName "Gen"         ) $ NameQ gn_mod
arbitrary_cl_nm     = Name (mkOccName "Arbitrary"   ) $ NameQ gn_mod
object_nm           = Name (mkOccName "object"      ) $ NameQ gn_mod
object_con_nm       = Name (mkOccName "Object"      ) $ NameQ gn_mod
string_con_nm       = Name (mkOccName "String"      ) $ NameQ gn_mod
bool_con_nm         = Name (mkOccName "Bool"        ) $ NameQ gn_mod
dot_eq_nm           = Name (mkOccName ".="          ) $ NameQ gn_mod
dot_co_nm           = Name (mkOccName ".:"          ) $ NameQ gn_mod
mzero_nm            = Name (mkOccName "mzero"       ) $ NameQ gn_mod
astar_nm            = Name (mkOccName "<*>"         ) $ NameQ gn_mod
mismatch_nm         = Name (mkOccName "typeMismatch") $ NameQ gn_mod
with_text_nm        = Name (mkOccName "withText"    ) $ NameQ gn_mod
with_bool_nm        = Name (mkOccName "withBool"    ) $ NameQ gn_mod
mk_int_nm           = Name (mkOccName "mkInt"       ) $ NameQ gn_mod
mk_utc_nm           = Name (mkOccName "mkUTC"       ) $ NameQ gn_mod
with_int_nm         = Name (mkOccName "withInt"     ) $ NameQ gn_mod
with_utc_nm         = Name (mkOccName "withUTC"     ) $ NameQ gn_mod
alts_nm             = Name (mkOccName "alternatives") $ NameQ gn_mod
arbitrary_fn_nm     = Name (mkOccName "arbitrary"   ) $ NameQ gn_mod
oneof_nm            = Name (mkOccName "oneof"       ) $ NameQ gn_mod
elements_nm         = Name (mkOccName "elements"    ) $ NameQ gn_mod
gen_text_map_id     = Name (mkOccName "genTextMap"  ) $ NameQ gn_mod
json_str_map_id     = Name (mkOccName "jsonStrMap_p") $ NameQ gn_mod
mk_binary_nm        = Name (mkOccName "mkBinary"    ) $ NameQ gn_mod
with_binary_nm      = Name (mkOccName "withBinary"  ) $ NameQ gn_mod
binary_nm           = Name (mkOccName "Binary"      ) $ NameQ gn_mod


example_nm, example_cl_nm, example_fn_nm,
    ex_str_lit_nm,
    ex_bin_lit_nm,
    ex_boo_lit_nm,
    ex_int_lit_nm,
    ex_utc_lit_nm   :: Name

example_nm    = mkName "example"
example_cl_nm = Name (mkOccName "Example"           ) $ NameQ gn_mod
example_fn_nm = Name (mkOccName "example"           ) $ NameQ gn_mod
ex_str_lit_nm = Name (mkOccName "ex_str_lit"        ) $ NameQ gn_mod
ex_bin_lit_nm = Name (mkOccName "ex_bin_lit"        ) $ NameQ gn_mod
ex_boo_lit_nm = Name (mkOccName "ex_boo_lit"        ) $ NameQ gn_mod
ex_int_lit_nm = Name (mkOccName "ex_int_lit"        ) $ NameQ gn_mod
ex_utc_lit_nm = Name (mkOccName "ex_utc_lit"        ) $ NameQ gn_mod

prep_ex_str :: T.Text   -> Exp
prep_ex_str = LitE . StringL . T.unpack
 
ex_str_lit :: String -> T.Text
ex_str_lit = T.pack

prep_ex_bin :: Binary   -> Exp
prep_ex_bin (Binary bs) = LitE $ StringL $ BC.unpack $ B64.encode bs

ex_bin_lit :: String -> Binary
ex_bin_lit = Binary . B64.decodeLenient . BC.pack

prep_ex_boo :: Bool     -> Exp
prep_ex_boo b = ConE $ if b then true_nm else false_nm 

ex_boo_lit :: Bool -> Bool
ex_boo_lit = id

prep_ex_int :: Int      -> Exp
prep_ex_int = LitE . IntegerL . toInteger 

ex_int_lit :: Int -> Int
ex_int_lit = id

prep_ex_utc :: UTCTime  -> Exp
prep_ex_utc = prep_ex_str . mkUTC'

ex_utc_lit :: String -> UTCTime
ex_utc_lit = maybe oops id . parseUTC_
  where
    oops = error "Data.API.generate.ex_utc_lit: failed to parse UTC literal"

gn_mod :: ModName
gn_mod = mkModName "Data.API.Generate"

mkInt :: Int -> Value
mkInt = Number . I . toInteger

withInt :: String -> (Int->Parser a) -> Value -> Parser a
withInt lab f = withNumber lab g
  where
    g (I i) = f $ fromInteger i
    g val   = typeMismatch lab $ Number val

alternatives :: [Parser a] -> Parser a
alternatives = foldr (<|>) empty 

genTextMap :: (Ord a,Bounded a,Enum a) => (a->T.Text) -> Map.Map T.Text a
genTextMap f = Map.fromList [ (f x,x) | x<-[minBound..maxBound] ]

jsonStrMap_p :: Ord a => Map.Map T.Text a -> Value -> Parser a
jsonStrMap_p mp = json_string_p $ flip Map.lookup mp 

json_string_p :: Ord a => (T.Text->Maybe a) -> Value -> Parser a
json_string_p p (String t) | Just val <- p t = return val
                           | otherwise       = mzero
json_string_p _  _                           = mzero


-- app <$> <*> ke []          => ke
-- app <$> <*> ke [e1,e2,...,en] =>
--      ... ((ke <$> e1) <*> e2) ... <*> en

app :: Exp -> Exp -> Exp -> [Exp] -> Exp
app fd fs ke es0 =
    case es0 of
      []   -> ke
      e:es -> app' (ke `dl` e) es
  where
    app' e []      = e
    app' e (e':es) = app' (e `st` e') es

    st e1 e2 = AppE (AppE fs e1) e2 
    dl e1 e2 = AppE (AppE fd e1) e2 


ap2 :: Exp -> Exp -> Exp -> Exp
ap2 f e e' = AppE (AppE f e) e'

ap_l :: Exp -> [Exp] -> Exp
ap_l f []     = f
ap_l f (e:es) = ap_l (AppE f e) es

$(L.makeLenses           ''Binary)
$(deriveSafeCopy 0 'base ''Binary)
