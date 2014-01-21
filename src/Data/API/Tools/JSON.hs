{-# LANGUAGE TemplateHaskell            #-}

module Data.API.Tools.JSON
    ( jsonTool
    , toJsonNodeTool
    , fromJsonNodeTool
    ) where

import           Data.API.JSON
import           Data.API.TH
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Tools.Enum
import           Data.API.Types hiding (withUTC, withBinary)

import           Control.Applicative
import qualified Data.Map                       as Map
import qualified Data.Text                      as T
import           Data.Time
import           Language.Haskell.TH.Syntax


jsonTool :: APITool
jsonTool = apiNodeTool $ toJsonNodeTool `appendTool` fromJsonNodeTool


toJsonNodeTool :: APINodeTool
toJsonNodeTool = apiSpecTool gen_sn_to gen_sr_to gen_su_to gen_se_to (const emptyTool)
                 `appendTool` gen_pr

fromJsonNodeTool :: APINodeTool
fromJsonNodeTool = apiSpecTool gen_sn_fm gen_sr_fm gen_su_fm gen_se_fm (const emptyTool)
                   `appendTool` gen_in


gen_sn_to :: APINode -> SpecNewtype -> Q [Dec]
gen_sn_to as sn = mkInstanceIfNotExists ''ToJSON [ConT $ rep_type_nm as]
                      [FunD 'toJSON [Clause [] bdy []]]
  where
    bdy = NormalB $ VarE '(.) `AppE` ine `AppE` VarE (newtype_prj_nm as)

    ine = case snType sn of
            BTstring -> ConE 'String
            BTbinary -> VarE 'mkBinary
            BTbool   -> ConE 'Bool
            BTint    -> VarE 'mkInt
            BTutc    -> VarE 'mkUTC

gen_sn_fm :: APINode -> SpecNewtype -> Q [Dec]
gen_sn_fm as sn = mk_FromJSON_instances (rep_type_nm as) [Clause [] bdy []]
  where
    bdy   = NormalB $ AppE
                (AppE wth (LitE $ StringL $ _TypeName $ anName as)) $
                    AppE (AppE (VarE '(.)) (VarE 'pure)) $ ConE tn

    tn    = rep_type_nm as

    wth   =
        case snType sn of
            BTstring -> wth_s                 -- with_text_nm
            BTbinary -> VarE 'withBinary
            BTbool   -> VarE 'withBool
            BTint    -> wth_i                 -- with_int_nm
            BTutc    -> wth_u                 -- with_utc_nm

    wth_s =
        case snFilter sn of
          Just (FtrStrg re) -> AppE (VarE 'with_txt_re) (LitE $ StringL $ T.unpack $ re_text re)
          _                 -> VarE 'withText

    wth_i =
        case snFilter sn of
          Just (FtrIntg(IntRange Nothing   (Just hi))) -> AppE (VarE 'with_int_to)    (LitE $ IntegerL $ toInteger hi)
          Just (FtrIntg(IntRange (Just lo) Nothing  )) -> AppE (VarE 'with_int_fr)    (LitE $ IntegerL $ toInteger lo)
          Just (FtrIntg(IntRange (Just lo) (Just hi))) -> ap2  (VarE 'with_int_fr_to) (LitE $ IntegerL $ toInteger lo) (LitE $ IntegerL $ toInteger hi)
          _                                           -> VarE 'withInt

    wth_u =
        case snFilter sn of
          Just (FtrUTC(UTCRange Nothing   (Just hi))) -> AppE (VarE 'with_utc_to)    (LitE $ StringL $ utc_lit hi)
          Just (FtrUTC(UTCRange (Just lo) Nothing  )) -> AppE (VarE 'with_utc_fr)    (LitE $ StringL $ utc_lit lo)
          Just (FtrUTC(UTCRange (Just lo) (Just hi))) -> ap2  (VarE 'with_utc_fr_to) (LitE $ StringL $ utc_lit lo) (LitE $ StringL $ utc_lit hi)
          _                                            -> VarE 'withUTC



gen_sr_to :: APINode -> SpecRecord -> Q [Dec]
gen_sr_to as sr = mkInstanceIfNotExists ''ToJSON [ConT $ rep_type_nm as] [fd]
  where
    fd  = FunD 'toJSON [cl]

    cl  = Clause [VarP x_nm] bdy []

    bdy = NormalB $ AppE (VarE 'object) $
            ListE [ AppE (AppE (VarE '(.=))
                         (LitE $ StringL $ _FieldName fn)) $
                                AppE (VarE $ pre fn) (VarE x_nm) | fn<-fns ]

    pre = pref_field_nm as

    fns = map fst $ srFields sr

gen_sr_fm :: APINode -> SpecRecord -> Q [Dec]
gen_sr_fm as sr = mk_FromJSON_instances (rep_type_nm as) [cl,cl']
  where
    cl   = Clause [ConP 'Object [VarP x_nm]] (NormalB bdy) []
    bdy  = app (ConE $ rep_type_nm as)
                [ AppE (AppE (VarE '(.:)) (VarE x_nm))
                        (LitE $ StringL $ _FieldName fn) | fn<-fns ]
    fns  = map fst $ srFields sr

    cl'  = Clause [VarP x_nm] (NormalB bdy') []
    bdy' = AppE (VarE 'failWith) $ AppE (VarE 'expectedObject) $ VarE x_nm



gen_su_to :: APINode -> SpecUnion -> Q [Dec]
gen_su_to as su = mkInstanceIfNotExists ''ToJSON [ConT $ rep_type_nm as] [fd]
  where
    fd     = FunD 'toJSON $ map cl fns

    cl  fn = Clause [ConP (pre fn) [VarP x_nm]] (bdy fn) []

    bdy fn = NormalB $ AppE (VarE 'object) $
                ListE [ AppE (AppE (VarE '(.=))
                         (LitE $ StringL $ _FieldName fn)) (VarE x_nm) ]

    pre    = pref_con_nm as

    fns    = map fst $ suFields su

gen_su_fm :: APINode -> SpecUnion -> Q [Dec]
gen_su_fm as su = mk_FromJSON_instances (rep_type_nm as) [cl,cl']
  where
    cl   = Clause [ConP 'Object [VarP x_nm]] (NormalB bdy) []
    bdy  = AppE (AppE (VarE 'alternatives) $
                (AppE (VarE 'failWith) $ AppE (ConE 'MissingAlt) $
                          ListE [ LitE $ StringL $ _FieldName fn | fn<- fns ])) $
                ListE
                [ AppE (AppE (VarE 'fmap) (ConE $ pref_con_nm as fn)) $
                        AppE (AppE (VarE '(.::)) (VarE x_nm)) $
                             LitE $ StringL $ _FieldName fn | fn<-fns ]
    fns  = map fst $ suFields su

    cl'  = Clause [VarP x_nm] (NormalB bdy') []
    bdy' = AppE (VarE 'failWith) $ AppE (VarE 'expectedObject) $ VarE x_nm


gen_se_to :: APINode -> SpecEnum -> Q [Dec]
gen_se_to as _se = mkInstanceIfNotExists ''ToJSON [ConT $ rep_type_nm as] [FunD 'toJSON [cl]]
  where
    cl  = Clause [] bdy []

    bdy = NormalB $ AppE (AppE (VarE '(.)) (ConE 'String)) $
                VarE $ txt_nm as

gen_se_fm :: APINode -> SpecEnum -> Q [Dec]
gen_se_fm as _se = mk_FromJSON_instances (rep_type_nm as) [Clause [] bdy []]
  where
    bdy = NormalB $ AppE (VarE 'jsonStrMap_p) (VarE $ map_nm as)



gen_in :: APINode -> Q [Dec]
gen_in an = case anConvert an of
  Just (inj_fn, _) -> mk_FromJSON_instances (type_nm an) [cl]
   where
    cl     = Clause [VarP x_nm] bdy []

    bdy    = NormalB $ ap2 (VarE '(>>=))
                           (AppE (VarE 'parseJSONWithErrs) (VarE x_nm))
                           (VarE inj_nm)

    inj_nm = mkName $ _FieldName inj_fn

  Nothing -> return []

gen_pr :: APINode -> Q [Dec]
gen_pr an = case anConvert an of
  Just (_, prj_fn) -> mkInstanceIfNotExists ''ToJSON [ConT $ type_nm an] [fd]
   where
    fd     = FunD 'toJSON [cl]

    cl     = Clause [] bdy []

    bdy    = NormalB $ ap2 (VarE '(.)) (VarE 'toJSON) (VarE prj_nm)

    prj_nm = mkName $ _FieldName prj_fn

  Nothing-> return []


ap2 :: Exp -> Exp -> Exp -> Exp
ap2 f e e' = AppE (AppE f e) e'


utc_lit :: UTCTime -> String
utc_lit = mkUTC_


alternatives :: Alternative t => t a -> [t a] -> t a
alternatives none = foldr (<|>) none

mkInt :: Int -> Value
mkInt = Number . fromInteger . toInteger


jsonStrMap_p :: Ord a => Map.Map T.Text a -> Value -> ParserWithErrs a
jsonStrMap_p mp = json_string_p (Map.keys mp) $ flip Map.lookup mp

json_string_p :: Ord a => [T.Text] -> (T.Text->Maybe a) -> Value -> ParserWithErrs a
json_string_p xs p (String t) | Just val <- p t = pure val
                              | otherwise       = failWith $ UnexpectedEnumVal xs t
json_string_p _  _ v                            = failWith $ expectedString v



-- | Make instance of FromJSONWithErrs given the type name and clauses
-- of the parseJSON definition
mk_FromJSON_instances :: Name -> [Clause] -> Q [Dec]
mk_FromJSON_instances nm cls =
    mkInstanceIfNotExists ''FromJSONWithErrs [ConT nm]
        [FunD 'parseJSONWithErrs cls]
