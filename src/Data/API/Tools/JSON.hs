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
import           Data.API.Types hiding (withUTC)

import           Data.Aeson hiding (withText, withBool)
import           Control.Applicative
import qualified Data.Map                       as Map
import qualified Data.Text                      as T
import           Data.Time
import           Language.Haskell.TH


-- | Tool to generate 'ToJSON' and 'FromJSONWithErrs' instances for
-- types generated by 'datatypesTool'.  This depends on 'enumTool'.
jsonTool :: APITool
jsonTool = apiNodeTool $ toJsonNodeTool `appendTool` fromJsonNodeTool


-- | Tool to generate 'ToJSON' instance for an API node
toJsonNodeTool :: APINodeTool
toJsonNodeTool = apiSpecTool gen_sn_to gen_sr_to gen_su_to gen_se_to (const emptyTool)
                 `appendTool` gen_pr

-- | Tool to generate 'FromJSONWithErrs' instance for an API node
fromJsonNodeTool :: APINodeTool
fromJsonNodeTool = apiSpecTool gen_sn_fm gen_sr_fm gen_su_fm gen_se_fm (const emptyTool)
                   `appendTool` gen_in


{-
instance ToJSON JobId where
    toJSON = String . _JobId

instance FromJSONWithErrs JobId where
    parseJSONWithErrs = withText "JobId" (pure . JobId)
-}

gen_sn_to :: APINode -> SpecNewtype -> Q [Dec]
gen_sn_to an sn = optionalInstanceD ''ToJSON [nodeRepT an]
                      [simpleD 'toJSON bdy]
  where
    bdy = [e| $ine . $(newtypeProjectionE an) |]

    ine = case snType sn of
            BTstring -> [e| String |]
            BTbinary -> [e| toJSON |]
            BTbool   -> [e| Bool   |]
            BTint    -> [e| mkInt  |]
            BTutc    -> [e| mkUTC  |]

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



{-
instance FromJSONWithErrs JobSpecId where
     parseJSONWithErrs (Object v) =
        JobSpecId <$>
            v .: "Id"                               <*>
            v .: "Input"                            <*>
            v .: "Output"                           <*>
            v .: "PipelineId"
     parseJSONWithErrs v          = failWith $ expectedObject val

instance ToJSON JobSpecId where
     toJSON = \ x ->
        object
            [ "Id"         .= jsiId         x
            , "Input"      .= jsiInput      x
            , "Output"     .= jsiOutput     x
            , "PipelineId" .= jsiPipelineId x
            ]
-}

gen_sr_to :: APINode -> SpecRecord -> Q [Dec]
gen_sr_to an sr = do
    x <- newName "x"
    optionalInstanceD ''ToJSON [nodeRepT an] [simpleD 'toJSON (bdy x)]
  where
    bdy x = lamE [varP x] $
            varE 'object `appE`
            listE [ [e| $(fieldNameE fn) .= $(nodeFieldE an fn) $(varE x) |]
                  | (fn, _) <- srFields sr ]

gen_sr_fm :: APINode -> SpecRecord -> Q [Dec]
gen_sr_fm as sr = mk_FromJSON_instances (rep_type_nm as) [cl,cl']
  where
    cl   = Clause [ConP 'Object [VarP x_nm]] (NormalB bdy) []
    bdy  = app (ConE $ rep_type_nm as)
                [ AppE (AppE (VarE '(.:.)) (VarE x_nm))
                        (LitE $ StringL $ _FieldName fn) | fn<-fns ]
    fns  = map fst $ srFields sr

    cl'  = Clause [VarP x_nm] (NormalB bdy') []
    bdy' = AppE (VarE 'failWith) $ AppE (VarE 'expectedObject) $ VarE x_nm


{-
instance ToJSON Foo where
    toJSON (Bar x) = object [ "x" .= x ]
    toJSON (Baz x) = object [ "y" .= x ]

instance FromJSONWithErrs Foo where
    parseJSONWithErrs (Object v) = alternatives (failWith $ MissingAlt ["x", "y"])
        [ Bar <$> v .:: "x"
        , Baz <$> v .:: "y"
        ]
    parseJSONWithErrs val        = failWith $ expectedObject val
-}

gen_su_to :: APINode -> SpecUnion -> Q [Dec]
gen_su_to an su = optionalInstanceD ''ToJSON [nodeRepT an] [funD 'toJSON cls]
  where
    cls      = map (cl . fst) (suFields su)

    cl  fn   = do x <- newName "x"
                  clause [nodeAltConP an fn [varP x]] (bdy fn x) []

    bdy fn x = normalB [e| object [ $(fieldNameE fn) .= $(varE x) ] |]

gen_su_fm :: APINode -> SpecUnion -> Q [Dec]
gen_su_fm an su = optionalInstanceD ''FromJSONWithErrs [nodeRepT an]
                      [funD 'parseJSONWithErrs [cl,cl']]
  where
    cl   = clause [conP 'Object [varP x_nm]] (normalB bdy) []
    bdy  = [e| alternatives (failWith $ MissingAlt $ss) $alts |]

    alt fn = [e| fmap $(nodeAltConE an fn) ($(varE x_nm) .:: $(fieldNameE fn)) |]

    alts = listE $ map alt fns
    ss   = listE $ map fieldNameE fns

    fns  = map fst $ suFields su

    cl'  = clause [varP x_nm] (normalB bdy') []
    bdy' = [e| failWith (expectedObject $(varE x_nm)) |]



{-
instance ToJSON FrameRate where
    toJSON    = String . _text_FrameRate

instance FromJSONWithErrs FrameRate where
    parseJSONWithErrs = jsonStrMap_p _map_FrameRate
-}

gen_se_to :: APINode -> SpecEnum -> Q [Dec]
gen_se_to an _se = optionalInstanceD ''ToJSON [nodeRepT an] [simpleD 'toJSON bdy]
  where
    bdy = [e| String . $(varE (text_enum_nm an)) |]

gen_se_fm :: APINode -> SpecEnum -> Q [Dec]
gen_se_fm as _se = mk_FromJSON_instances (rep_type_nm as) [Clause [] bdy []]
  where
    bdy = NormalB $ AppE (VarE 'jsonStrMap_p) (VarE $ map_enum_nm as)



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
  Nothing          -> return []
  Just (_, prj_fn) -> optionalInstanceD ''ToJSON [nodeT an] [simpleD 'toJSON bdy]
   where
    bdy = [e| toJSON . $prj |]
    prj = varE $ mkName $ _FieldName prj_fn


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


fieldNameE :: FieldName -> ExpQ
fieldNameE = stringE . _FieldName
