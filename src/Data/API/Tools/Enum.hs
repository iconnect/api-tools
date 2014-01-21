{-# LANGUAGE TemplateHaskell            #-}
module Data.API.Tools.Enum
    ( enumTool
    , txt_nm
    , map_nm
    ) where

import           Data.API.TH
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Types

import           Control.Applicative
import qualified Data.Text                      as T
import qualified Data.Map                       as Map
import           Language.Haskell.TH


enumTool :: APITool
enumTool = apiNodeTool enumNodeTool

enumNodeTool :: APINodeTool
enumNodeTool an | SpEnum se <- anSpec an = (++) <$> gen_se_tx an se <*> gen_se_mp an se
                | otherwise              = return []


gen_se_tx :: APINode -> SpecEnum -> Q [Dec]
gen_se_tx as se = return [ SigD (txt_nm as) $ AppT (AppT ArrowT $ ConT tnm) $ ConT ''T.Text
                         , FunD (txt_nm as) [Clause [VarP x_nm] bdy []] ]
  where
    tnm = rep_type_nm as

    bdy    = NormalB $
                CaseE (VarE x_nm) [ Match (pt fnm) (bd fnm) [] | fnm<-fnms ]

    fnms   = map fst $ seAlts se

    pt fnm = ConP (pref_con_nm as fnm) []

    bd fnm = NormalB $ LitE $ StringL $ _FieldName fnm


gen_se_mp :: APINode -> SpecEnum -> Q [Dec]
gen_se_mp as _se = return [ SigD (map_nm as) $
                                AppT (AppT (ConT ''Map.Map) $ ConT ''T.Text) $ ConT tnm
                          , FunD (map_nm as) [Clause [] bdy []] ]
  where
    tnm = rep_type_nm as

    bdy    = NormalB $ AppE (VarE 'genTextMap) (VarE $ txt_nm as)


genTextMap :: (Ord a,Bounded a,Enum a) => (a->T.Text) -> Map.Map T.Text a
genTextMap f = Map.fromList [ (f x,x) | x<-[minBound..maxBound] ]


txt_nm, map_nm :: APINode -> Name
txt_nm         an = mkName $ "_text_" ++ (_TypeName $ anName an)
map_nm         an = mkName $ "_map_"  ++ (_TypeName $ anName an)
