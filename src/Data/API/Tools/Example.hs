{-# LANGUAGE TemplateHaskell            #-}

module Data.API.Tools.Example
    ( exampleTool
    ) where

import           Data.API.TH
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Types

import           Data.Aeson
import           Language.Haskell.TH.Syntax
import           Test.QuickCheck                as QC


exampleTool :: String -> APITool
exampleTool nm_s = generateSamples nm_s `appendTool` apiNodeTool exampleNodeTool

generateSamples :: String -> APITool
generateSamples nm_s api' = return
    [ SigD nm $ AppT ListT $ AppT (AppT (TupleT 2) (ConT ''String))
                                        (AppT (ConT ''Gen) (ConT ''Value))
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
             , VarE 'fmap `AppE` VarE 'toJSON `AppE`
                    SigE (VarE 'example) (AppT (ConT ''Gen) (ConT tnm))
             ]
      where
        tnm   = mkName tnm_s
        tnm_s = _TypeName $ anName an


exampleNodeTool :: APINodeTool
exampleNodeTool = apiSpecTool gen_sn_ex gen_sr_ex gen_su_ex gen_se_ex (const emptyTool)

-- | Generate an Example instance for a newtype, using the filter, or
-- an example of the underlying type.  Like Arbitrary, if a regular
-- expression filter is applied the instance must be defined manually.
gen_sn_ex :: APINode -> SpecNewtype -> Q [Dec]
gen_sn_ex as sn = case snFilter sn of
                               Nothing          -> inst bdy
                               Just (FtrStrg _) -> return []
                               Just _           -> inst $ VarE 'QC.arbitrary
  where
    inst e = mkInstanceIfNotExists ''Example [ConT tn]
                 [FunD 'example [Clause [] (NormalB e) []]]

    bdy  = AppE (AppE (VarE 'fmap) $ ConE $ rep_type_nm as) $
                           VarE 'example

    tn     = rep_type_nm as


gen_sr_ex :: APINode -> SpecRecord -> Q [Dec]
gen_sr_ex as sr = mkInstanceIfNotExists ''Example [ConT tn] [FunD 'example [cl]]
  where
    cl    = Clause [] bdy []

    -- Ditto for example
    bdy   = NormalB $ AppE (VarE 'QC.sized) $ LamE [VarP x_nm] $
                app (ConE tn) $
                replicate (length $ srFields sr) $
                VarE 'QC.resize
                         `AppE` (VarE 'div `AppE` VarE x_nm `AppE` LitE (IntegerL 2))
                         `AppE` VarE 'example

    tn    = rep_type_nm as


gen_su_ex :: APINode -> SpecUnion -> Q [Dec]
gen_su_ex as su = mkInstanceIfNotExists ''Example [ConT tn] [FunD 'example [cl]]
  where
    cl  = Clause [] bdy []

    bdy = NormalB $ if null ks then emp else prp

    emp = ConE tn

    prp = AppE (VarE 'oneof) $
                ListE [ VarE 'fmap `AppE` ConE k `AppE` VarE 'example | k<- ks ]

    tn    = rep_type_nm as

    ks  = map (pref_con_nm as) $ map fst $ suFields su

gen_se_ex :: APINode -> SpecEnum -> Q [Dec]
gen_se_ex as _ = mkInstanceIfNotExists ''Example [ConT tn] []
  where
    tn    = rep_type_nm as
