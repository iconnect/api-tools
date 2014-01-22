{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Data.API.Tools.QuickCheck
    ( quickCheckTool
    ) where

import           Data.API.TH
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Types

import           Control.Applicative
import           Data.Time
import           Language.Haskell.TH.Syntax
import           Safe
import           Test.QuickCheck                as QC


-- | Tool to generate 'Arbitrary' instances for generated types.
quickCheckTool :: APITool
quickCheckTool = apiNodeTool $ apiSpecTool gen_sn_ab gen_sr_ab gen_su_ab gen_se_ab (const emptyTool)


-- | Generate an Arbitrary instance for a newtype that respects its
-- filter.  We don't try to generate arbitrary data matching a regular
-- expression, however: instances must be supplied manually.  When
-- generating arbitrary integers, use arbitraryBoundedIntegral rather
-- than arbitrary (the latter tends to generate non-unique values).
gen_sn_ab :: APINode -> SpecNewtype -> Q [Dec]
gen_sn_ab as sn = case snFilter sn of
    Nothing -> case snType sn of
                 BTint -> mk_instance $ VarE 'QC.arbitraryBoundedIntegral
                 _     -> mk_instance $ VarE 'arbitrary
    Just (FtrStrg _)                -> return []
    Just (FtrIntg (IntRange lo hi)) -> mk_instance $ arbitrary_IntRange lo hi
    Just (FtrUTC (UTCRange lo hi))  -> mk_instance $ arbitrary_UTCRange lo hi
  where
    mk_instance arb = mkInstanceIfNotExists ''Arbitrary [ConT tn]
                          [FunD 'arbitrary [Clause [] (bdy arb) []]]

    tn  = rep_type_nm as

    bdy arb = NormalB $ VarE 'fmap `AppE` ConE tn `AppE` arb

    arbitrary_IntRange lo hi = arbitrary_range liftInt 'choose_int_fr
                                   'choose_int_to 'choose_int_fr_to lo hi

    arbitrary_UTCRange lo hi = arbitrary_range liftUTC 'choose_time_fr
                                   'choose_time_to 'choose_time_fr_to lo hi

    arbitrary_range _ _   _   _ Nothing   Nothing   = VarE 'QC.arbitrary
    arbitrary_range l cfr _   _ (Just lo)  Nothing  = VarE cfr `AppE` l lo
    arbitrary_range l _   cto _ Nothing   (Just hi) = VarE cto `AppE` l hi
    arbitrary_range l _   _   c (Just lo) (Just hi) = VarE c `AppE` l lo `AppE` l hi

    liftInt i = LitE $ IntegerL $ toInteger i

    liftUTC u = AppE (VarE 'fromJustNote `AppE` LitE (StringL "gen_sn_sb")) $
                AppE (VarE 'parseUTC_) $
                LitE $ StringL $ mkUTC_ u


gen_sr_ab :: APINode -> SpecRecord -> Q [Dec]
gen_sr_ab as sr = mkInstanceIfNotExists ''QC.Arbitrary [ConT tn] [FunD 'arbitrary [cl]]
  where
    cl    = Clause [] bdy []

    -- Reduce size of fields to avoid generating massive test data
    -- by giving an arbitrary implementation like this:
    --   sized (\ x -> JobSpecId <$> resize (x `div` 2) arbitrary <*> ...)
    bdy   = NormalB $ AppE (VarE 'QC.sized) $ LamE [VarP x_nm] $
                app (ConE tn) $
                replicate (length $ srFields sr) $
                VarE 'QC.resize
                         `AppE` (VarE 'div `AppE` VarE x_nm `AppE` LitE (IntegerL 2))
                         `AppE` VarE 'arbitrary

    tn    = rep_type_nm as


gen_su_ab :: APINode -> SpecUnion -> Q [Dec]
gen_su_ab as su = mkInstanceIfNotExists ''QC.Arbitrary [ConT tn] [FunD 'arbitrary [cl]]
  where
    cl  = Clause [] bdy []

    bdy = NormalB $ if null ks then emp else prp

    emp = ConE tn

    prp = AppE (VarE 'oneof) $
                ListE [ VarE 'fmap `AppE` ConE k `AppE` VarE 'arbitrary | k<- ks ]

    tn  = rep_type_nm as
    ks  = map (pref_con_nm as) $ map fst $ suFields su


gen_se_ab :: APINode -> SpecEnum -> Q [Dec]
gen_se_ab as se = mkInstanceIfNotExists ''QC.Arbitrary [ConT tn] [FunD 'arbitrary [cl]]
  where
    cl  = Clause [] bdy []

    bdy = NormalB $ if null ks then emp else prp

    emp = ConE tn

    prp = AppE (VarE 'elements) $ ListE [ ConE k | k<- ks ]

    tn  = rep_type_nm as

    ks  = map (pref_con_nm as . fst) $ seAlts se



choose_int_fr :: Int -> QC.Gen Int
choose_int_fr lo = QC.choose (lo, maxBound)

choose_int_to :: Int -> QC.Gen Int
choose_int_to hi = QC.choose (minBound, hi)

choose_int_fr_to :: Int -> Int -> QC.Gen Int
choose_int_fr_to lo hi = QC.choose (lo, hi)


-- TODO: we might want to generate a broader range of sample times,
-- rather than just the extrema
choose_time_fr :: UTCTime -> QC.Gen UTCTime
choose_time_fr lo = pure lo

choose_time_to :: UTCTime -> QC.Gen UTCTime
choose_time_to hi = pure hi

choose_time_fr_to :: UTCTime -> UTCTime -> QC.Gen UTCTime
choose_time_fr_to lo hi = QC.elements [lo, hi]



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

        lab = "Data.API.Tools.QuickCheck.Arbitrary-UTCTime"
