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
import           Language.Haskell.TH
import           Safe
import           Test.QuickCheck                as QC


-- | Tool to generate 'Arbitrary' instances for generated types.
quickCheckTool :: APITool
quickCheckTool = apiNodeTool $ apiSpecTool gen_sn_ab gen_sr_ab gen_su_ab gen_se_ab (const emptyTool)


-- | Generate an 'Arbitrary' instance for a newtype that respects its
-- filter.  We don't try to generate arbitrary data matching a regular
-- expression, however: instances must be supplied manually.  When
-- generating arbitrary integers, use 'arbitraryBoundedIntegral'
-- rather than 'arbitrary' (the latter tends to generate non-unique
-- values).
gen_sn_ab :: APINode -> SpecNewtype -> Q [Dec]
gen_sn_ab an sn = case snFilter sn of
    Nothing | snType sn == BTint    -> mk_instance [e| QC.arbitraryBoundedIntegral |]
            | otherwise             -> mk_instance [e| arbitrary |]
    Just (FtrIntg ir) -> mk_instance [e| arbitraryIntRange ir |]
    Just (FtrUTC ur)  -> mk_instance [e| arbitraryUTCRange ur |]
    Just (FtrStrg _)                -> return []
  where
    mk_instance arb = optionalInstanceD ''Arbitrary [nodeRepT an]
                          [simpleD 'arbitrary [e| fmap $(nodeConE an) $arb |]]


-- | Generate an 'Arbitrary' instance for a record:
--
-- > instance Arbitrary Foo where
-- >     arbitrary = sized $ \ x -> Foo <$> resize (x `div` 2) arbitrary <*> ... <*> resize (x `div` 2) arbitrary

gen_sr_ab :: APINode -> SpecRecord -> Q [Dec]
gen_sr_ab an sr = optionalInstanceD ''QC.Arbitrary [nodeRepT an] [simpleD 'arbitrary bdy]
  where
    -- Reduce size of fields to avoid generating massive test data
    -- by giving an arbitrary implementation like this:
    --   sized (\ x -> JobSpecId <$> resize (x `div` 2) arbitrary <*> ...)
    bdy   = do x <- newName "x"
               appE (varE 'QC.sized) $ lamE [varP x] $
                 applicativeE (nodeConE an) $
                 replicate (length $ srFields sr) $
                 [e| QC.resize ($(varE x) `div` 2) arbitrary |]


-- | Generate an 'Arbitrary' instance for a union:
--
-- > instance Arbitrary Foo where
-- >     arbitrary = oneOf [ fmap Bar arbitrary, fmap Baz arbitrary ]

gen_su_ab :: APINode -> SpecUnion -> Q [Dec]
gen_su_ab an su = optionalInstanceD ''QC.Arbitrary [nodeRepT an] [simpleD 'arbitrary bdy]
  where
    bdy | null (suFields su) = nodeConE an
        | otherwise          = [e| oneof $(listE alts) |]

    alts = [ [e| fmap $(nodeAltConE an k) arbitrary |]
           | (k, _) <- suFields su ]


-- | Generate an 'Arbitrary' instance for an enumeration:
--
-- > instance Arbitrary Foo where
-- >     arbitrary = elements [Bar, Baz]

gen_se_ab :: APINode -> SpecEnum -> Q [Dec]
gen_se_ab an se = optionalInstanceD ''QC.Arbitrary [nodeRepT an] [simpleD 'arbitrary bdy]
  where
    bdy | null ks   = nodeConE an
        | otherwise = varE 'elements `appE` listE ks

    ks  = map (nodeAltConE an . fst) $ seAlts se


-- | Generate an arbitrary 'Int' in a given range.
arbitraryIntRange :: IntRange -> Gen Int
arbitraryIntRange (IntRange (Just lo) Nothing  ) = QC.choose (lo, maxBound)
arbitraryIntRange (IntRange Nothing   (Just hi)) = QC.choose (minBound, hi)
arbitraryIntRange (IntRange (Just lo) (Just hi)) = QC.choose (lo, hi)
arbitraryIntRange (IntRange Nothing   Nothing  ) = QC.arbitrary

-- | Generate an arbitrary 'UTCTime' in a given range.
-- TODO: we might want to generate a broader range of sample times,
-- rather than just the extrema.
arbitraryUTCRange :: UTCRange -> Gen UTCTime
arbitraryUTCRange (UTCRange (Just lo) Nothing  ) = pure lo
arbitraryUTCRange (UTCRange Nothing   (Just hi)) = pure hi
arbitraryUTCRange (UTCRange (Just lo) (Just hi)) = QC.elements [lo, hi]
arbitraryUTCRange (UTCRange Nothing   Nothing  ) = QC.arbitrary

-- TODO: use a more arbitrary instance (quickcheck-instances?)
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
