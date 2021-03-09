{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Data.API.Tools.QuickCheck
    ( quickCheckTool
    ) where

import           Data.API.TH
import           Data.API.Time
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Types

import           Control.Applicative
import           Data.Monoid
import           Data.Time
import           Language.Haskell.TH
import           Test.QuickCheck                as QC
import           Prelude


-- | Tool to generate 'Arbitrary' instances for generated types.
quickCheckTool :: APITool
quickCheckTool = apiNodeTool $ apiSpecTool gen_sn_ab gen_sr_ab gen_su_ab gen_se_ab mempty


-- | Generate an 'Arbitrary' instance for a newtype that respects its
-- filter.  We don't try to generate arbitrary data matching a regular
-- expression, however: instances must be supplied manually.  When
-- generating arbitrary integers, use 'arbitraryBoundedIntegral'
-- rather than 'arbitrary' (the latter tends to generate non-unique
-- values).
gen_sn_ab :: Tool (APINode, SpecNewtype)
gen_sn_ab = mkTool $ \ ts (an, sn) -> case snFilter sn of
    Nothing | snType sn == BTint    -> mk_instance ts an sn [e| QC.arbitraryBoundedIntegral |]
            | otherwise             -> mk_instance ts an sn [e| arbitrary |]
    Just (FtrIntg ir)               -> mk_instance ts an sn [e| arbitraryIntRange ir |]
    Just (FtrUTC ur)                -> mk_instance ts an sn [e| arbitraryUTCRange ur |]
    Just (FtrStrg _)                -> return []
  where
    mk_instance ts an sn arb = optionalInstanceD ts ''Arbitrary [nodeRepT an]
                                  [simpleD 'arbitrary [e| fmap $(nodeNewtypeConE ts an sn) $arb |]]


-- | Generate an 'Arbitrary' instance for a record:
--
-- > instance Arbitrary Foo where
-- >     arbitrary = sized $ \ x -> Foo <$> resize (x `div` 2) arbitrary <*> ... <*> resize (x `div` 2) arbitrary

gen_sr_ab :: Tool (APINode, SpecRecord)
gen_sr_ab = mkTool $ \ ts (an, sr) -> optionalInstanceD ts ''QC.Arbitrary [nodeRepT an]
                                          [simpleD 'arbitrary (bdy an sr)]
  where
    -- Reduce size of fields to avoid generating massive test data
    -- by giving an arbitrary implementation like this:
    --   sized (\ x -> JobSpecId <$> resize (x `div` 2) arbitrary <*> ...)
    bdy an sr = do x <- newName "x"
                   appE (varE 'QC.sized) $ lamE [varP x] $
                     applicativeE (nodeConE an) $
                     replicate (length $ srFields sr) $
                     [e| QC.resize ($(varE x) `div` 2) arbitrary |]


-- | Generate an 'Arbitrary' instance for a union:
--
-- > instance Arbitrary Foo where
-- >     arbitrary = oneOf [ fmap Bar arbitrary, fmap Baz arbitrary ]

gen_su_ab :: Tool (APINode, SpecUnion)
gen_su_ab = mkTool $ \ ts (an, su) -> optionalInstanceD ts ''QC.Arbitrary [nodeRepT an]
                                          [simpleD 'arbitrary (bdy an su)]
  where
    bdy an su | null (suFields su) = nodeConE an
              | otherwise          = [e| oneof $(listE alts) |]
      where
        alts = [ [e| fmap $(nodeAltConE an k) arbitrary |]
               | (k, _) <- suFields su ]


-- | Generate an 'Arbitrary' instance for an enumeration:
--
-- > instance Arbitrary Foo where
-- >     arbitrary = elements [Bar, Baz]

gen_se_ab :: Tool (APINode, SpecEnum)
gen_se_ab = mkTool $ \ ts (an, se) -> optionalInstanceD ts ''QC.Arbitrary [nodeRepT an]
                                          [simpleD 'arbitrary (bdy an se)]
  where
    bdy an se | null ks   = nodeConE an
              | otherwise = varE 'elements `appE` listE ks
      where
        ks = map (nodeAltConE an . fst) $ seAlts se


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
-- (in particular, there are no subsecond-resolution times here)
instance QC.Arbitrary UTCTime where
    arbitrary = QC.elements
        [ mk "2010-01-01T00:00:00Z"
        , mk "2013-05-27T19:13:50Z"
        , mk "2011-07-20T22:04:00Z"
        , mk "2012-02-02T15:45:11Z"
        , mk "2009-11-12T20:57:54Z"
        , mk "2000-10-28T21:03:24Z"
        , mk "1965-03-10T09:23:01Z"
        -- , mk "1965-03-10T09:23:01.001Z"
        -- , mk "1965-03-10T09:23:01.000001Z"
        ]
      where
        mk = unsafeParseUTC
