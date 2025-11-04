{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Data.API.Tools.QuickCheck
    ( quickCheckTool
    ) where

import           Data.API.TH
import           Data.API.Time ()
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Types

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Data.Time
import           Language.Haskell.TH
import           Prelude
import           Test.QuickCheck                as QC


-- | Tool to generate 'Arbitrary' instances for generated types. This tool generates
-- also a stock shrinker via the 'generic-arbitrary' package, which means we require
-- the wrapped type to be an instance of 'Generic'.
quickCheckTool :: APITool
quickCheckTool = apiNodeTool $ apiSpecTool gen_sn_ab gen_sr_ab gen_su_ab gen_se_ab mempty

-- | Helper to create an 'Arbitrary' implementation.
mkArbitraryInstance :: ToolSettings
                    -> TypeQ
                    -> ExpQ
                    -- ^ The body of the 'arbitrary' method.
                    -> ExpQ
                    -- ^ The body of the 'shrink' method.
                    -> Q [Dec]
mkArbitraryInstance ts typeQ arbitraryBody shrinkBody = do
  optionalInstanceD ts ''QC.Arbitrary [typeQ]
                                      [ simpleD 'arbitrary arbitraryBody
                                      , simpleD 'shrink    shrinkBody
                                      ]

-- | Generate an 'Arbitrary' instance for a newtype that respects its
-- filter.  We don't try to generate arbitrary data matching a regular
-- expression, however: instances must be supplied manually.  When
-- generating arbitrary integers, use 'arbitraryBoundedIntegral'
-- rather than 'arbitrary' (the latter tends to generate non-unique
-- values).
gen_sn_ab :: Tool (APINode, SpecNewtype)
gen_sn_ab = mkTool $ \ ts (an, sn) -> case snFilter sn of
    Nothing | snType sn == BTint    -> mk_instance ts an sn [e| QC.arbitraryBoundedIntegral |] (shrinkNewtype ts an sn)
            | otherwise             -> mk_instance ts an sn [e| arbitrary |] (shrinkNewtype ts an sn)
    Just (FtrIntg ir)               ->
      mk_instance ts an sn [e| arbitraryIntRange ir |] (shrinkNewtype ts an sn)
    Just (FtrUTC ur)                ->
      mk_instance ts an sn [e| arbitraryUTCRange ur |] (shrinkNewtype ts an sn)
    Just (FtrStrg _)                -> return []
  where
    mk_instance ts an sn arb =
      mkArbitraryInstance ts (nodeRepT an) [e| fmap $(nodeNewtypeConE ts an sn) $arb |]

    -- shrinking a newtype means calling shrink and repack the newtype.
    -- Example:
    -- shrink = \x -> case x of { Foo y -> map Foo (shrink y) }
    shrinkNewtype ts an sn = do
      x <- newName "x"
      y <- newName "y"
      lamE [varP x] $
        caseE (varE x) [
          match (nodeNewtypeConP ts an sn [varP y])
                (normalB [| map $(nodeNewtypeConE ts an sn) (QC.shrink $(varE y)) |])
                []
        ]

-- | Generate an 'Arbitrary' instance for a record:
--
-- > instance Arbitrary Foo where
-- >     arbitrary = sized $ \ x -> Foo <$> resize (x `div` 2) arbitrary <*> ... <*> resize (x `div` 2) arbitrary
-- >     shrink    = (TH-derived shrinker)

gen_sr_ab :: Tool (APINode, SpecRecord)
gen_sr_ab = mkTool $ \ ts (an, sr) -> mkArbitraryInstance ts (nodeRepT an) (bdy an sr) (shrinkRecord an sr)
  where
    -- Reduce size of fields to avoid generating massive test data
    -- by giving an arbitrary implementation like this:
    --   sized (\ x -> JobSpecId <$> resize (x `div` 2) arbitrary <*> ...)
    bdy an sr = do x <- newName "x"
                   appE (varE 'QC.sized) $ lamE [varP x] $
                     applicativeE (nodeConE an) $
                     replicate (length $ srFields sr) $
                     [e| QC.resize ($(varE x) `div` 2) arbitrary |]

    -- For records, using the same principle behind 'genericShrink', we need
    -- to generate a list of lists, each sublist being the shrinking of a single
    -- individual field, and finally mconcat everything together.
    -- Example:
    -- shrink = \(Foo a b c) ->
    --  (Foo <$> shrink a <*> pure b   <*> pure c) ++
    --  (Foo <$> pure   a <*> shrink b <*> pure c) ++
    --  (Foo <$> pure   a <*> pure b   <*> shrink c)
    shrinkRecord :: APINode -> SpecRecord -> ExpQ
    shrinkRecord an sr = do
      x <- newName "x"
      -- Matches the fields of the record with fresh variables
      -- [( "field1", "field1"), ("field2", "field2") ... ]
      recordPatterns <-
        forM (srFields sr) $ \(fn,_) -> do
          let freshRecName = pref_field_nm an fn
          freshPatName <- nodeFieldP an fn
          pure (freshRecName,freshPatName)

      lamE [varP x] $
        caseE (varE x) [
          -- temporary, not correct. it won't shrink properly.
          match (recP nm (map pure recordPatterns))
                (normalB $ applicativeE (nodeConE an) $
                   flip map recordPatterns $ \(fld, _pat) ->
                    [e| QC.shrink $(varE fld) |]
                ) []
        ]
       where
         nm = rep_type_nm an

-- | Generate an 'Arbitrary' instance for a union:
--
-- > instance Arbitrary Foo where
-- >     arbitrary = oneOf [ fmap Bar arbitrary, fmap Baz arbitrary ]

gen_su_ab :: Tool (APINode, SpecUnion)
gen_su_ab = mkTool $ \ ts (an, su) -> mkArbitraryInstance ts (nodeRepT an) (bdy an su) (shrinkUnion an su)
  where
    bdy an su | null (suFields su) = nodeConE an
              | otherwise          = [e| oneof $(listE alts) |]
      where
        alts = [ [e| fmap $(nodeAltConE an k) arbitrary |]
               | (k, _) <- suFields su ]

    -- For a union, we shrink the individual wrappers.
    shrinkUnion :: APINode -> SpecUnion -> ExpQ
    shrinkUnion an su = do
      x <- newName "x"
      y <- newName "y"
      lamE [varP x] $ caseE (varE x) (map (shrink_alt y) (suFields su))
      where
        shrink_alt y (fn,_) =
          match (nodeAltConP an fn [varP y])
                (normalB [| map $(nodeAltConE an fn) (QC.shrink $(varE y)) |])
                []

-- | Generate an 'Arbitrary' instance for an enumeration:
--
-- > instance Arbitrary Foo where
-- >     arbitrary = elements [Bar, Baz]

gen_se_ab :: Tool (APINode, SpecEnum)
gen_se_ab = mkTool $ \ ts (an, se) -> mkArbitraryInstance ts (nodeRepT an) (bdy an se) noShrink
  where
    bdy an se | null ks   = nodeConE an
              | otherwise = varE 'elements `appE` listE ks
      where
        ks = map (nodeAltConE an . fst) $ seAlts se

    noShrink :: ExpQ
    noShrink = [e| \_ -> [] |]

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
