{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.API.Test.Time (timeTests) where

import           Data.API.Time

import           Data.Time
import qualified Data.Text                      as T
import           Test.Tasty
import qualified Test.Tasty.QuickCheck          as QC

timeTests :: TestTree
timeTests = testGroup "Time" [ QC.testProperty "parseUTC/printUTC round-trip"    parseUTCPrintUTC
                             , QC.testProperty "parseUTC/show round-trip"        parseUTCShow
                             , QC.testProperty "parseUTC/parseUTC_old agreement" parseUTCReference
                             ]

-- | 'printUTC' drops the fractional seconds, so check that 'parseUTC' of its
-- result is the same up to removing them.
parseUTCPrintUTC :: QC.Property
parseUTCPrintUTC = QC.forAll moreArbitraryUTCTime $ \u -> parseUTC (printUTC u) QC.=== Just (stripFractionalSeconds u)

-- | 'show' does not drop the fractional seconds, so check that 'parseUTC' of
-- its result is the original time.
parseUTCShow :: QC.Property
parseUTCShow = QC.forAll moreArbitraryUTCTime $ \u -> parseUTC (T.pack (show u)) QC.=== Just u

stripFractionalSeconds :: UTCTime -> UTCTime
stripFractionalSeconds (UTCTime d t) = UTCTime d (fromIntegral (floor t :: Integer))


-- | Test that the reference implementation and optimized implementation of
-- 'parseUTC' agree on a variety of inputs.
--
-- Note that the optimized implementation is more permissive, where out-of-range
-- times are supplied (e.g. it allows seconds to exceed 60).
parseUTCReference :: QC.Property
parseUTCReference = QC.forAll genTimestamp (\ s -> parseUTC s QC.=== parseUTC_old s)

-- | Generate a (roughly) ISO 8601 format data/time string.
genTimestamp :: QC.Gen T.Text
genTimestamp = do
    day <- fromGregorian <$> QC.choose (1999,2100) <*> QC.choose (1,12) <*> QC.choose(1,31)
    sep <- QC.elements [" ", "T"]
    hh <- gen_hh
    mm <- gen_mm
    mb_ss <- gen_ss
    tz <- QC.oneof [ pure "", pure "Z", gen_tz_offset ]
    pure (T.pack (show day ++ sep ++ hh ++ ":" ++ mm ++ maybe "" (":" ++) mb_ss ++ tz))
  where
    gen_hh = pad2 . show <$> QC.choose (0,23 :: Int)
    gen_mm = pad2 . show <$> QC.choose (0,59 :: Int)
    gen_ss = fmap (pad2 . show) <$> QC.oneof [Just <$> QC.choose (0,59 :: Int), pure Nothing]

    -- Either + or - followed by one of HH:MM or HHMM
    -- (the new implementation accepts just HH too, but the old one did not)
    gen_tz_offset = do
      sign <- QC.elements ["+","-"]
      hh <- gen_hh
      sep <- QC.elements [":", ""]
      mm <- gen_mm
      pure (sign ++ hh ++ sep ++ mm)

    pad2 [c] = ['0',c]
    pad2 cs  = cs

-- | The @Arbitrary UTCTime@ instance defined in 'Data.API.Time' does not
-- generate subsecond-resolution times, whereas this generator does.
moreArbitraryUTCTime :: QC.Gen UTCTime
moreArbitraryUTCTime =
    QC.oneof [ QC.arbitrary
             , QC.elements [ unsafeParseUTC "1965-03-10T09:23:01.001Z"
                           , unsafeParseUTC "1965-03-10T09:23:01.000001Z"
                           ]
             , UTCTime <$> arbitraryDay <*> arbitraryDiffTime
             ]
  where
    arbitraryDay = fromGregorian <$> QC.choose (1999,2100) <*> QC.choose (1,12) <*> QC.choose(1,31)
    arbitraryDiffTime = picosecondsToDiffTime <$> QC.choose (0, 86401*10^(12 :: Int))
