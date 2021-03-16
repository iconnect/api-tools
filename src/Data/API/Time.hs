{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wwarn=orphans #-}
module Data.API.Time
    ( printUTC
    , parseUTC
    , parseDay
    , unsafeParseUTC
    , unsafeParseDay
    , parseUTC_old
    ) where

import           Control.Monad
import qualified Data.Attoparsec.Text           as AP
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text                      as T
import           Data.Time

import           GHC.Stack
import           Test.QuickCheck                as QC

utcFormat :: String
utcFormat =               "%Y-%m-%dT%H:%M:%SZ"

utcFormats :: [String]
utcFormats =
                        [ "%Y-%m-%dT%H:%M:%S%Z"
                        , "%Y-%m-%dT%H:%M:%S"
                        , "%Y-%m-%dT%H:%M%Z"
                        , "%Y-%m-%dT%H:%M"
                        , "%Y-%m-%dT%H:%M:%S%QZ"
                        , utcFormat
                        , "%Y-%m-%d %H:%M:%S"
                        , "%Y-%m-%d %H:%M:%S%Z"
                        , "%Y-%m-%d %H:%M:%S%QZ"
                        , "%Y-%m-%d %H:%M%Z"
                        , "%Y-%m-%d %H:%M"
                        ]

-- | Render a 'UTCTime' in ISO 8601 format to a precision of seconds
-- (i.e. omitting any subseconds).
printUTC :: UTCTime -> T.Text
printUTC utct = T.pack $ formatTime defaultTimeLocale utcFormat utct

-- | Parse text as a 'UTCTime' in ISO 8601 format or a number of slight
-- variations thereof (the @T@ may be replaced with a space, and the seconds,
-- milliseconds and/or @Z@ timezone indicator may optionally be omitted).
--
-- Time zone designations other than @Z@ for UTC are not currently supported.
parseUTC :: T.Text -> Maybe UTCTime
parseUTC t = case AP.parseOnly (parserUTCTime <* AP.endOfInput) t of
    Left _  -> Nothing
    Right r -> Just r

-- | Parse text as a 'Day' in @YYYY-MM-DD@ format.
parseDay :: T.Text -> Maybe Day
parseDay t = case AP.parseOnly (parserDay <* AP.endOfInput) t of
    Left _  -> Nothing
    Right r -> Just r


parserUTCTime :: AP.Parser UTCTime
parserUTCTime = do
    day <- parserDay
    void $ AP.skip (\c -> c == ' ' || c == 'T')
    time <- parserTime
    mb_offset <- parserTimeZone
    pure (maybe id addUTCTime mb_offset $ UTCTime day time)

-- | Parser for @YYYY-MM-DD@ format.
parserDay :: AP.Parser Day
parserDay = do
    y :: Int <- AP.decimal <* AP.char '-'
    m :: Int <- AP.decimal <* AP.char '-'
    d :: Int <- AP.decimal
    case fromGregorianValid (fromIntegral y) m d of
        Just x  -> pure x
        Nothing -> fail "invalid date"

-- | Parser for times in the format @HH:MM@, @HH:MM:SS@ or @HH:MM:SS.QQQ...@.
parserTime :: AP.Parser DiffTime
parserTime = do
    h :: Int <- AP.decimal
    void $ AP.char ':'
    m :: Int <- AP.decimal
    c <- AP.peekChar
    s <- case c of
           Just ':' -> AP.anyChar *> AP.scientific
           _        -> pure 0
    case toBoundedInteger (10^(12::Int) * (s + fromIntegral (60*(m + 60*h)))) of
      Just n -> pure (picosecondsToDiffTime (fromIntegral (n :: Int)))
      Nothing -> fail "seconds out of range"

-- | Parser for time zone indications such as @Z@, @ UTC@ or an explicit offset
-- like @+HH:MM@ or @-HH@.  Returns 'Nothing' for UTC.  Local times (without a
-- timezone designator) are assumed to be UTC.  If there is an explicit offset,
-- returns its negation.
parserTimeZone :: AP.Parser (Maybe NominalDiffTime)
parserTimeZone = do
    c <- AP.option 'Z' AP.anyChar
    case c of
      'Z' -> pure Nothing
      ' ' -> "UTC" *> pure Nothing
      '+' -> parse_offset True
      '-' -> parse_offset False
      _   -> fail "unexpected time zone character"
  where
    parse_offset pos = do
      hh :: Int <- read <$> AP.count 2 AP.digit
      AP.option () (AP.skip (== ':'))
      mm :: Int <- AP.option 0 (read <$> AP.count 2 AP.digit)
      let v = (if pos then negate else id) ((hh*60 + mm) * 60)
      return (Just (fromIntegral v))

-- | Parse text as a 'UTCTime' in ISO 8601 format or a number of slight
-- variations thereof (the @T@ may be replaced with a space, and the seconds and
-- timezone indicator may optionally be omitted).
parseUTC_old :: T.Text -> Maybe UTCTime
parseUTC_old t = stringToUTC $ T.unpack t

stringToUTC :: String -> Maybe UTCTime
stringToUTC s = listToMaybe $ catMaybes $
            map (\fmt->parseTimeM True defaultTimeLocale fmt s) utcFormats

-- | Variant of 'parseUTC' that throws an error if the input text could not be
-- parsed.
unsafeParseUTC :: HasCallStack => T.Text -> UTCTime
unsafeParseUTC t = fromMaybe (error msg) (parseUTC t)
  where
    msg = "unsafeParseUTC: unable to parse: " ++ T.unpack t

-- | Variant of 'parseDay' that throws an error if the input text could not be
-- parsed.
unsafeParseDay :: HasCallStack => T.Text -> Day
unsafeParseDay t = fromMaybe (error msg) (parseDay t)
  where
    msg = "unsafeParseDay: unable to parse: " ++ T.unpack t


-- TODO: use a more arbitrary instance (quickcheck-instances?)
-- (in particular, there are no subsecond-resolution times here)
instance QC.Arbitrary UTCTime where
    arbitrary = QC.oneof
        [ QC.elements [mk "2010-01-01T00:00:00Z"
        , mk "2013-05-27T19:13:50Z"
        , mk "2011-07-20T22:04:00Z"
        , mk "2012-02-02T15:45:11Z"
        , mk "2009-11-12T20:57:54Z"
        , mk "2000-10-28T21:03:24Z"
        , mk "1965-03-10T09:23:01Z"
        ]]
      where
        mk = unsafeParseUTC
