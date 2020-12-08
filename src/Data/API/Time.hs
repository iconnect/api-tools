module Data.API.Time
    ( printUTC
    , parseUTC
    , unsafeParseUTC
    ) where

import           Data.Maybe
import qualified Data.Text                      as T
import           Data.Time

import           GHC.Stack

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
                        ]

-- | Render a 'UTCTime' in ISO 8601 format to a precision of seconds
-- (i.e. omitting any subseconds).
printUTC :: UTCTime -> T.Text
printUTC utct = T.pack $ formatTime defaultTimeLocale utcFormat utct

-- | Parse text as a 'UTCTime' in ISO 8601 format or a number of slight
-- variations thereof (the @T@ may be replaced with a space, and the seconds and
-- timezone indicator may optionally be omitted).
parseUTC :: T.Text -> Maybe UTCTime
parseUTC t = stringToUTC $ T.unpack t

stringToUTC :: String -> Maybe UTCTime
stringToUTC s = listToMaybe $ catMaybes $
            map (\fmt->parseTimeM True defaultTimeLocale fmt s) utcFormats

-- | Variant of 'parseUTC' that throws an error if the input text could not be
-- parsed.
unsafeParseUTC :: HasCallStack => T.Text -> UTCTime
unsafeParseUTC t = fromMaybe (error msg) (parseUTC t)
  where
    msg = "unsafeParseUTC: unable to parse: " ++ T.unpack t
