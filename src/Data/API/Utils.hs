{-# LANGUAGE CPP #-}
module Data.API.Utils
    ( mkUTC
    , mkUTC'
    , mkUTC_
    , parseUTC'
    , parseUTC_
    , (?!)
    , (?!?)
    ) where

import           Data.Aeson
import           Data.Maybe
import qualified Data.Text                      as T
import           Data.Time

#if MIN_VERSION_time(1,5,0)
#else
import           System.Locale (defaultTimeLocale)
#endif


mkUTC :: UTCTime -> Value
mkUTC = String . mkUTC'

utcFormat :: String
utcFormat =               "%Y-%m-%dT%H:%M:%SZ"

utcFormats :: [String]
utcFormats =
                        [ "%Y-%m-%dT%H:%M:%S%z"
                        , "%Y-%m-%dT%H:%M:%S%Z"
                        , "%Y-%m-%dT%H:%M%Z"
                        , "%Y-%m-%dT%H:%M:%S%QZ"
                        , utcFormat
                        ]

mkUTC' :: UTCTime -> T.Text
mkUTC' = T.pack . mkUTC_

mkUTC_ :: UTCTime -> String
mkUTC_ utct = formatTime defaultTimeLocale utcFormat utct

parseUTC' :: T.Text -> Maybe UTCTime
parseUTC' t = parseUTC_ $ T.unpack t

parseUTC_ :: String -> Maybe UTCTime
parseUTC_ s = listToMaybe $ catMaybes $
            map (\fmt->parseTime defaultTimeLocale fmt s) utcFormats


-- | The \"oh noes!\" operator.
--
(?!) :: Maybe a -> e -> Either e a
Nothing ?! e = Left  e
Just x  ?! _ = Right x

(?!?) :: Either e a -> (e -> e') -> Either e' a
Left  e ?!? f = Left  (f e)
Right x ?!? _ = Right x
