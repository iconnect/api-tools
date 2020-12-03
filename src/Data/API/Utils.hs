{-# LANGUAGE CPP #-}
module Data.API.Utils
    ( simpleParseVersion
    , mkUTC
    , mkUTC'
    , mkUTC_
    , parseUTC'
    , parseUTC_
    , (?!)
    , (?!?)
      -- * Utils for merging and diffing maps
    , MergeResult(..)
    , mergeMaps
    , diffMaps
    , matchMaps
    ) where

import           Data.Aeson
import           Data.Map ( Map )
import qualified Data.Map                       as Map
import           Data.Maybe
import qualified Data.Text                      as T
import           Data.Time
import           Data.Version
import qualified Text.ParserCombinators.ReadP as ReadP

#if MIN_VERSION_time(1,5,0)
#else
import           System.Locale (defaultTimeLocale)
#endif

simpleParseVersion :: String -> Maybe Version
simpleParseVersion s = case filter (null . snd) (ReadP.readP_to_S parseVersion s) of
  [(v,_)] -> Just v
  x       -> Nothing


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
            map (\fmt->parseTimeM True defaultTimeLocale fmt s) utcFormats


-- | The \"oh noes!\" operator.
--
(?!) :: Maybe a -> e -> Either e a
Nothing ?! e = Left  e
Just x  ?! _ = Right x

(?!?) :: Either e a -> (e -> e') -> Either e' a
Left  e ?!? f = Left  (f e)
Right x ?!? _ = Right x


-------------------------------------
-- Utils for merging and diffing maps
--

data MergeResult a b = OnlyInLeft a | InBoth a b | OnlyInRight b
  deriving (Eq, Show)

mergeMaps :: Ord k => Map k a -> Map k b -> Map k (MergeResult a b)
mergeMaps m1 m2 = Map.unionWith (\(OnlyInLeft a) (OnlyInRight b) -> InBoth a b)
                      (fmap OnlyInLeft m1) (fmap OnlyInRight m2)

diffMaps :: (Eq a, Ord k) => Map k a -> Map k a -> Map k (MergeResult a a)
diffMaps m1 m2 = Map.filter different $ mergeMaps m1 m2
  where
    different (InBoth a b) = a /= b
    different _            = True

-- | Attempts to match the keys of the maps to produce a map from keys
-- to pairs.
matchMaps :: Ord k => Map k a -> Map k b -> Either (k, Either a b) (Map k (a, b))
matchMaps m1 m2 = Map.traverseWithKey win $ mergeMaps m1 m2
  where
    win _ (InBoth x y)    = return (x, y)
    win k (OnlyInLeft x)  = Left (k, Left x)
    win k (OnlyInRight x) = Left (k, Right x)
