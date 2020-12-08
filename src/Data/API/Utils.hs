module Data.API.Utils
    ( simpleParseVersion
    , (?!)
    , (?!?)
      -- * Utils for merging and diffing maps
    , MergeResult(..)
    , mergeMaps
    , diffMaps
    , matchMaps
    ) where

import           Data.Map ( Map )
import qualified Data.Map                       as Map
import           Data.Version
import qualified Text.ParserCombinators.ReadP as ReadP

simpleParseVersion :: String -> Maybe Version
simpleParseVersion s = case filter (null . snd) (ReadP.readP_to_S parseVersion s) of
  [(v,_)] -> Just v
  _       -> Nothing


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
