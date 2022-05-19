{-# LANGUAGE CPP #-}

-- | This module provides wrappers for compatibility between versions of @aeson@
-- prior to and after 2.0.  It is not necessarily stable.
--
module Data.API.JSON.Compat
    ( Key
    , KeyMap

    , lookupKey
    , listToObject
    , objectToList
    , matchSingletonObject
    , singletonObject
    , insertKey
    , deleteKey
    , objectToMap
    , mapToObject
    , traverseObjectWithKey
    , adjustObject

    , fieldNameToKey
    , keyToFieldName
    , textToKey
    , keyToText
    ) where

import           Data.API.Types

import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as T

#if MIN_VERSION_aeson(2,0,0)

import           Data.Aeson.Key (Key)
import qualified Data.Aeson.Key                 as Key
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap              as KeyMap

lookupKey :: T.Text -> KeyMap a -> Maybe a
lookupKey k m = KeyMap.lookup (Key.fromText k) m

listToObject :: [(T.Text, a)] -> KeyMap a
listToObject = KeyMap.fromList . map (\ (x, y) -> (Key.fromText x, y))

objectToList :: KeyMap a -> [(T.Text, a)]
objectToList = map (\ (x, y) -> (Key.toText x, y)) . KeyMap.toList

matchSingletonObject :: KeyMap a -> Maybe (T.Text, a)
matchSingletonObject km = case objectToList km of
                            [(k, v)] -> Just (k, v)
                            _        -> Nothing

singletonObject :: T.Text -> a -> KeyMap a
singletonObject k = KeyMap.singleton (Key.fromText k)

insertKey :: T.Text -> a -> KeyMap a -> KeyMap a
insertKey k = KeyMap.insert (Key.fromText k)

deleteKey :: T.Text -> KeyMap a -> KeyMap a
deleteKey k = KeyMap.delete (Key.fromText k)

objectToMap :: KeyMap a -> Map.Map T.Text a
objectToMap = KeyMap.toMapText

mapToObject :: Map.Map T.Text a -> KeyMap a
mapToObject = KeyMap.fromMapText

traverseObjectWithKey :: Applicative f => (T.Text -> v1 -> f v2) -> KeyMap v1 -> f (KeyMap v2)
traverseObjectWithKey f = KeyMap.traverseWithKey (\ k -> f (Key.toText k))

fieldNameToKey :: FieldName -> Key
fieldNameToKey = Key.fromText . _FieldName

textToKey :: T.Text -> Key
textToKey = Key.fromText

keyToFieldName :: Key -> FieldName
keyToFieldName = FieldName . Key.toText

keyToText :: Key -> T.Text
keyToText = Key.toText

adjustObject :: (v -> v) -> Key -> KeyMap v -> KeyMap v
adjustObject f k m = case KeyMap.lookup k m of
                       Nothing -> m
                       Just v  -> KeyMap.insert k (f v) m

#else

import qualified Data.HashMap.Strict as HMap

type Key = T.Text

type KeyMap = HMap.HashMap Key

lookupKey :: T.Text -> KeyMap a -> Maybe a
lookupKey = HMap.lookup

listToObject :: [(T.Text, a)] -> KeyMap a
listToObject = HMap.fromList

objectToList :: KeyMap a -> [(T.Text, a)]
objectToList = HMap.toList

matchSingletonObject :: KeyMap a -> Maybe (T.Text, a)
matchSingletonObject km = case objectToList km of
                            [(k, v)] -> Just (k, v)
                            _        -> Nothing

singletonObject :: T.Text -> a -> KeyMap a
singletonObject = HMap.singleton

insertKey :: T.Text -> a -> KeyMap a -> KeyMap a
insertKey = HMap.insert

deleteKey :: T.Text -> KeyMap a -> KeyMap a
deleteKey = HMap.delete

objectToMap :: KeyMap a -> Map.Map T.Text a
objectToMap = Map.fromList . HMap.toList

mapToObject :: Map.Map T.Text a -> KeyMap a
mapToObject = HMap.fromList . Map.toList

traverseObjectWithKey :: Applicative f => (T.Text -> v1 -> f v2) -> KeyMap v1 -> f (KeyMap v2)
traverseObjectWithKey = HMap.traverseWithKey

adjustObject :: (v -> v) -> Key -> KeyMap v -> KeyMap v
adjustObject = HMap.adjust

fieldNameToKey :: FieldName -> Key
fieldNameToKey = _FieldName

keyToFieldName :: Key -> FieldName
keyToFieldName = FieldName

textToKey :: T.Text -> Key
textToKey = id

keyToText :: Key -> T.Text
keyToText = id

#endif
