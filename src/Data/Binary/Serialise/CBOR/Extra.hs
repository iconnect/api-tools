{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Binary.Serialise.CBOR.Extra
    ( encodeListWith
    , encodeSetLikeWith
    , encodeMaybeWith
    , encodeRecordFields
    , encodeUnion
    , decodeUnion
    , decodeListWith
    , decodeSetLikeWith
    , decodeMaybeWith
    , (<$!>)
    , cborSetTag
    , pattern TSetI
    , pattern JSONCBORSet
    ) where

import           Codec.CBOR.Decoding (decodeListLenCanonical)
import           Codec.CBOR.Term
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding
import           Data.Word
import qualified Data.Aeson                     as JS
import qualified Data.Text                      as T
import qualified Data.Vector                    as V

#if MIN_VERSION_base(4,8,0)
import           Control.Monad ((<$!>), when)
#else
-- | Strict version of '<$>', which is available in base >= 4.8.0
(<$!>) :: Monad m => (a -> b) -> m a -> m b
{-# INLINE (<$!>) #-}
f <$!> m = do
  x <- m
  let z = f x
  z `seq` return z
infixl 4 <$!>
#endif

encodeListWith :: (a -> Encoding) -> [a] -> Encoding
encodeListWith _ [] = encodeListLen 0
encodeListWith f xs = encodeListLenIndef
                        <> foldr (\x r -> f x <> r) encodeBreak xs

encodeMaybeWith :: (a -> Encoding) -> Maybe a -> Encoding
encodeMaybeWith _ Nothing  = encodeListLen 0
encodeMaybeWith f (Just x) = encodeListLen 1 <> f x

-- We can assume the record has at least 1 field.
encodeRecordFields :: [Encoding] -> Encoding
encodeRecordFields = mconcat

-- | Encode an element of a union as single-element map from a field
-- name to a value.
encodeUnion :: T.Text -> Encoding -> Encoding
encodeUnion t e = encodeMapLen 1 <> encodeString t <> e

decodeUnion :: [(T.Text, Decoder s a)] -> Decoder s a
decodeUnion ds = do
    _   <- decodeMapLen -- should always be 1
    dfn <- decodeString
    case lookup dfn ds of
      Nothing -> fail "Unexpected field in union in CBOR"
      Just d -> d

decodeListWith :: Decoder s a -> Decoder s [a]
decodeListWith dec = do
    mn <- decodeListLenOrIndef
    case mn of
      Nothing -> decodeSequenceLenIndef (flip (:)) [] reverse   dec
      Just n  -> decodeSequenceLenN     (flip (:)) [] reverse n dec

decodeMaybeWith :: Decoder s a -> Decoder s (Maybe a)
decodeMaybeWith dec = do
    n <- decodeListLen
    case n of
      0 -> return Nothing
      1 -> do !x <- dec
              return (Just x)
      _ -> fail "unknown tag"

--
-- Encoding and decoding sets
--

encodeSetLikeWith :: (a -> Encoding) -> [a] -> Encoding
encodeSetLikeWith f xs = encodeSetSkel f length foldr xs


-- | This is the tag for the CBOR_SETS specification as defined
-- [here](https://github.com/input-output-hk/cbor-sets-spec/blob/master/CBOR_SETS.md) and
-- approved by IANA [here](https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml).
cborSetTag :: Word64
cborSetTag = 258

encodeSetTag :: Encoding
encodeSetTag = encodeTag64 cborSetTag

encodeSetSkel
  :: (a -> Encoding)
  -> (s -> Int)
  -> ((a -> Encoding -> Encoding) -> Encoding -> s -> Encoding)
  -> s
  -> Encoding
encodeSetSkel encode size foldFunction = mappend encodeSetTag . encodeContainerSkel
  encodeListLen
  size
  foldFunction
  (\a b -> encode a <> b)
{-# INLINE encodeSetSkel #-}

encodeContainerSkel :: (Word -> Encoding)
                    -> (container -> Int)
                    -> (accumFunc -> Encoding -> container -> Encoding)
                    -> accumFunc
                    -> container
                    -> Encoding
encodeContainerSkel encodeLen size foldFunction f  c =
    encodeLen (fromIntegral (size c)) <> foldFunction f mempty c
{-# INLINE encodeContainerSkel #-}

-- | It's not a 'Set' because we need to use this in the JSON conversion,
-- and a 'Value' doesn't have an 'Ord' instance.
decodeSetLikeWith :: Decoder s a -> Decoder s [a]
decodeSetLikeWith dec = decodeSetSkel dec id

-- | Cribbed from 'cardano-base' and 'cardano-sl', functions that I (adinapoli)
-- wrote eons ago anyway.
decodeSetTag :: Decoder s ()
decodeSetTag = do
  t <- decodeTag64
  when (t /= cborSetTag) $ cborError $ ("decodeSetTag: this doesn't appear to be a Set. Found tag: " <> show t)

cborError :: String -> Decoder s a
cborError = toCborError . Left
  where
    toCborError :: Either String a -> Decoder s a
    toCborError = either fail return

decodeSetSkel :: forall s a. Decoder s a -> ([a] -> [a]) -> Decoder s [a]
decodeSetSkel decode fromDistinctAscList = do
    decodeSetTag
    n <- decodeListLenCanonical
    case n of
        0 -> return (fromDistinctAscList [])
        _ -> do
            firstValue <- decode
            decodeEntries (n - 1) firstValue [firstValue]
  where
    decodeEntries :: Int -> a -> [a] -> Decoder s [a]
    decodeEntries 0 _ acc = pure $ reverse acc
    decodeEntries !remainingEntries _previousValue !acc = do
        newValue <- decode
        decodeEntries (remainingEntries - 1) newValue (newValue : acc)
{-# INLINE decodeSetSkel #-}

-- | This pattern uses the CBOR_SETS specification as defined
-- [here](https://github.com/input-output-hk/cbor-sets-spec/blob/master/CBOR_SETS.md) and
-- approved by IANA [here](https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml).
pattern TSetI :: [Term] -> Term
pattern TSetI xs <- TTagged 258 (TListI xs) where
  TSetI xs = TTagged cborSetTag (TListI xs)

pattern JSONCBORSet :: [JS.Value] -> JS.Value
pattern JSONCBORSet xs <- JS.Array (V.toList -> xs) where
  JSONCBORSet xs = JS.object [ "_api_tools_set_tag" JS..= JS.toJSON cborSetTag
                             , "_api_tools_set_values" JS..= JS.toJSON xs
                             ]
