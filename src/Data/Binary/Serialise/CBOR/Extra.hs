{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module Data.Binary.Serialise.CBOR.Extra
    ( encodeListWith
    , encodeMaybeWith
    , encodeRecordFields
    , encodeUnion
    , decodeUnion
    , decodeListWith
    , decodeMaybeWith
    , (<$!>)
    ) where

import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding
import qualified Data.Text                      as T

#if MIN_VERSION_base(4,8,0)
import           Control.Monad ((<$!>))
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
