{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.API.JSONToCBOR
    ( serialiseJSONWithSchema
    , jsonToCBORWithSchema
    , deserialiseJSONWithSchema
    , postprocessJSON
    ) where

import           Data.API.Changes
import           Data.API.JSON
import           Data.API.JSON.Compat
import           Data.API.Time
import           Data.API.Types
import           Data.API.Utils

import           Control.Applicative
import           Data.Aeson hiding (encode)
import qualified Data.ByteString.Base64         as B64
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map                       as Map
import           Data.Traversable
import qualified Data.Vector                    as Vec
import           Codec.Serialise     as CBOR
import           Data.Binary.Serialise.CBOR.JSON (cborToJson, jsonToCbor)
import           Codec.CBOR.Term
import           Data.Fixed (Pico)
import           Data.Scientific
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           Data.Time.Clock.POSIX
import           Data.Time (UTCTime(UTCTime))
import           Prelude


-- | Serialise a JSON value as a CBOR term in a generic but
-- schema-dependent fashion.  This is necessary because the JSON
-- representation carries less information than we need in CBOR
-- (e.g. it lacks a distinction between bytestrings and text).
--
-- There is a corner case where this may lose information: if the schema
-- contains a field with type @? json@ (i.e. @'Maybe' 'Value'@), then we have
--
-- > toJSON Nothing     == Null
-- > toJSON (Just Null) == Null
--
-- so 'serialiseJSONWithSchema' cannot distinguish these values, and will use
-- the CBOR-encoding of 'Nothing' for both.
--
serialiseJSONWithSchema :: API -> TypeName -> Value -> LBS.ByteString
serialiseJSONWithSchema api tn v = serialise $ jsonToCBORWithSchema api tn v

-- | Convert a JSON value into a CBOR term in a generic but
-- schema-dependent fashion.
jsonToCBORWithSchema :: API -> TypeName -> Value -> Term
jsonToCBORWithSchema = jsonToCBORTypeName . apiNormalForm

jsonToCBORTypeName :: NormAPI -> TypeName -> Value -> Term
jsonToCBORTypeName napi tn v =
    case Map.lookup tn napi of
      Just (NRecordType nrt) -> jsonToCBORRecord napi nrt v
      Just (NUnionType  nut) -> jsonToCBORUnion  napi nut v
      Just (NEnumType   net) -> jsonToCBOREnum   napi net v
      Just (NTypeSynonym ty) -> jsonToCBORType   napi ty  v
      Just (NNewtype     bt) -> jsonToCBORBasic       bt  v
      Nothing                -> error $ "serialiseJSONWithSchema: missing definition for type "
                                          ++ T.unpack (_TypeName tn)

jsonToCBORType :: NormAPI -> APIType -> Value -> Term
jsonToCBORType napi ty0 v = case (ty0, v) of
    (TyList  ty, Array arr) | Vec.null arr -> TList []
                            | otherwise    -> TListI $ map (jsonToCBORType napi ty) (Vec.toList arr)
    (TyList  _ , _)         -> error "serialiseJSONWithSchema: expected array"
    (TyMaybe _ , Null)      -> TList []
    (TyMaybe ty, _)         -> TList [jsonToCBORType napi ty v]
    (TyName  tn, _)         -> jsonToCBORTypeName napi tn v
    (TyBasic bt, _)         -> jsonToCBORBasic bt v
    (TyJSON    , _)         -> jsonToCbor v

-- | Encode a record as a map from field names to values.  Crucially,
-- the fields are in ascending order by field name.
jsonToCBORRecord :: NormAPI -> NormRecordType -> Value -> Term
jsonToCBORRecord napi nrt v = case v of
    Object hm -> TMap $ map (f hm) $ Map.toAscList nrt
    _         -> error "serialiseJSONWithSchema: expected object"
  where
    f hm (fn, ty) = case lookupKey (_FieldName fn) hm of
                      Nothing -> error $ "serialiseJSONWithSchema: missing field " ++ T.unpack (_FieldName fn)
                      Just v' -> (TString (_FieldName fn), jsonToCBORType napi ty v')

-- | Encode a union as a single-element map from the field name to the value.
jsonToCBORUnion :: NormAPI -> NormUnionType -> Value -> Term
jsonToCBORUnion napi nut v = case v of
    Object hm | Just (k, r) <- matchSingletonObject hm -> case Map.lookup (FieldName k) nut of
       Just ty -> TMap [(TString k, jsonToCBORType napi ty r)]
       Nothing -> error "serialiseJSONWithSchema: unexpected alternative in union"
    _ -> error "serialiseJSONWithSchema: expected single-field object"

-- | Encode an enumerated value as its name; we do not check that it
-- actually belongs to the type here.
jsonToCBOREnum :: NormAPI -> NormEnumType -> Value -> Term
jsonToCBOREnum _ _ v = case v of
                         String t -> TString t
                         _        -> error "serialiseJSONWithSchema: expected string"

jsonToCBORBasic :: BasicType -> Value -> Term
jsonToCBORBasic bt v = case (bt, v) of
    (BTstring, String t) -> TString t
    (BTstring, _)        -> error "serialiseJSONWithSchema: expected string"
    (BTbinary, String t) -> case B64.decode $ TE.encodeUtf8 t of
                              Left  err-> error $ "serialiseJSONWithSchema: base64-decoding failed: " ++ err
                              Right bs -> TBytes bs
    (BTbinary, _)        -> error "serialiseJSONWithSchema: expected string"
    (BTbool  , Bool b)   -> TBool b
    (BTbool  , _)        -> error "serialiseJSONWithSchema: expected bool"
    (BTint   , Number n) | Right i <- (floatingOrInteger n :: Either Double Int) -> TInt i
    (BTint   , _)        -> error "serialiseJSONWithSchema: expected integer"
    (BTutc   , String t) ->
      TTagged 1000 (TMap [ (TInt 1, TInt secs)
                         , (TInt (-12), TInt psecs) ])
        where  -- taken from @Codec.Serialise.Class@:
          (secs, frac) = case properFraction $ utcTimeToPOSIXSeconds utc of
                           -- fractional part must be positive
                           (secs', frac')
                             | frac' < 0  -> (secs' - 1, frac' + 1)
                             | otherwise -> (secs', frac')
          psecs = round $ frac * 1000000000000
          utc = unsafeParseUTC t
    (BTutc   , _)        -> error "serialiseJSONWithSchema: expected string"


-- | When a JSON value has been deserialised from CBOR, the
-- representation may need some modifications in order to match the
-- result of 'toJSON' on a Haskell datatype.  In particular, Aeson's
-- representation of 'Maybe' does not round-trip (because 'Nothing' is
-- encoded as 'Null' and @'Just' x@ as @'toJSON' x@), so CBOR uses a
-- different representation (as an empty or 1-element list).
deserialiseJSONWithSchema :: API -> TypeName -> LBS.ByteString -> Value
deserialiseJSONWithSchema api tn bs = case postprocessJSON api tn (cborToJson (deserialise bs)) of
    Right v  -> v
    Left err -> error $ "deserialiseJSONWithSchema could not post-process: " ++ prettyValueError err

postprocessJSON :: API -> TypeName -> Value -> Either ValueError Value
postprocessJSON api = postprocessJSONTypeName (apiNormalForm api)

postprocessJSONTypeName :: NormAPI -> TypeName -> Value -> Either ValueError Value
postprocessJSONTypeName napi tn v = do
    t <- Map.lookup tn napi ?! InvalidAPI (TypeDoesNotExist tn)
    case t of
      NRecordType nrt -> postprocessJSONRecord napi nrt v
      NUnionType  nut -> postprocessJSONUnion  napi nut v
      NEnumType    _  -> pure v
      NTypeSynonym ty -> postprocessJSONType   napi ty  v
      NNewtype     bt -> postprocessJSONType   napi (TyBasic bt) v

postprocessJSONType :: NormAPI -> APIType -> Value -> Either ValueError Value
postprocessJSONType napi ty0 v = case ty0 of
    TyList ty  -> case v of
                   Array arr -> Array <$> traverse (postprocessJSONType napi ty) arr
                   _         -> Left $ JSONError $ expectedArray v
    TyMaybe ty -> case v of
                    Array arr -> case Vec.toList arr of
                                   []    -> pure Null
                                   [v1]  -> postprocessJSONType napi ty v1
                                   _:_:_ -> Left $ JSONError $ SyntaxError "over-long array when converting Maybe value"
                    _         -> Left $ JSONError $ expectedArray v
    TyName tn  -> postprocessJSONTypeName napi tn v
    TyBasic BTutc -> case v of
      Object obj
        | Just (Number v0) <- lookupKey "1" obj
        , Just (Number v1) <- lookupKey "-12" obj ->
          -- Taken from @Codec.Serialise.Class@:
          let psecs :: Pico
              psecs = realToFrac v1 / 1000000000000

              dt :: POSIXTime
              dt = realToFrac v0 + realToFrac psecs

          in pure $! String $! printUTC $! forceUTCTime (posixSecondsToUTCTime dt)
        | otherwise -> Left $ JSONError (Expected ExpObject "UTCTime" v)
      String t -> case parseUTC t of
        Nothing -> Left $ JSONError $ SyntaxError $
                     "UTC time in wrong format: " ++ T.unpack t
        Just utcTime -> pure $! String $! printUTC $! forceUTCTime utcTime
      _ -> Left $ JSONError $ expectedObject v
    TyBasic _  -> pure v
    TyJSON     -> pure v

-- | Force the unnecessarily lazy @'UTCTime'@ representation.
forceUTCTime :: UTCTime -> UTCTime
forceUTCTime t@(UTCTime !_day !_daytime) = t

postprocessJSONRecord :: NormAPI -> NormRecordType -> Value -> Either ValueError Value
postprocessJSONRecord napi nrt v = case v of
    Object hm -> Object <$> traverseObjectWithKey f hm
    _         -> Left $ JSONError $ expectedObject v
  where
    f t v' = do ty <- Map.lookup (FieldName t) nrt ?! JSONError MissingField
                postprocessJSONType napi ty v'

postprocessJSONUnion :: NormAPI -> NormUnionType -> Value -> Either ValueError Value
postprocessJSONUnion napi nut v = case v of
    Object hm | Just (k, r) <- matchSingletonObject hm
              , Just ty <- Map.lookup (FieldName k) nut
              -> Object . singletonObject k <$> postprocessJSONType napi ty r
    _ -> Left $ JSONError $ expectedObject v
