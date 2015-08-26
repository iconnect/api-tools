module Data.API.JSONToCBOR
    ( serialiseJSONWithSchema
    , deserialiseJSONWithSchema
    , postprocessJSON
      -- * Encoding utilities
    , encodeRecord
    , encodeUnion
    ) where

import           Data.API.Changes
import           Data.API.JSON
import           Data.API.Types
import           Data.API.Utils

import           Control.Applicative
import           Data.Aeson hiding (encode)
import qualified Data.ByteString.Base64         as B64
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.HashMap.Strict            as HMap
import qualified Data.Map                       as Map
import           Data.Monoid
import           Data.Traversable
import qualified Data.Vector                    as Vec
import           Data.Binary.Serialise.CBOR     as CBOR
import           Data.Binary.Serialise.CBOR.JSON (cborToJson, encodeJSON)
import           Data.Binary.Serialise.CBOR.Encoding
import           Data.Scientific
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE

{-
-- TODO this should be in binary-serialise-cbor
instance Serialise Encoding where
  encode = id
  decode = error "can't decode Encoding"
-}

-- | Convert a JSON value into a CBOR term in a generic but
-- schema-dependent fashion.  This is necessary because the JSON
-- representation carries less information than we need in CBOR
-- (e.g. it lacks a distinction between bytestrings and text).
serialiseJSONWithSchema :: API -> TypeName -> Value -> Encoding
serialiseJSONWithSchema api = jsonToCBORTypeName (apiNormalForm api)

jsonToCBORTypeName :: NormAPI -> TypeName -> Value -> Encoding
jsonToCBORTypeName napi tn v =
    case Map.lookup tn napi of
      Just (NRecordType nrt) -> jsonToCBORRecord napi nrt v
      Just (NUnionType  nut) -> jsonToCBORUnion  napi nut v
      Just (NEnumType   net) -> jsonToCBOREnum   napi net v
      Just (NTypeSynonym ty) -> jsonToCBORType   napi ty  v
      Just (NNewtype     bt) -> jsonToCBORBasic       bt  v
      Nothing                -> error $ "serialiseJSONWithSchema: missing definition for type " ++ _TypeName tn

jsonToCBORType :: NormAPI -> APIType -> Value -> Encoding
jsonToCBORType napi ty0 v = case (ty0, v) of
    (TyList  ty, Array arr) -> encode $ map (jsonToCBORType napi ty) (Vec.toList arr)
    (TyList  _ , _)         -> error "serialiseJSONWithSchema: expected array"
    (TyMaybe _ , Null)      -> encode (Nothing :: Maybe ())
    (TyMaybe ty, _)         -> encode . Just $ jsonToCBORType napi ty v
    (TyName  tn, _)         -> jsonToCBORTypeName napi tn v
    (TyBasic bt, _)         -> jsonToCBORBasic bt v
    (TyJSON    , _)         -> encodeJSON v

-- | Encode a record as a map from field names to values.  Crucially,
-- the fields are in ascending order by field name.
jsonToCBORRecord :: NormAPI -> NormRecordType -> Value -> Encoding
jsonToCBORRecord napi nrt v = case v of
    Object hm -> encodeRecord $ map (f hm) $ Map.toAscList nrt
    _         -> error "serialiseJSONWithSchema: expected object"
  where
    f hm (fn, ty) = case HMap.lookup t hm of
                      Nothing -> error $ "serialiseJSONWithSchema: missing field " ++ _FieldName fn
                      Just v' -> (t, jsonToCBORType napi ty v')
      where
        t = T.pack $ _FieldName fn

-- | Encode a union as a single-element map from the field name to the value.
jsonToCBORUnion :: NormAPI -> NormUnionType -> Value -> Encoding
jsonToCBORUnion napi nut v = case v of
    Object hm | [(k, r)] <- HMap.toList hm -> case Map.lookup (FieldName $ T.unpack k) nut of
       Just ty -> encodeUnion k $ jsonToCBORType napi ty r
       Nothing -> error "serialiseJSONWithSchema: unexpected alternative in union"
    _ -> error "serialiseJSONWithSchema: expected single-field object"

-- | Encode an enumerated value as its name; we do not check that it
-- actually belongs to the type here.
jsonToCBOREnum :: NormAPI -> NormEnumType -> Value -> Encoding
jsonToCBOREnum _ _ v = case v of
                         String t -> encode t
                         _        -> error "serialiseJSONWithSchema: expected string"

jsonToCBORBasic :: BasicType -> Value -> Encoding
jsonToCBORBasic bt v = case (bt, v) of
    (BTstring, String t) -> encode t
    (BTstring, _)        -> error "serialiseJSONWithSchema: expected string"
    (BTbinary, String t) -> case B64.decode $ TE.encodeUtf8 t of
                              Left  err-> error $ "serialiseJSONWithSchema: base64-decoding failed: " ++ err
                              Right bs -> encode bs
    (BTbinary, _)        -> error "serialiseJSONWithSchema: expected string"
    (BTbool  , Bool b)   -> encode b
    (BTbool  , _)        -> error "serialiseJSONWithSchema: expected bool"
    (BTint   , Number n) | Right i <- (floatingOrInteger n :: Either Double Int) -> encode i
    (BTint   , _)        -> error "serialiseJSONWithSchema: expected integer"
    (BTutc   , String t) -> encodeTag 0 <> encode t
    (BTutc   , _)        -> error "serialiseJSONWithSchema: expected string"


-- | Encode a record as a map from field names to values; the field
-- names must be in alphabetical order!
encodeRecord :: CBOR.Serialise a => [(T.Text, a)] -> Encoding
encodeRecord fs = encodeMapLen (fromIntegral (length fs)) <> mconcat (map encodeField fs)
  where
    encodeField (t, v) = encode t <> encode v

-- | Encode an element of a union as single-element map from a field
-- name to a value.
encodeUnion :: CBOR.Serialise a => T.Text -> a -> Encoding
encodeUnion t e = encodeMapLen 1 <> encodeString t <> CBOR.encode e


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
      NNewtype     _  -> pure v

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
    TyBasic _  -> pure v
    TyJSON     -> pure v

postprocessJSONRecord :: NormAPI -> NormRecordType -> Value -> Either ValueError Value
postprocessJSONRecord napi nrt v = case v of
    Object hm -> Object <$> HMap.traverseWithKey f hm
    _         -> Left $ JSONError $ expectedObject v
  where
    f t v' = do ty <- Map.lookup (FieldName $ T.unpack t) nrt ?! JSONError UnexpectedField
                postprocessJSONType napi ty v'

postprocessJSONUnion :: NormAPI -> NormUnionType -> Value -> Either ValueError Value
postprocessJSONUnion napi nut v = case v of
    Object hm | [(k, r)] <- HMap.toList hm
              , Just ty <- Map.lookup (FieldName $ T.unpack k) nut
              -> Object . HMap.singleton k <$> postprocessJSONType napi ty r
    _ -> Left $ JSONError $ expectedObject v
