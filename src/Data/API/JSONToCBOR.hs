{-# LANGUAGE CPP #-}

module Data.API.JSONToCBOR
    ( jsonToCBOR
    , postprocessJSON
    , Direction(..)
    , encodeUnion
    , encodeUTCTime
    , encodeUTCTime'
    ) where

import           Data.API.Changes
import           Data.API.JSON
import           Data.API.Types
import           Data.API.Utils

import           Control.Applicative
import           Data.Aeson hiding (encode)
import qualified Data.ByteString.Base64         as B64
import qualified Data.HashMap.Strict            as HMap
import qualified Data.Map                       as Map
import           Data.Monoid
import           Data.Traversable
import qualified Data.Vector                    as Vec
import           Data.Binary.Serialise.CBOR     as CBOR
import           Data.Binary.Serialise.CBOR.JSON ()
import           Data.Binary.Serialise.CBOR.Encoding
import           Data.Scientific
import           Data.Time
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (formatTime, defaultTimeLocale)
#else
import Data.Time.Format (formatTime)
import System.Locale    (defaultTimeLocale)
#endif

-- TODO this should be in binary-serialise-cbor
instance Serialise Encoding where
  encode = id
  decode = error "can't decode Encoding"

-- | Convert a JSON value into a CBOR term in a generic but
-- schema-dependent fashion.  This is necessary because the JSON
-- representation carries less information than we need in CBOR
-- (e.g. it lacks a distinction between bytestrings and text).
jsonToCBOR :: API -> TypeName -> Value -> Either ValueError Encoding
jsonToCBOR api = jsonToCBORTypeName (apiNormalForm api)

jsonToCBORTypeName :: NormAPI -> TypeName -> Value -> Either ValueError Encoding
jsonToCBORTypeName napi tn v = do
    t <- Map.lookup tn napi ?! InvalidAPI (TypeDoesNotExist tn)
    case t of
      NRecordType nrt -> jsonToCBORRecord napi nrt v
      NUnionType  nut -> jsonToCBORUnion  napi nut v
      NEnumType   net -> jsonToCBOREnum   napi net v
      NTypeSynonym ty -> jsonToCBORType   napi ty  v
      NNewtype     bt -> jsonToCBORBasic       bt  v

jsonToCBORType :: NormAPI -> APIType -> Value -> Either ValueError Encoding
jsonToCBORType napi ty0 v = case (ty0, v) of
    (TyList  ty, Array arr) -> encode <$> traverse (jsonToCBORType napi ty) (Vec.toList arr)
    (TyList  _ , _)         -> Left $ JSONError $ expectedArray v
    (TyMaybe _ , Null)      -> pure $ encode (Nothing :: Maybe ())
    (TyMaybe ty, _)         -> encode . Just <$> jsonToCBORType napi ty v
    (TyName  tn, _)         -> jsonToCBORTypeName napi tn v
    (TyBasic bt, _)         -> jsonToCBORBasic bt v
    (TyJSON    , _)         -> pure $ encode v

-- | Encode a record as a map from field names to values.  Crucially,
-- the fields are in ascending order by field name.
jsonToCBORRecord :: NormAPI -> NormRecordType -> Value -> Either ValueError Encoding
jsonToCBORRecord napi nrt (Object hm) = encodeRecord <$> traverse f (Map.toAscList nrt)
  where
    f (fn, ty) = do let t = T.pack $ _FieldName fn
                    v <- HMap.lookup t hm ?! JSONError MissingField
                    (,) t <$> jsonToCBORType napi ty v
jsonToCBORRecord _ _ v = Left $ JSONError $ expectedObject v

encodeRecord :: CBOR.Serialise a => [(T.Text, a)] -> Encoding
encodeRecord fs = encodeMapLen (fromIntegral (length fs)) <> mconcat (map encodeField fs)
  where
    encodeField (t, v) = encode t <> encode v

-- | Encode a union as a single-element map from the field name to the value.
jsonToCBORUnion :: NormAPI -> NormUnionType -> Value -> Either ValueError Encoding
jsonToCBORUnion napi nut v = case v of
    Object hm | [(k, r)] <- HMap.toList hm
              , Just ty <- Map.lookup (FieldName $ T.unpack k) nut
              -> encodeUnion k <$> jsonToCBORType napi ty r
    _ -> Left $ JSONError $ expectedObject v

encodeUnion :: CBOR.Serialise a => T.Text -> a -> Encoding
encodeUnion t e = encodeMapLen 1 <> encodeString t <> CBOR.encode e


-- | Encode an enumerated value as its name; we do not check that it
-- actually belongs to the type here.
jsonToCBOREnum :: NormAPI -> NormEnumType -> Value -> Either ValueError Encoding
jsonToCBOREnum _ _ v = case v of
                         String t -> pure $ encode t
                         _        -> Left $ JSONError $ expectedString v

jsonToCBORBasic :: BasicType -> Value -> Either ValueError Encoding
jsonToCBORBasic bt v = case (bt, v) of
    (BTstring, String t) -> pure $ encode t
    (BTstring, _)        -> Left $ JSONError $ expectedString v
    (BTbinary, String t) -> case B64.decode $ TE.encodeUtf8 t of
                              Left  _  -> Left $ JSONError $ BadFormat FmtBinary "binary" t
                              Right bs -> pure $ encode bs
    (BTbinary, _)        -> Left $ JSONError $ expectedString v
    (BTbool  , Bool b)   -> pure $ encode b
    (BTbool  , _)        -> Left $ JSONError $ expectedBool v
    (BTint   , Number n) -> case floatingOrInteger n :: Either Double Int of
                              Right i -> pure $ encodeInt i
                              Left  _ -> Left $ JSONError $ expectedInt v
    (BTint   , _)        -> Left $ JSONError $ expectedInt v
    (BTutc   , String t) -> pure $ encodeTag 0 <> encode t -- encodeUTCTime <$> (parseUTC' t ?! JSONError (BadFormat FmtUTC "utc" t))
    (BTutc   , _)        -> Left $ JSONError $ expectedString v

-- | Encode a UTCTime to match Aeson
-- TODO: this is a pain, dubiously necessary and changes with Aeson versions
encodeUTCTime :: UTCTime -> Encoding
encodeUTCTime t = encodeTag 0 <> encode (formatTime defaultTimeLocale fmt t)
 where
   fmt = "%FT%T." ++ take 3 (formatTime defaultTimeLocale "%q" t) ++ "Z"

-- | Encode a UTCTime to match generated ToJSON instances (not the
-- same as 'encodeUTCTime', of course...)
encodeUTCTime' :: UTCTime -> Encoding
encodeUTCTime' t = encodeTag 0 <> encode (mkUTC' t)



data Direction = AesonToCBOR | CBORToAeson

-- | When a JSON value has been deserialised from CBOR, the
-- representation may need some modifications in order to match the
-- result of 'toJSON' on a Haskell datatype.  In particular, Aeson's
-- representation of 'Maybe' does not round-trip (because 'Nothing' is
-- encoded as 'Null' and @'Just' x@ as @'toJSON' x@), so CBOR uses a
-- different representation (as an empty or 1-element list).
postprocessJSON :: Direction -> API -> TypeName -> Value -> Either ValueError Value
postprocessJSON dir api = postprocessJSONTypeName dir (apiNormalForm api)

postprocessJSONTypeName :: Direction -> NormAPI -> TypeName -> Value -> Either ValueError Value
postprocessJSONTypeName dir napi tn v = do
    t <- Map.lookup tn napi ?! InvalidAPI (TypeDoesNotExist tn)
    case t of
      NRecordType nrt -> postprocessJSONRecord dir napi nrt v
      NUnionType  nut -> postprocessJSONUnion  dir napi nut v
      NEnumType    _  -> pure v
      NTypeSynonym ty -> postprocessJSONType   dir napi ty  v
      NNewtype     _  -> pure v

postprocessJSONType :: Direction -> NormAPI -> APIType -> Value -> Either ValueError Value
postprocessJSONType dir napi ty0 v = case ty0 of
    TyList ty  -> case v of
                   Array arr -> Array <$> traverse (postprocessJSONType dir napi ty) arr
                   _         -> Left $ JSONError $ expectedArray v
    TyMaybe ty -> case (dir, v) of
                    (CBORToAeson, Array arr) -> case Vec.toList arr of
                                                  []    -> pure Null
                                                  [v1]  -> postprocessJSONType dir napi ty v1
                                                  _:_:_ -> Left $ JSONError $ SyntaxError "over-long array when converting Maybe value"
                    (CBORToAeson, _)         -> Left $ JSONError $ expectedArray v
                    (AesonToCBOR, Null)      -> pure $ Array $ Vec.empty
                    (AesonToCBOR, v)         -> pure $ Array $ Vec.singleton v
    TyName tn  -> postprocessJSONTypeName dir napi tn v
    TyBasic _  -> pure v
    TyJSON     -> pure v

postprocessJSONRecord :: Direction -> NormAPI -> NormRecordType -> Value -> Either ValueError Value
postprocessJSONRecord dir napi nrt v = case v of
    Object hm -> Object <$> HMap.traverseWithKey f hm
    _         -> Left $ JSONError $ expectedObject v
  where
    f t v = do ty <- Map.lookup (FieldName $ T.unpack t) nrt ?! JSONError UnexpectedField
               postprocessJSONType dir napi ty v

postprocessJSONUnion :: Direction -> NormAPI -> NormUnionType -> Value -> Either ValueError Value
postprocessJSONUnion dir napi nut v = case v of
    Object hm | [(k, r)] <- HMap.toList hm
              , Just ty <- Map.lookup (FieldName $ T.unpack k) nut
              -> Object . HMap.singleton k <$> postprocessJSONType dir napi ty r
    _ -> Left $ JSONError $ expectedObject v
