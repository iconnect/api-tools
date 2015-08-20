module Data.API.JSONToCBOR
    ( jsonToCBOR
    ) where

import           Data.API.Changes
import           Data.API.JSON
import           Data.API.NormalForm
import           Data.API.Types
import           Data.API.Utils

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Base64         as B64
import qualified Data.HashMap.Strict            as HMap
import qualified Data.Map                       as Map
import           Data.Traversable
import qualified Data.Vector                    as Vec
import           Data.Binary.Serialise.CBOR     as CBOR
import           Data.Binary.Serialise.CBOR.Aeson ()
import           Data.Binary.Serialise.CBOR.Term
import           Data.Scientific
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE

-- | Convert a JSON value into a CBOR term in a generic but
-- schema-dependent fashion.  This is necessary because the JSON
-- representation carries less information than we need in CBOR
-- (e.g. it lacks a distinction between bytestrings and text).
jsonToCBOR :: API -> TypeName -> Value -> Either ValueError Term
jsonToCBOR api = jsonToCBORTypeName (apiNormalForm api)

jsonToCBORTypeName :: NormAPI -> TypeName -> Value -> Either ValueError Term
jsonToCBORTypeName napi tn v = do
    t <- Map.lookup tn napi ?! InvalidAPI (TypeDoesNotExist tn)
    case t of
      NRecordType nrt -> jsonToCBORRecord napi nrt v
      NUnionType  nut -> jsonToCBORUnion  napi nut v
      NEnumType   net -> jsonToCBOREnum   napi net v
      NTypeSynonym ty -> jsonToCBORType   napi ty  v
      NNewtype     bt -> jsonToCBORBasic  napi bt  v

jsonToCBORType :: NormAPI -> APIType -> Value -> Either ValueError Term
jsonToCBORType napi ty v = case (ty, v) of
    (TyList ty, Array arr) -> TListI <$> traverse (jsonToCBORType napi ty) (Vec.toList arr)
    (TyList _, _)          -> Left $ JSONError $ expectedArray v
    (TyMaybe _, Null)      -> pure $ TList []
    (TyMaybe ty, v)        -> TList . pure <$> jsonToCBORType napi ty v
    (TyName tn, _)         -> jsonToCBORTypeName napi tn v
    (TyBasic bt, _)        -> jsonToCBORBasic napi bt v
    (TyJSON, _)            -> pure $ CBOR.deserialise $ CBOR.serialise v -- TODO

-- | Encode a record as a map from field names to values.  Crucially,
-- the fields are in ascending order by field name.
jsonToCBORRecord :: NormAPI -> NormRecordType -> Value -> Either ValueError Term
jsonToCBORRecord napi nrt (Object hm) = TMap <$> traverse f (Map.toAscList nrt)
  where
    f (fn, ty) = do let t = T.pack $ _FieldName fn
                    v <- HMap.lookup t hm ?! JSONError MissingField
                    (,) (TString t) <$> jsonToCBORType napi ty v
jsonToCBORRecord _ _ v = Left $ JSONError $ expectedObject v

-- | Encode a union as a single-element map from the field name to the value.
jsonToCBORUnion :: NormAPI -> NormUnionType -> Value -> Either ValueError Term
jsonToCBORUnion napi nut v = case v of
    Object hm | [(k, r)] <- HMap.toList hm
              , Just ty <- Map.lookup (FieldName $ T.unpack k) nut
              -> TMap . pure . ((,) (TString k)) <$> jsonToCBORType napi ty r
    _ -> Left $ JSONError $ expectedObject v

-- | Encode an enumerated value as its name; we do not check that it
-- actually belongs to the type here.
jsonToCBOREnum :: NormAPI -> NormEnumType -> Value -> Either ValueError Term
jsonToCBOREnum _ _ v = case v of
                         String t -> pure $ TString t
                         _        -> Left $ JSONError $ expectedString v

jsonToCBORBasic :: NormAPI -> BasicType -> Value -> Either ValueError Term
jsonToCBORBasic napi bt v = case (bt, v) of
    (BTstring, String t) -> pure $ TString t
    (BTstring, _)        -> Left $ JSONError $ expectedString v
    (BTbinary, String t) -> case B64.decode $ TE.encodeUtf8 t of
                              Left  _  -> Left $ JSONError $ BadFormat FmtBinary "binary" t
                              Right bs -> pure $ TBytes bs
    (BTbinary, _)        -> Left $ JSONError $ expectedString v
    (BTbool  , Bool b)   -> pure $ TBool b
    (BTbool  , _)        -> Left $ JSONError $ expectedBool v
    (BTint   , Number n) -> case floatingOrInteger n of
                              Right i -> pure $ TInt i
                              Left  _ -> Left $ JSONError $ expectedInt v
    (BTint   , _)        -> Left $ JSONError $ expectedInt v
    (BTutc   , String t) -> do u <- parseUTC' t ?! JSONError (BadFormat FmtUTC "utc" t)
                               pure $ CBOR.deserialise $ CBOR.serialise u -- TODO
    (BTutc   , _)        -> Left $ JSONError $ expectedString v
