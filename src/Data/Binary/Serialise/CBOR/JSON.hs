{-# LANGUAGE BangPatterns #-}
module Data.Binary.Serialise.CBOR.JSON (
    cborToJson,
    jsonToCbor,
    encodeJSON,
  ) where

import qualified Data.Aeson          as JSON
import qualified Data.Scientific     as Scientific
import qualified Data.Vector         as Vec
import qualified Data.HashMap.Strict as HashMap

import           Data.Text (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text
import qualified Data.Text.Lazy                  as Text.Lazy
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Base64          as Base64
-- import qualified Data.ByteString.Base64.URL      as Base64url
import qualified Data.ByteString.Base16          as Base16

import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Term as CBOR
import Data.Binary.Serialise.CBOR

import Control.Applicative

instance Serialise JSON.Value where
  encode = encodeJSON
  decode = cborToJson <$> decode

encodeJSON :: JSON.Value -> Encoding
encodeJSON = encode . jsonToCbor

-- Most of the types in CBOR have direct analogs in JSON.  However, some
-- do not, and someone implementing a CBOR-to-JSON converter has to
-- consider what to do in those cases.  The following non-normative
-- advice deals with these by converting them to a single substitute
-- value, such as a JSON null.

cborToJson :: CBOR.Term -> JSON.Value

-- o  An integer (major type 0 or 1) becomes a JSON number.

-- We modify this advice by only converting numbers in the range -2^53 .. 2^53
-- and otherwise handling them like big nums

cborToJson (CBOR.TInt n) = cborToJson (CBOR.TInteger (fromIntegral n))

-- o  A byte string (major type 2) that is not embedded in a tag that
--    specifies a proposed encoding is encoded in base64url without
--    padding and becomes a JSON string.

cborToJson (CBOR.TBytes  bs) = JSON.String (base64url bs)
cborToJson (CBOR.TBytesI bs) = JSON.String (base64url (LBS.toStrict bs))

-- o  A UTF-8 string (major type 3) becomes a JSON string.  Note that
--    JSON requires escaping certain characters (RFC 4627, Section 2.5):
--    quotation mark (U+0022), reverse solidus (U+005C), and the "C0
--    control characters" (U+0000 through U+001F).  All other characters
--    are copied unchanged into the JSON UTF-8 string.

cborToJson (CBOR.TString  s) = JSON.String s -- aeson will escape correctly
cborToJson (CBOR.TStringI s) = JSON.String (Text.Lazy.toStrict s)

-- o  An array (major type 4) becomes a JSON array.

cborToJson (TList  vs) = JSON.Array (Vec.fromList (map cborToJson vs))

-- o  A map (major type 5) becomes a JSON object.  This is possible
--    directly only if all keys are UTF-8 strings.  A converter might
--    also convert other keys into UTF-8 strings (such as by converting
--    integers into strings containing their decimal representation);
--    however, doing so introduces a danger of key collision.

cborToJson (TMap  kvs) = JSON.object [ (cborToJsonString k, cborToJson v)
                                     | (k, v) <- kvs ]

-- o  False (major type 7, additional information 20) becomes a JSON false.

-- o  True (major type 7, additional information 21) becomes a JSON true.
--
-- o  Null (major type 7, additional information 22) becomes a JSON null.

cborToJson (TBool b) = JSON.Bool b
cborToJson  TNull    = JSON.Null

-- o  A floating-point value (major type 7, additional information 25
--    through 27) becomes a JSON number if it is finite (that is, it can
--    be represented in a JSON number); if the value is non-finite (NaN,
--    or positive or negative Infinity), it is represented by the
--    substitute value.

cborToJson (THalf f)
  | isNaN f || isInfinite f = JSON.Null
  | otherwise               = JSON.Number (Scientific.fromFloatDigits f)
cborToJson (TFloat f)
  | isNaN f || isInfinite f = JSON.Null
  | otherwise               = JSON.Number (Scientific.fromFloatDigits f)
cborToJson (TDouble f)
  | isNaN f || isInfinite f = JSON.Null
  | otherwise               = JSON.Number (Scientific.fromFloatDigits f)

-- o  Any other simple value (major type 7, any additional information
--    value not yet discussed) is represented by the substitute value.

cborToJson  TUndef     = JSON.Null
cborToJson (TSimple _) = JSON.Null

-- o  A bignum (major type 6, tag value 2 or 3) is represented by
--    encoding its byte string in base64url without padding and becomes
--    a JSON string.  For tag value 3 (negative bignum), a "~" (ASCII
--    tilde) is inserted before the base-encoded value.  (The conversion
--    to a binary blob instead of a number is to prevent a likely
--    numeric overflow for the JSON decoder.)

-- NOTE We ignore this advice and just use 'JSON.Number'.

cborToJson (TInteger n) = JSON.Number (fromInteger n)

-- o  A byte string with an encoding hint (major type 6, tag value 21
--    through 23) is encoded as described and becomes a JSON string.

cborToJson (TTagged 21 (CBOR.TBytes bs)) = JSON.String (base64url bs)
cborToJson (TTagged 22 (CBOR.TBytes bs)) = JSON.String (base64 bs)
cborToJson (TTagged 23 (CBOR.TBytes bs)) = JSON.String (base16 bs)

--   o  For all other tags (major type 6, any other tag value), the
--      embedded CBOR item is represented as a JSON value; the tag value
--      is ignored.

cborToJson (TTagged _tag term) = cborToJson term

-- o  Indefinite-length items are made definite before conversion.

cborToJson (TListI kvs) = cborToJson (TList kvs)
cborToJson (TMapI  kvs) = cborToJson (TMap kvs)


-- used just for converting CBOR terms to JSON map keys
-- TODO: partial
cborToJsonString :: CBOR.Term -> Text.Text
cborToJsonString (TInt     n) = Text.pack (show n)
cborToJsonString (TInteger n) = Text.pack (show n)
cborToJsonString (TString  s) = s
cborToJsonString (TStringI s) = Text.Lazy.toStrict s

cborToJsonString (TBytes  bs) = base64url bs
cborToJsonString (TBytesI bs) = base64url (LBS.toStrict bs)

-- TODO not strictly following the spec - this uses padding, spec says
-- we shouldn't
-- TODO moreover, api-tools uses base64 rather than base64url!
base64url :: ByteString -> Text
base64url = base64 -- Text.decodeLatin1 . Base64url.encode

base64 :: ByteString -> Text
base64 = Text.decodeLatin1 . Base64.encode

base16 :: ByteString -> Text
base16 = Text.decodeLatin1 . Base16.encode


jsonToCbor :: JSON.Value -> CBOR.Term
jsonToCbor (JSON.Object kvs) = CBOR.TMap [ (CBOR.TString k, jsonToCbor v)
                                         | (k, v) <- HashMap.toList kvs ]
jsonToCbor (JSON.Array  vs)  = CBOR.TList [ jsonToCbor v | v <- Vec.toList vs ]
jsonToCbor (JSON.String str) = CBOR.TString str
jsonToCbor (JSON.Number n)   = case Scientific.floatingOrInteger n of
                                 Left  d -> CBOR.TDouble d
                                 Right i
                                   | i >= fromIntegral (minBound :: Int) &&
                                     i <= fromIntegral (maxBound :: Int)
                                               -> CBOR.TInt (fromIntegral i)
                                   | otherwise -> CBOR.TInteger i
jsonToCbor (JSON.Bool   b)   = CBOR.TBool b
jsonToCbor  JSON.Null        = CBOR.TNull
