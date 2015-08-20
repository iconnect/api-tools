module Data.API.JSONToCBOR
    ( jsonToCBOR
    ) where

import           Data.API.JSON
import           Data.API.Types

import           Data.Aeson
import           Data.Binary.Serialise.CBOR
import           Data.Binary.Serialise.CBOR.Aeson ()
import           Data.Binary.Serialise.CBOR.Term

jsonToCBOR :: API -> TypeName -> Value -> Either JSONError Term
jsonToCBOR _ _ v = Right (deserialise (serialise v))
