{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE BangPatterns #-}

-- Here we use Template Haskell to generate a test suite for the Aeson wrappers
-- from the DSL description of same.

module Data.API.Tools.JSONTests
    ( jsonTestsTool
    , jsonTestsToolCBOR
    , jsonTestsToolCBOR2
    , prop_decodesTo
    , prop_decodesTo'
    , prop_resultsMatchRoundtrip
    ) where

import           Data.API.JSON
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.TH
import           Data.API.Types

import qualified Data.Aeson                     as JS
import           Data.Binary.Serialise.CBOR
import qualified Data.ByteString.Lazy           as BS
import           Language.Haskell.TH
import           Test.QuickCheck

import System.IO.Unsafe (unsafePerformIO)
import System.Process.ByteString (readProcessWithExitCode)
import Data.Attoparsec.ByteString (parseOnly, endOfInput)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Control.Applicative ((<*))

-- | Tool to generate a list of tests of type @[('String', 'Property')]@
-- with the given name.  This depends on 'jsonTool' and 'quickCheckTool'.
jsonTestsTool :: Name -> APITool
jsonTestsTool nm = simpleTool $ \ api -> simpleSigD nm [t| [(String, Property)] |] (props api)
  where
    props api = listE $ map generateProp [ an | ThNode an <- api ]

-- | For an APINode, generate a (String, Property) pair giving the
-- type name and an appropriate instance of the
-- prop_resultsMatchRoundtrip property
generateProp :: APINode -> ExpQ
generateProp an = [e| ($ty, property (prop_resultsMatchRoundtrip :: $(nodeT an) -> Bool)) |]
  where
    ty = stringE $ _TypeName $ anName an

jsonTestsToolCBOR :: Name -> APITool
jsonTestsToolCBOR nm = simpleTool $ \ api -> simpleSigD nm [t| [(String, Property)] |] (props api)
  where
    props api = listE $ map generatePropCBOR [ an | ThNode an <- api ]

-- | For an APINode, generate a (String, Property) pair giving the
-- type name and an appropriate instance of the
-- prop_resultsMatchRoundtrip property
generatePropCBOR :: APINode -> ExpQ
generatePropCBOR an = [e| ($ty, property (prop_resultsMatchRoundtripCBOR :: $(nodeT an) -> Bool)) |]
  where
    ty = stringE $ _TypeName $ anName an


jsonTestsToolCBOR2 :: Name -> APITool
jsonTestsToolCBOR2 nm = simpleTool $ \ api -> simpleSigD nm [t| [(String, Property)] |] (props api)
  where
    props api = listE $ map generatePropCBOR2 [ an | ThNode an <- api ]

-- | For an APINode, generate a (String, Property) pair giving the
-- type name and an appropriate instance of the
-- prop_resultsMatchRoundtrip property
generatePropCBOR2 :: APINode -> ExpQ
generatePropCBOR2 an = [e| ($ty, property (prop_resultsMatchRoundtripCBOR2 :: $(nodeT an) -> Bool)) |]
  where
    ty = stringE $ _TypeName $ anName an


-- | QuickCheck property that a 'Value' decodes to an expected Haskell
-- value, using 'fromJSONWithErrs'
prop_decodesTo :: forall a . (Eq a, FromJSONWithErrs a)
               => JS.Value -> a -> Bool
prop_decodesTo v x = case fromJSONWithErrs v :: Either [(JSONError, Position)] a of
                       Right y | x == y -> True
                       _                -> False

-- | QuickCheck property that a 'Value' decodes to an expected Haskell
-- value, using 'fromJSONWithErrs'' with the given 'ParseFlags'
prop_decodesTo' :: forall a . (Eq a, FromJSONWithErrs a)
               => ParseFlags -> JS.Value -> a -> Bool
prop_decodesTo' pf v x = case fromJSONWithErrs' pf v :: Either [(JSONError, Position)] a of
                           Right y | x == y -> True
                           _                -> False

-- | QuickCheck property that Haskell values can be encoded with
-- 'toJSON' and decoded with 'fromJSONWithErrs' to get the original
-- value
prop_resultsMatchRoundtrip :: forall a . (Eq a, JS.ToJSON a, FromJSONWithErrs a )
                           => a -> Bool
prop_resultsMatchRoundtrip x = prop_decodesTo (JS.toJSON x) x

-- TODO: check that JSON obtained via cbor2json.rb from the CBOR generated
-- from the Haskell values is equal to that obtained via toJSON.
-- For now, before we can generate CBOR from haskell values,
-- we obtain CBOR from JSON and so basically test the Ruby code only.
prop_resultsMatchRoundtripCBOR :: forall a . (Eq a, JS.ToJSON a, FromJSONWithErrs a )
                               => a -> Bool
prop_resultsMatchRoundtripCBOR x =
  let !js = toStrict (JS.encode x)
      !cbor = readProcessUnsafe "json2cbor.rb" [] js
      !js2 = readProcessUnsafe "cbor2json.rb" [] cbor
      !parsed = either error id (parseOnly (JS.json' {-<* endOfInput-}) js2)
  in prop_decodesTo parsed x

readProcessUnsafe :: FilePath -> [String] -> ByteString -> ByteString
{-# NOINLINE readProcessUnsafe #-}
readProcessUnsafe p args input = unsafePerformIO $ do
  (_, out, _) <- readProcessWithExitCode p args input
  return out


prop_decodesToCBOR :: forall a . (Eq a, Serialise a)
                   => BS.ByteString -> a -> Bool
prop_decodesToCBOR v x = deserialise v == x

-- TODO: should we test serialise or serialiseIncremental?
prop_resultsMatchRoundtripCBOR2 :: forall a . (Eq a, Serialise a )
                                => a -> Bool
prop_resultsMatchRoundtripCBOR2 x = prop_decodesToCBOR (serialise x) x
