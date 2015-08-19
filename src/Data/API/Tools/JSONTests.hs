{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE BangPatterns #-}

-- Here we use Template Haskell to generate a test suite for the Aeson wrappers
-- from the DSL description of same.

module Data.API.Tools.JSONTests
    ( -- * Tools
      jsonTestsTool
    , cborTestsTool
    , jsonViaCBORTestsTool
    , jsonTestsToolCBOR

      -- * Properties
    , prop_decodesTo
    , prop_decodesTo'
    , prop_resultsMatchRoundtrip
    , prop_cborRoundtrip
    , prop_toJSONViaCBOR
    ) where

import           Data.API.JSON
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.TH
import           Data.API.Types

import qualified Data.Aeson                     as JS
import           Data.Binary.Serialise.CBOR
import           Data.Binary.Serialise.CBOR.Aeson ()
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
jsonTestsTool = testsTool 'prop_resultsMatchRoundtrip

cborTestsTool :: Name -> APITool
cborTestsTool = testsTool 'prop_cborRoundtrip

jsonViaCBORTestsTool :: Name -> APITool
jsonViaCBORTestsTool = testsTool 'prop_toJSONViaCBOR

jsonTestsToolCBOR :: Name -> APITool
jsonTestsToolCBOR = testsTool 'prop_resultsMatchRoundtripCBOR


-- | Tool to generate a list of tests of type @[('String', 'Property')]@
-- based on instantiating the first argument at type @A -> Bool@ for each
-- type @A@ in the API.  The second argument is the name of the declaration
-- that should be produced.
testsTool :: Name -> Name -> APITool
testsTool prop_nm nm = simpleTool $ \ api -> simpleSigD nm [t| [(String, Property)] |] (props api)
  where
    props api = listE $ map (generateProp prop_nm) [ an | ThNode an <- api ]

-- | For an APINode, generate a (String, Property) pair giving the
-- type name and an appropriate instance of the property
generateProp :: Name -> APINode -> ExpQ
generateProp prop_nm an = [e| ($ty, property ($(varE prop_nm) :: $(nodeT an) -> Bool)) |]
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

-- | QuickCheck property that CBOR decoding is a left inverse for encoding
prop_cborRoundtrip :: forall a . (Eq a, Serialise a)
                   => a -> Bool
prop_cborRoundtrip x = deserialise (serialise x) == x

-- | QuickCheck property that 'toJSON' agrees with encoding to CBOR
-- and then decoding using the generic decoder
prop_toJSONViaCBOR :: forall a . (Eq a, Serialise a, JS.ToJSON a)
                   => a -> Bool
prop_toJSONViaCBOR x = deserialise (serialise x) == JS.toJSON x

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
