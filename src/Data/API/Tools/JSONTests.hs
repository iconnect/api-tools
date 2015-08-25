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
    , jsonTestsToolCBOR
    , cborToJSONTestsTool
    , jsonToCBORTestsTool

      -- * Properties
    , prop_decodesTo
    , prop_decodesTo'
    , prop_resultsMatchRoundtrip
    , prop_cborRoundtrip
    , prop_cborToJSON
    , prop_jsonToCBOR
    ) where

import           Data.API.Changes
import           Data.API.JSON
import           Data.API.JSONToCBOR
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.TH
import           Data.API.Types

import qualified Data.Aeson                     as JS
import           Data.Binary.Serialise.CBOR
import           Data.Binary.Serialise.CBOR.JSON ()
import qualified Data.ByteString.Lazy           as BS
import           Language.Haskell.TH
import           Test.QuickCheck
import           Test.QuickCheck.Property       as QCProperty

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


-- TODO not really tools at all?
cborToJSONTestsTool :: Name -> Name -> APITool
cborToJSONTestsTool = schemaTestsTool 'prop_cborToJSON

jsonToCBORTestsTool :: Name -> Name -> APITool
jsonToCBORTestsTool = schemaTestsTool 'prop_jsonToCBOR

-- | Tool to generate a list of tests of properties that take the API
-- and the type name as arguments, and return a 'QCProperty.Result'.
schemaTestsTool :: Name -> Name -> Name -> APITool
schemaTestsTool prop_nm api_nm nm = simpleTool $ \ api -> simpleSigD nm [t| [(String, Property)] |] (props api)
  where
    props api = listE $ map (genProp api) [ an | ThNode an <- api ]

    genProp api an = [e| ($ty, property ($(varE prop_nm) $(varE api_nm) tn :: $(nodeT an) -> QCProperty.Result)) |]
      where
        tn = anName an
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
prop_cborToJSON :: forall a . (Eq a, Serialise a, JS.ToJSON a)
                   => API -> TypeName -> a -> QCProperty.Result
prop_cborToJSON api tn x = case postprocessJSON CBORToAeson api tn (deserialise (serialise x)) of
                         Right v | v == JS.toJSON x -> succeeded
                                 | otherwise        -> failed { QCProperty.reason = "Post-processed: " ++ show v
                                                                               ++ "\nDirect JSON:    " ++ show (JS.toJSON x) }
                         Left err                   -> failed { QCProperty.reason = prettyValueError err }

-- | QuickCheck property that conversion 'toJSON', followed by
-- schema-aware 'jsonToCBOR' conversion, gives the same results as
-- direct conversion to CBOR via 'serialise'
prop_jsonToCBOR :: forall a . (Eq a, Serialise a, JS.ToJSON a)
                => API -> TypeName -> a -> QCProperty.Result
prop_jsonToCBOR api tn x = case jsonToCBOR api tn (JS.toJSON x) of
                             Right v  | serialise v == serialise x -> succeeded
                                      | otherwise -> failed { QCProperty.reason = "Failed with JSON: " ++ show (JS.toJSON x)
                                                                                ++ "\n serialise v: " ++ show (serialise v)
                                                                                ++ "\n serialise x: " ++ show (serialise x) }
                             Left err -> failed { QCProperty.reason = prettyValueError err }

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
