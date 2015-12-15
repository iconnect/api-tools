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
    , cborToJSONTestsTool
    , jsonToCBORTestsTool
    , jsonGenericValueTestsTool
    , cborGenericValueTestsTool

      -- * Properties
    , prop_decodesTo
    , prop_decodesTo'
    , prop_resultsMatchRoundtrip
    , prop_cborRoundtrip
    , prop_cborToJSON
    , prop_jsonToCBOR
    ) where

import           Data.API.JSON
import           Data.API.JSONToCBOR
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.TH
import           Data.API.Types
import           Data.API.Value

import qualified Data.Aeson                     as JS
import           Data.Binary.Serialise.CBOR
import           Data.Binary.Serialise.CBOR.JSON ()
import           Data.Monoid
import           Language.Haskell.TH
import           Test.QuickCheck
import           Test.QuickCheck.Property       as QCProperty

-- | Tool to generate a list of JSON round-trip tests of type
-- @[('String', 'Property')]@ with the given name.  This depends on
-- 'jsonTool' and 'quickCheckTool'.
jsonTestsTool :: Name -> APITool
jsonTestsTool = testsTool 'prop_resultsMatchRoundtrip

-- | Tool to generate a list of CBOR round-trip tests of type
-- @[('String', 'Property')]@ with the given name.  This depends on
-- 'cborTool' and 'quickCheckTool'.
cborTestsTool :: Name -> APITool
cborTestsTool = testsTool 'prop_cborRoundtrip

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
    ty = typeNameE $ anName an


-- | Tool to generate a list of CBOR-to-JSON conversion tests of type
-- @[('String', 'Property')]@.  The first name must be the 'API' being
-- tested, and the second should be the name of the declaration to be
-- produced.  This depends on 'cborTool', 'jsonTool' and 'quickCheckTool'.
cborToJSONTestsTool :: Name -> Name -> APITool
cborToJSONTestsTool = schemaTestsTool 'prop_cborToJSON

-- | Tool to generate a list of JSON-to-CBOR conversion tests of type
-- @[('String', 'Property')]@.  The first name must be the 'API' being
-- tested, and the second should be the name of the declaration to be
-- produced.  This depends on 'cborTool', 'jsonTool' and 'quickCheckTool'.
jsonToCBORTestsTool :: Name -> Name -> APITool
jsonToCBORTestsTool = schemaTestsTool 'prop_jsonToCBOR

jsonGenericValueTestsTool :: Name -> Name -> APITool
jsonGenericValueTestsTool = schemaTestsTool 'prop_jsonGeneric

cborGenericValueTestsTool :: Name -> Name -> APITool
cborGenericValueTestsTool = schemaTestsTool 'prop_cborGeneric

-- | Tool to generate a list of tests of properties that take the API
-- and the type name as arguments, and return a 'QCProperty.Result'.
schemaTestsTool :: Name -> Name -> Name -> APITool
schemaTestsTool prop_nm api_nm nm = simpleTool $ \ api -> simpleSigD nm [t| [(String, Property)] |] (props api)
  where
    props api = listE $ map genProp [ an | ThNode an <- api ]

    genProp an = [e| ($ty, property ($(varE prop_nm) $(varE api_nm) tn :: $(nodeT an) -> QCProperty.Result)) |]
      where
        tn = anName an
        ty = typeNameE $ anName an


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
-- directly and then decoding using the schema-aware generic decoder.
-- From this and 'prop_resultsMatchRoundtrip' it follows that
--
-- > fromJSONWithErrs . deserialiseJSONWithSchema . serialise == id
prop_cborToJSON :: forall a . (Eq a, Serialise a, JS.ToJSON a)
                   => API -> TypeName -> a -> QCProperty.Result
prop_cborToJSON api tn x
  | v1 == v2  = succeeded
  | otherwise = failed { QCProperty.reason = "Post-processed: " ++ show v1
                                        ++ "\nDirect JSON:    " ++ show v2 }
  where
    v1 = deserialiseJSONWithSchema api tn (serialise x)
    v2 = JS.toJSON x

-- | QuickCheck property that direct encoding to CBOR agrees with
-- conversion to JSON followed by the schema-aware generic encoder.
-- From this and 'prop_cborRoundtrip' it follows that
--
-- > deserialise . serialiseJSONWithSchema . toJSON == id
prop_jsonToCBOR :: forall a . (Eq a, Serialise a, JS.ToJSON a)
                => API -> TypeName -> a -> QCProperty.Result
prop_jsonToCBOR api tn x
  | e1 == e2  = succeeded
  | otherwise = failed { QCProperty.reason = "Failed with JSON:      " ++ show v
                                        ++ "\nGeneric serialisation: " ++ show e1
                                        ++ "\nDirect serialisation:  " ++ show e2 }
  where
    v  = JS.toJSON x
    e1 = serialiseJSONWithSchema api tn v
    e2 = serialise x
