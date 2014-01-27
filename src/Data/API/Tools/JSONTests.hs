{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes          #-}

-- Here we use Template Haskell to generate a test suite for the Aeson wrappers
-- from the DSL description of same.

module Data.API.Tools.JSONTests
    ( jsonTestsTool
    , prop_decodesTo
    , prop_resultsMatchRoundtrip
    ) where

import           Data.API.JSON
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.TH
import           Data.API.Types

import qualified Data.Aeson                     as JS
import           Language.Haskell.TH
import           Test.QuickCheck


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


-- | QuickCheck property that a 'Value' decodes to an expected Haskell
-- value, using 'fromJSONWithErrs'
prop_decodesTo :: forall a . (Eq a, FromJSONWithErrs a)
               => JS.Value -> a -> Bool
prop_decodesTo v x = case fromJSONWithErrs v :: Either [(JSONError, Position)] a of
                       Right y | x == y -> True
                       _                -> False

-- | QuickCheck property that Haskell values can be encoded with
-- 'toJSON' and decoded with 'fromJSONWithErrs' to get the original
-- value
prop_resultsMatchRoundtrip :: forall a . (Eq a, JS.ToJSON a, FromJSONWithErrs a )
                           => a -> Bool
prop_resultsMatchRoundtrip x = prop_decodesTo (JS.toJSON x) x
