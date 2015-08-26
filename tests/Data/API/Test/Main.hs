module Data.API.Test.Main
    ( main
    ) where

import           Data.API.API
import           Data.API.API.Gen
import           Data.API.Test.JSON
import           Data.API.Test.Migration

import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "api-tools" [ migrationTests
                              , jsonTests
                              , testProperty "Convert/unconvert" convertUncovertTest
                              ]

-- | Test that converting an 'API' into a self-hosted 'APISpec' and
-- back does not change it.  We have 'Arbitrary' instances only for
-- 'APISpec', but we compare 'API' because it is slightly more precise
-- (e.g. 'APISpec' contains bogus read-only fields for unions).
convertUncovertTest :: APISpec -> Bool
convertUncovertTest api = unconvertAPI (convertAPI unconverted_api) == unconverted_api
  where
    unconverted_api = unconvertAPI api
