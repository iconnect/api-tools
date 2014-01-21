module Data.API.Test.Main
    ( main
    ) where

import           Data.API.Test.JSON
import           Data.API.Test.Migration

import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "api-tools" [ migrationTests
                              , jsonTests
                              ]
