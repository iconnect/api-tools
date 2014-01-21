{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Data.API.Test.JSON
    ( jsonTests
    ) where

import           Data.API.API.Gen
import           Data.API.JSON
import           Data.API.Tools
import           Data.API.Test.Gen (exampleSimpleTests, example2SimpleTests)
import           Data.API.Test.MigrationData

import qualified Data.Aeson               as JS
import qualified Data.HashMap.Strict      as HMap

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck


$(generate startSchema)
$(generateInstances startSchema)

-- | Test that literals are decoded correctly, including the dubious
-- use of strings for numbers and numbers for booleans, and missing
-- fields being treated as nulls.
basicValueDecoding :: Assertion
basicValueDecoding = sequence_ [ help (JS.String "12")  (12 :: Int) True
                               , help (JS.String "0")   (0 :: Int)  True
                               , help (JS.String "-9")  (-9 :: Int) True
                               , help (JS.String "1a")  (1 :: Int)  False
                               , help (JS.Number 0)     False       True
                               , help (JS.Number 1)     True        True
                               , help (JS.Number 2)     True        False
                               , help (JS.String "0")   False       False
                               , help (JS.String "1")   True        False
                               , help (JS.Object (HMap.singleton "id" (JS.Number 3)))
                                      (Recursive (Id 3) Nothing)
                                      True
                               ]
  where
    help v x yes = assertBool ("Failed on " ++ show v ++ " " ++ show x)
                              (prop_decodesTo v x == yes)

-- | Test that the correct errors are generated for bad JSON data
errorDecoding :: [TestTree]
errorDecoding = [ help "not enough bytes" ""         (proxy :: Int)
                      [(SyntaxError "not enough bytes", [])]
                , help "object for int"   "{}"       (proxy :: Int)
                      [(Expected ExpInt "Int" (JS.Object HMap.empty), [])]
                , help "missing alt"      "{}"       (proxy :: AUnion)
                      [(MissingAlt ["bar"], [])]
                , help "unexpected value" "[\"no\"]" (proxy :: [AnEnum])
                      [(UnexpectedEnumVal ["bar", "foo"] "no", [InElem 0])]
                , help "missing field"    "{}"       (proxy :: Bar)
                      [(MissingField, [InField "id"])]
                ]
  where
    proxy = error "proxy"

    help x s v es = testCase x $ case decodeWithErrs s `asTypeOf` Right v of
                      Right _  -> assertFailure $ "Decode returned value: " ++ show s
                      Left es' -> assertBool ("Unexpected error when decoding: " ++ show s
                                              ++ "\n" ++ prettyJSONErrorPositions es'
                                              ++ "\ninstead of\n" ++ prettyJSONErrorPositions es)
                                             (es == es')

jsonTests :: TestTree
jsonTests = testGroup "JSON"
  [ testCase  "Basic value decoding"  basicValueDecoding
  , testGroup "Decoding invalid data" errorDecoding
  , testGroup "Round-trip tests"
      [ testGroup "example"  $ map (uncurry testProperty) exampleSimpleTests
      , testGroup "example2" $ map (uncurry testProperty) example2SimpleTests
      , testGroup "api"      $ map (uncurry testProperty) apiAPISimpleTests
      ]
  ]
