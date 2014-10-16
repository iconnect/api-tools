{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Data.API.Test.JSON
    ( jsonTests
    ) where

import           Data.API.API.Gen ( apiAPISimpleTests )
import           Data.API.JSON
import           Data.API.Tools
import           Data.API.Tools.JSONTests
import           Data.API.Test.Gen hiding ( Foo )
import           Data.API.Test.MigrationData
import           Data.API.Types
import           Data.API.Utils

import qualified Data.Aeson               as JS
import qualified Data.HashMap.Strict      as HMap
import           Data.Time

import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck    as QC


$(generate         startSchema)
$(generateAPITools startSchema
                   [ enumTool
                   , jsonTool'
                   , quickCheckTool
                   ])

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
                               , help' noFilter (JS.Number 0) (UnsafeMkFilteredInt 0) True
                               , help' noFilter (JS.String "cabcage") (UnsafeMkFilteredString "cabcage") True
                               , help' noFilter (JS.String "2014-10-13T15:20:10Z") (UnsafeMkFilteredUTC (pUTC "2014-10-13T15:20:10Z")) True
                               ]
  where
    help v x yes = assertBool ("Failed on " ++ show v ++ " " ++ show x)
                              (prop_decodesTo v x == yes)
    help' pf v x yes = assertBool ("Failed on " ++ show v ++ " " ++ show x)
                                  (prop_decodesTo' pf v x == yes)

    noFilter = defaultParseFlags { enforceFilters = False }

-- | Test that the correct errors are generated for bad JSON data
errorDecoding :: [TestTree]
errorDecoding = [ help "not enough input" ""         (proxy :: Int)
                      [(SyntaxError "not enough input", [])]
                , help "object for int"   "{}"       (proxy :: Int)
                      [(Expected ExpInt "Int" (JS.Object HMap.empty), [])]
                , help "missing alt"      "{}"       (proxy :: AUnion)
                      [(MissingAlt ["bar"], [])]
                , help "error inside alt" "{\"bar\": {}}" (proxy :: AUnion)
                      [(MissingField, [InField "id", InField "bar"])]
                , help "unexpected value" "[\"no\"]" (proxy :: [AnEnum])
                      [(UnexpectedEnumVal ["bar", "foo"] "no", [InElem 0])]
                , help "missing field"    "{}"       (proxy :: Bar)
                      [(MissingField, [InField "id"])]
                , help "int out of range" "[0]" (proxy :: [FilteredInt])
                      [(IntRangeError "FilteredInt" 0 (IntRange (Just 3) (Just 5)), [InElem 0])]
                , help "string mismatch" "[\"cabcage\"]" (proxy :: [FilteredString])
                      [(RegexError "FilteredString" "cabcage" (mkRegEx "cab*age"), [InElem 0])]
                , help "utc out of range" "[\"2014-10-13T15:20:10Z\"]" (proxy :: [FilteredUTC])
                      [(UTCRangeError "FilteredUTC" (pUTC "2014-10-13T15:20:10Z") (UTCRange (parseUTC_ "2014-10-13T15:20:11Z") Nothing), [InElem 0])]
                ]
  where
    proxy = error "proxy"

    help x s v es = testCase x $ case decodeWithErrs s `asTypeOf` Right v of
                      Right _  -> assertFailure $ "Decode returned value: " ++ show s
                      Left es' -> assertBool ("Unexpected error when decoding: " ++ show s
                                              ++ "\n" ++ prettyJSONErrorPositions es'
                                              ++ "\ninstead of\n" ++ prettyJSONErrorPositions es)
                                             (es == es')

-- | Test that smart constructors correctly enforce the invariants
smartConstructors :: [TestTree]
smartConstructors =
  [ testCase "mkFilteredInt"    $ do mkFilteredInt    2         @?= Nothing
                                     mkFilteredInt    3         @?= Just (UnsafeMkFilteredInt 3)
  , testCase "mkFilteredUTC"    $ do mkFilteredUTC    bad_time  @?= Nothing
                                     mkFilteredUTC    good_time @?= Just (UnsafeMkFilteredUTC good_time)
  , testCase "mkFilteredString" $ do mkFilteredString "cabcage" @?= Nothing
                                     mkFilteredString "cabbage" @?= Just (UnsafeMkFilteredString "cabbage")
  ]
  where
    bad_time  = pUTC "2014-10-13T15:20:10Z"
    good_time = pUTC "2014-10-13T15:20:13Z"

pUTC :: String -> UTCTime
pUTC = maybe (error "pUTC") id . parseUTC_

jsonTests :: TestTree
jsonTests = testGroup "JSON"
  [ testCase  "Basic value decoding"  basicValueDecoding
  , testGroup "Decoding invalid data" errorDecoding
  , testGroup "Smart constructors"    smartConstructors
  , testGroup "Round-trip tests"
      [ testGroup "example"  $ map (uncurry QC.testProperty) exampleSimpleTests
      , testGroup "example2" $ map (uncurry QC.testProperty) example2SimpleTests
      , testGroup "api"      $ map (uncurry QC.testProperty) apiAPISimpleTests
      ]
  ]
