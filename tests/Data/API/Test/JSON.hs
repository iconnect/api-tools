{-# LANGUAGE CPP                        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Data.API.Test.JSON
    ( jsonTests
    ) where

import           Data.API.API.Gen ( apiAPITestsJSON, apiAPITestsCBOR, apiAPITestsCBORToJSON, apiAPITestsJSONToCBOR )
import           Data.API.JSON
import           Data.API.NormalForm
import           Data.API.Tools
import           Data.API.Tools.JSONTests
import           Data.API.Test.DSL
import           Data.API.Test.Gen hiding ( Foo )
import           Data.API.Test.MigrationData
import           Data.API.Time
import           Data.API.Types
import qualified Data.API.Value           as Value

import qualified Data.Aeson               as JS
import           Data.Functor.Identity    (Identity(..))
import           Data.List                (find)
import qualified Data.Set                 as Set

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
                               , help (JS.Number 1.1)   (1 :: Int)  True
                               , help (JS.Number 1.9)   (1 :: Int)  True
                               , help (JS.Number 0)     False       True
                               , help (JS.Number 1)     True        True
                               , help (JS.Number 2)     True        False
                               , help (JS.String "0")   False       False
                               , help (JS.String "1")   True        False
                               , help (JS.object ["id" JS..= JS.Number 3])
                                      (Recursive (Id 3) Nothing)
                                      True
                                 -- Sets decode from plain arrays, insensitive
                                 -- to element order and to duplicates, and are
                                 -- not decoded from objects.
                               , help (JS.toJSON [3, 1, 2, 1 :: Int])
                                      (Set.fromList [1, 2, 3 :: Int])
                                      True
                               , help (JS.object ["value" JS..= [1 :: Int]])
                                      (Set.fromList [1 :: Int])
                                      False
                               , help' noFilter (JS.Number 0) (UnsafeMkFilteredInt 0) True
                               , help' noFilter (JS.String "cabcage") (UnsafeMkFilteredString "cabcage") True
                               , help' noFilter (JS.String "2014-10-13T15:20:10Z") (UnsafeMkFilteredUTC (unsafeParseUTC "2014-10-13T15:20:10Z")) True
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
#if MIN_VERSION_aeson(2,2,0)
                      [(SyntaxError "Unexpected end-of-input, expecting JSON value", [])]
#elif MIN_VERSION_aeson(0,10,0)
                      [(SyntaxError "Error in $: not enough input", [])]
#else
                      [(SyntaxError "not enough input", [])]
#endif
                , help "object for int"   "{}"       (proxy :: Int)
                      [(Expected ExpInt "Int" (JS.Object mempty), [])]
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
                      [(UTCRangeError "FilteredUTC" (unsafeParseUTC "2014-10-13T15:20:10Z") (UTCRange (parseUTC "2014-10-13T15:20:11Z") Nothing), [InElem 0])]
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
    bad_time  = unsafeParseUTC "2014-10-13T15:20:10Z"
    good_time = unsafeParseUTC "2014-10-13T15:20:13Z"

-- | Test that empty record definitions are parsed correctly
emptyRecordParsing :: [TestTree]
emptyRecordParsing =
  [ testCase "empty record has no fields" $
      case findNode "EmptyRecord" example2 of
        Nothing   -> assertFailure "EmptyRecord not found in example2 API"
        Just node -> anSpec node @?= SpRecord (SpecRecord [])
  ]
  where
    findNode name = find (\n -> anName n == TypeName name) . concatMap thNode
    thNode (ThNode n) = [n]
    thNode _          = []

jsonTests :: TestTree
jsonTests = testGroup "JSON"
  [ testCase  "Basic value decoding"  basicValueDecoding
  , testGroup "Decoding invalid data" errorDecoding
  , testGroup "Smart constructors"    smartConstructors
  , testGroup "Empty record parsing"  emptyRecordParsing
  , testGroup "Round-trip tests"
      [ testGroup "example JSON"   $ map (uncurry QC.testProperty) exampleTestsJSON
      , testGroup "example CBOR"   $ map (uncurry QC.testProperty) exampleTestsCBOR
      , testGroup "example CBOR to JSON" $ map (uncurry QC.testProperty) exampleTestsCBORToJSON
      , testGroup "example JSON to CBOR" $ map (uncurry QC.testProperty) exampleTestsJSONToCBOR
      , testGroup "example2 JSON"  $ map (uncurry QC.testProperty) example2TestsCBOR
      , testGroup "example2 CBOR"  $ map (uncurry QC.testProperty) example2TestsCBOR
      , testGroup "example2 CBOR to JSON" $ map (uncurry QC.testProperty) example2TestsCBORToJSON
      , testGroup "example2 JSON to CBOR" $ map (uncurry QC.testProperty) example2TestsJSONToCBOR
      , testGroup "api JSON"       $ map (uncurry QC.testProperty) apiAPITestsJSON
      , testGroup "api CBOR"       $ map (uncurry QC.testProperty) apiAPITestsCBOR
      , testGroup "api CBOR to JSON" $ map (uncurry QC.testProperty) apiAPITestsCBORToJSON
      , testGroup "api JSON to CBOR" $ map (uncurry QC.testProperty) apiAPITestsJSONToCBOR
      , QC.testProperty "Aeson Value to CBOR" (prop_cborRoundtrip :: JS.Value -> Bool)
      ]
  , testGroup "Generic values"
    [ QC.testProperty "example JSON round-trip" (Value.prop_jsonRoundTrip exampleNF)
    , QC.testProperty "example2 JSON round-trip" (Value.prop_jsonRoundTrip example2NF)
    , QC.testProperty "example CBOR round-trip" (Value.prop_cborRoundTrip exampleNF)
    , QC.testProperty "example2 CBOR round-trip" (Value.prop_cborRoundTrip example2NF)
    , testGroup "example agreement with ToJSON" $ map (uncurry QC.testProperty) exampleJSONGenericValueTests
    , testGroup "example2 agreement with ToJSON" $ map (uncurry QC.testProperty) example2JSONGenericValueTests
    , testGroup "example agreement with Serialise" $ map (uncurry QC.testProperty) exampleCBORGenericValueTests
    , testGroup "example2 agreement with Serialise" $ map (uncurry QC.testProperty) example2CBORGenericValueTests
    ]
  , testCase "traversal visits set elements" setTraversalTest
  ]

-- | Generated traversals must descend into set-valued fields, not silently
-- leave them alone.
setTraversalTest :: Assertion
setTraversalTest = assertEqual "flags not traversed" (Set.singleton True) (_srec_flags r')
  where
    -- Flag is a synonym for boolean, so the set has at most two elements;
    -- mapping them all to True must collapse it to a singleton.
    r  = SetRec (Set.fromList [1, 2]) (Set.fromList [False, True]) Nothing
    r' = runIdentity (traverseFlagSetRec (\ _ -> Identity True) r)

exampleNF :: NormAPI
exampleNF = apiNormalForm example

example2NF :: NormAPI
example2NF = apiNormalForm example2
