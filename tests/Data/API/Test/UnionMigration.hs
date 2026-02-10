{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Tests for union alternative migration with type changes
--
-- This module tests the 'alternative changed' changelog feature, which allows
-- changing the type of a union alternative with a custom migration function.
module Data.API.Test.UnionMigration
    ( unionMigrationTests
    ) where

import           Data.API.Changes
import           Data.API.JSON
import           Data.API.JSON.Compat
import           Data.API.Types
import           Data.API.Utils

import qualified Data.Aeson               as JS
import qualified Data.Aeson.Encode.Pretty as JS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text                as T
import           Data.Version
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.API.Test.UnionMigrationData


-- Generate migration enums from changelog
$(generateMigrationKinds typeSwapChangelog "TypeSwapDbMigration" "TypeSwapRecordMigration" "TypeSwapUnionAltMigration" "TypeSwapFieldMigration")


-- -----------------------------------------------------------------------------
-- Type Swap Migration (PersonV1 -> PersonV2)
-- -----------------------------------------------------------------------------

-- | Migrate PersonV1 to PersonV2
--
-- PersonV1: { "name": "John" }
-- PersonV2: { "fullName": "John", "age": 0 }
--
-- This is a type migration because we're transforming the entire inner value
-- of the union alternative from one type to another.
migratePersonV1ToV2 :: TypeSwapUnionAltMigration -> JS.Value -> Either ValueError JS.Value
migratePersonV1ToV2 MigratePersonV1ToV2 (JS.Object obj) = do
    nameVal <- lookupKey "name" obj ?! CustomMigrationError "missing 'name' field" (JS.Object obj)
    case nameVal of
        JS.String name -> return $ JS.Object $
            insertKey "fullName" (JS.String name) $
            singletonObject "age" (JS.Number 0)
        _ -> Left $ CustomMigrationError "expected string for 'name'" (JS.Object obj)
migratePersonV1ToV2 MigratePersonV1ToV2 v =
    Left $ CustomMigrationError "expected object for PersonV1" v


typeSwapMigration :: CustomMigrations JS.Object JS.Value TypeSwapDbMigration TypeSwapRecordMigration TypeSwapUnionAltMigration TypeSwapFieldMigration
typeSwapMigration = CustomMigrations
    { databaseMigration       = \ _ -> noDataChanges
    , databaseMigrationSchema = \ _ -> noSchemaChanges
    , typeMigration           = \ _ -> noDataChanges
    , typeMigrationSchema     = \ _ -> noSchemaChanges
    , unionAltMigration       = migratePersonV1ToV2
    , fieldMigration          = \ _ -> noDataChanges
    }


-- Test data for type swap
--
-- Start: Container with MyUnion containing PersonV1
-- End:   Container with MyUnion containing PersonV2

-- | Start data: { "person": { "person": { "name": "Alice" } } }
startTypeSwapData :: JS.Value
Just startTypeSwapData = JS.decode "{ \"person\": { \"person\": { \"name\": \"Alice\" } } }"

-- | Expected end data: { "person": { "person": { "fullName": "Alice", "age": 0 } } }
expectedTypeSwapData :: JS.Value
Just expectedTypeSwapData = JS.decode "{ \"person\": { \"person\": { \"fullName\": \"Alice\", \"age\": 0 } } }"

-- | Start data with "other" alternative (should pass through unchanged)
startOtherAltData :: JS.Value
Just startOtherAltData = JS.decode "{ \"person\": { \"other\": 42 } }"

-- | Expected end data for "other" alternative (unchanged)
expectedOtherAltData :: JS.Value
Just expectedOtherAltData = JS.decode "{ \"person\": { \"other\": 42 } }"


-- | Test migrating PersonV1 to PersonV2 within a union
typeSwapMigrationTest :: Assertion
typeSwapMigrationTest = do
    -- Verify start data matches start schema
    case dataMatchesAPI rootName startTypeSwapSchema startTypeSwapData of
        Right () -> return ()
        Left err -> assertFailure $ "Start data does not match start API: "
                                      ++ prettyValueErrorPosition err

    -- Verify expected end data matches end schema
    case dataMatchesAPI rootName endTypeSwapSchema expectedTypeSwapData of
        Right () -> return ()
        Left err -> assertFailure $ "Expected end data does not match end API: "
                                      ++ prettyValueErrorPosition err

    -- Run migration
    case migrateDataDump (startTypeSwapSchema, parseVer "0")
                         (endTypeSwapSchema, Release (parseVer "1.0"))
                         typeSwapChangelog typeSwapMigration rootName CheckAll
                         startTypeSwapData of
      Right (v, [])
          | expectedTypeSwapData == v -> return ()
          | otherwise -> assertFailure $ unlines
              [ "Type swap migration produced wrong result"
              , "Expected:"
              , BL.unpack (JS.encodePretty expectedTypeSwapData)
              , "but got:"
              , BL.unpack (JS.encodePretty v)
              ]
      Right (_, ws) -> assertFailure $ "Unexpected warnings: " ++ show ws
      Left err      -> assertFailure $ "Migration failed: " ++ prettyMigrateFailure err


-- | Test that non-matching alternatives pass through unchanged
otherAlternativeUnchangedTest :: Assertion
otherAlternativeUnchangedTest = do
    -- Verify start data matches start schema
    case dataMatchesAPI rootName startTypeSwapSchema startOtherAltData of
        Right () -> return ()
        Left err -> assertFailure $ "Start data does not match start API: "
                                      ++ prettyValueErrorPosition err

    -- Verify expected end data matches end schema
    case dataMatchesAPI rootName endTypeSwapSchema expectedOtherAltData of
        Right () -> return ()
        Left err -> assertFailure $ "Expected end data does not match end API: "
                                      ++ prettyValueErrorPosition err

    -- Run migration - "other" alternative should pass through unchanged
    case migrateDataDump (startTypeSwapSchema, parseVer "0")
                         (endTypeSwapSchema, Release (parseVer "1.0"))
                         typeSwapChangelog typeSwapMigration rootName CheckAll
                         startOtherAltData of
      Right (v, [])
          | expectedOtherAltData == v -> return ()
          | otherwise -> assertFailure $ unlines
              [ "Other alternative was incorrectly modified"
              , "Expected:"
              , BL.unpack (JS.encodePretty expectedOtherAltData)
              , "but got:"
              , BL.unpack (JS.encodePretty v)
              ]
      Right (_, ws) -> assertFailure $ "Unexpected warnings: " ++ show ws
      Left err      -> assertFailure $ "Migration failed: " ++ prettyMigrateFailure err


rootName :: TypeName
rootName = TypeName "Container"

parseVer :: String -> Version
parseVer s = case simpleParseVersion s of
    Just v -> v
    Nothing -> error $ "Invalid version: " ++ s


-- | All union migration tests
unionMigrationTests :: TestTree
unionMigrationTests = testGroup "Union Alternative Migration"
  [ testCase "Type swap: PersonV1 -> PersonV2" typeSwapMigrationTest
  , testCase "Other alternatives pass through unchanged" otherAlternativeUnchangedTest
  ]
