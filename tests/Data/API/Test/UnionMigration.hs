{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Standalone test for union alternative migration with field changes
module Data.API.Test.UnionMigration
    ( unionMigrationTests
    ) where

import           Data.API.Changes
import           Data.API.JSON
import           Data.API.JSON.Compat
import           Data.API.Tools
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
$(generateMigrationKinds testChangelog "TestDbMigration" "TestRecordMigration" "TestFieldMigration")


-- Custom field migration that adds a 'name' field prefixed with "id_"
testFieldMigration :: TestFieldMigration -> JS.Value -> Either ValueError JS.Value
testFieldMigration AddNameToTestRecord (JS.Object x) = do
    i <- lookupKey "id" x ?! CustomMigrationError "missing id" (JS.Object x)
    case i of
        JS.Number n -> do
            let name = JS.String $ "id_" `T.append` T.pack (show (floor (toRational n) :: Int))
            return $ JS.Object $ insertKey "name" name x
        _ -> Left $ CustomMigrationError "bad id" (JS.Object x)
testFieldMigration AddNameToTestRecord v = Left $ CustomMigrationError "bad data" v


-- Custom migrations record
testMigration :: CustomMigrations JS.Object JS.Value TestDbMigration TestRecordMigration TestFieldMigration
testMigration = CustomMigrations
    { databaseMigration       = \ _ -> noDataChanges
    , databaseMigrationSchema = \ _ -> noSchemaChanges
    , typeMigration           = \ _ -> noDataChanges
    , typeMigrationSchema     = \ _ -> noSchemaChanges
    , fieldMigration          = testFieldMigration
    }


-- Test data
startUnionData :: JS.Value
Just startUnionData = JS.decode "{ \"alt\": {\"id\": 42} }"

expectedUnionData :: JS.Value
Just expectedUnionData = JS.decode "{ \"alt\": {\"id\": 42, \"name\": \"id_42\"} }"


-- | The basic test case for union alternative migration
unionAlternativeMigrationTest :: Assertion
unionAlternativeMigrationTest = do
    -- Verify data matches schemas
    case dataMatchesAPI rootUnionName startUnionSchema startUnionData of
        Right () -> return ()
        Left err -> assertFailure $ "Start data does not match start API: "
                                      ++ prettyValueErrorPosition err

    case dataMatchesAPI rootUnionName endUnionSchema expectedUnionData of
        Right () -> return ()
        Left err -> assertFailure $ "Expected end data does not match end API: "
                                      ++ prettyValueErrorPosition err

    -- Run migration
    let startVer = parseVer "0"
    case migrateDataDump (startUnionSchema, startVer) (endUnionSchema, parseVerExtra "1.0")
                         testChangelog testMigration rootUnionName CheckAll startUnionData of
      Right (v, []) | expectedUnionData == v -> return ()
                    | otherwise    -> assertFailure $ unlines
                                      [ "Expected:"
                                      , BL.unpack (JS.encodePretty expectedUnionData)
                                      , "but got:"
                                      , BL.unpack (JS.encodePretty v)
                                      ]
      Right (_, ws) -> assertFailure $ "Unexpected warnings: " ++ show ws
      Left err      -> assertFailure $ "Migration failed: " ++ prettyMigrateFailure err


rootUnionName :: TypeName
rootUnionName = TypeName "TestUnion"

parseVer :: String -> Version
parseVer s = case simpleParseVersion s of
    Just v -> v
    Nothing -> error $ "Invalid version: " ++ s

parseVerExtra :: String -> VersionExtra
parseVerExtra s = Release $ parseVer s


-- | All union migration tests
unionMigrationTests :: TestTree
unionMigrationTests = testGroup "Union Alternative Migration"
  [ testCase "Union alternative migration with field change" unionAlternativeMigrationTest
  ]
