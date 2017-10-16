{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS_GHC -XNoCPP -fno-warn-unused-binds  #-}

module Data.API.Test.Migration
    ( migrationTests
    ) where

import           Data.API.Changes
import           Data.API.PP
import           Data.API.Tools
import           Data.API.Test.MigrationData
import           Data.API.Types
import           Data.API.Utils
import qualified Data.API.Value           as Value

import qualified Data.Aeson               as JS
import qualified Data.Aeson.Encode.Pretty as JS
import qualified Codec.Serialise as CBOR
import qualified Codec.CBOR.FlatTerm as CBOR
import qualified Data.ByteString.Char8    as B
import qualified Data.ByteString.Base64   as B64
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict      as HMap
import qualified Data.Map                 as Map
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Data.Version
import           Test.Tasty               as Test
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck    as QC
import           Test.QuickCheck.Property as P


$(generateMigrationKinds changelog "TestDatabaseMigration" "TestRecordMigration" "TestFieldMigration")


-- Test of a whole-database migration: copy data between tables
testDatabaseMigration :: TestDatabaseMigration -> JS.Object -> Either ValueError JS.Object
testDatabaseMigration DuplicateBar x = do
    bar <- HMap.lookup "bar" x ?! CustomMigrationError "missing bar" (JS.Object x)
    return $ HMap.insert "bar2" bar x
testDatabaseMigration DuplicateRecursive x = do
    recur <- HMap.lookup "recur" x ?! CustomMigrationError "missing recur" (JS.Object x)
    return $ HMap.insert "recur2" recur x

testDatabaseMigration' :: TestDatabaseMigration -> Value.Record -> Either ValueError Value.Record
testDatabaseMigration' DuplicateBar r = do
    let x = Value.recordToMap r
    bar <- Map.lookup "bar" x ?! CustomMigrationError "missing bar" JS.Null
    return $ Value.mapToRecord $ Map.insert "bar2" bar x
testDatabaseMigration' DuplicateRecursive r = do
    let x = Value.recordToMap r
    recur <- Map.lookup "recur" x ?! CustomMigrationError "missing recur" JS.Null
    return $ Value.mapToRecord $ Map.insert "recur2" recur x

testDatabaseMigrationSchema :: TestDatabaseMigration -> NormAPI -> Either ApplyFailure (Maybe NormAPI)
testDatabaseMigrationSchema DuplicateBar _ = Right Nothing
testDatabaseMigrationSchema DuplicateRecursive napi =
    let Just recur = Map.lookup (TypeName "Recursive") napi
        Just (NRecordType dbs) = Map.lookup root_ napi
        dbs' = Map.insert (FieldName "recur2") (TyMaybe (TyList (TyName "DuplicateRecursive"))) dbs
    in Right $ Just $ Map.insert (TypeName "DuplicateRecursive") recur $
                      Map.insert root_ (NRecordType dbs') napi


-- Test of a single-record migration: copy the value in the id field
-- onto the end of the c field
testRecordMigration :: TestRecordMigration -> JS.Value -> Either ValueError JS.Value
testRecordMigration CopyIDtoC = mkRecordMigration $ \ x -> do
    i <- HMap.lookup "id" x ?! CustomMigrationError "missing id" (JS.Object x)
    b <- HMap.lookup "c" x  ?! CustomMigrationError "missing b" (JS.Object x)
    r <- case (i, b) of
        (JS.Number j, JS.String t)
            -> return $ JS.String $ t `T.append` T.pack (show (round j :: Int))
        _   -> Left $ CustomMigrationError "bad data" (JS.Object x)
    return $ HMap.insert "c" r x
testRecordMigration DuplicateNew = mkRecordMigration $ \ x -> do
    new <- HMap.lookup "new" x ?! CustomMigrationError "missing new" (JS.Object x)
    return $ HMap.insert "newnew" new x

testRecordMigration' :: TestRecordMigration -> Value.Value -> Either ValueError Value.Value
testRecordMigration' CopyIDtoC = mkRecordMigration' $ \ rec -> do
    let x = Value.recordToMap rec
    i <- Map.lookup "id" x ?! CustomMigrationError "missing id" JS.Null
    b <- Map.lookup "c" x  ?! CustomMigrationError "missing b" JS.Null
    r <- case (i, b) of
        (Value.Int j, Value.String t)
            -> return $ Value.String $ t `T.append` T.pack (show j)
        _   -> Left $ CustomMigrationError "bad data" JS.Null
    return $ Value.mapToRecord $ Map.insert "c" r x
testRecordMigration' DuplicateNew = mkRecordMigration' $ \ rec -> do
    let x = Value.recordToMap rec
    new <- Map.lookup "new" x ?! CustomMigrationError "missing new" JS.Null
    return $ Value.mapToRecord $ Map.insert "newnew" new x

testRecordMigrationSchema :: TestRecordMigration -> NormTypeDecl -> Either ApplyFailure (Maybe NormTypeDecl)
testRecordMigrationSchema CopyIDtoC    = noSchemaChanges
testRecordMigrationSchema DuplicateNew = mkRecordMigrationSchema "Recursive" $ \ r ->
    return $ Just $ Map.insert (FieldName "newnew") (TyBasic BTstring) r

-- Test of a single-field migration: change the type of the field from
-- binary to string by base64-decoding the contents
testFieldMigration :: TestFieldMigration -> JS.Value -> Either ValueError JS.Value
testFieldMigration ConvertBinaryToString v@(JS.String s) =
    case B64.decode (B.pack (T.unpack s)) of
        Left err  -> Left (CustomMigrationError err v)
        Right x -> return (JS.String (T.pack (B.unpack x)))
testFieldMigration ConvertBinaryToString v = Left $ CustomMigrationError "bad data" v

testFieldMigration' :: TestFieldMigration -> Value.Value -> Either ValueError Value.Value
testFieldMigration' ConvertBinaryToString (Value.Bytes bs) = return (Value.String (TE.decodeUtf8 (_Binary bs)))
testFieldMigration' ConvertBinaryToString v = Left $ CustomMigrationError "bad data" (JS.toJSON v)


testMigration :: CustomMigrations JS.Object JS.Value TestDatabaseMigration TestRecordMigration TestFieldMigration
testMigration = CustomMigrations testDatabaseMigration
                                 testDatabaseMigrationSchema
                                 testRecordMigration
                                 testRecordMigrationSchema
                                 testFieldMigration

testMigration' :: CustomMigrations Value.Record Value.Value TestDatabaseMigration TestRecordMigration TestFieldMigration
testMigration' = CustomMigrations testDatabaseMigration'
                                  testDatabaseMigrationSchema
                                  testRecordMigration'
                                  testRecordMigrationSchema
                                  testFieldMigration'


assertMatchesAPI :: String -> API -> JS.Value -> Assertion
assertMatchesAPI x a v = case dataMatchesAPI root_ a v of
    Right () -> return ()
    Left err -> assertFailure (x ++ ": " ++ prettyValueErrorPosition err)

basicMigrationTest :: Assertion
basicMigrationTest = do
    assertMatchesAPI "Start data does not match start API" startSchema startData
    assertMatchesAPI "End data does not match end API"     endSchema   endData
    case migrateDataDump (startSchema, startVersion) (endSchema, DevVersion)
                         changelog testMigration root_ CheckAll startData of
      Right (v, []) | endData == v -> return ()
                    | otherwise    -> assertFailure $ "expected:\n"
                                      ++ BL.unpack (JS.encodePretty endData)
                                      ++ "\nbut got:\n"
                                      ++ BL.unpack (JS.encodePretty v)
      Right (_, ws) -> assertFailure $ "Unexpcted warnings: " ++ show ws
      Left err      -> assertFailure (prettyMigrateFailure err)

applyFailureTest :: (Version, Version, ApplyFailure) -> Test.TestTree
applyFailureTest (ver, ver', expected) =
    testCase (showVersion ver ++ " -> " ++ showVersion ver') $
          case migrateDataDump (startSchema, ver) (endSchema, Release ver')
                               badChangelog testMigration root_ CheckAll startData of
            Right _ -> assertFailure $ "Successful migration!"
            Left (ValidateFailure (ChangelogEntryInvalid _ _ err))
                | err == expected -> return ()
            Left err -> assertFailure $ unlines $ ["Unexpected failure:"]
                        ++ indent (ppLines err) ++ ["Expecting:"]
                        ++ indent (ppLines expected)

migrateFailureTest :: MigrateFailureTest
                    -> Test.TestTree
migrateFailureTest (s, start, end, clog, db, expected) =
    testCase s $ case migrateDataDump start end clog testMigration root_ CheckAll db of
        Right _                 -> assertFailure $ "Successful migration!"
        Left err | expected err -> return ()
                 | otherwise    -> assertFailure $ unlines $ ["Unexpected failure:"]
                                                             ++ indent (ppLines err)


$(generate         startSchema)
$(generateAPITools startSchema
                   [ enumTool
                   , jsonTool'
                   , cborTool
                   , quickCheckTool
                   ])

validMigrationProperty :: DatabaseSnapshot -> P.Result
validMigrationProperty db =
    case migrateDataDump (startSchema, startVersion) (endSchema, DevVersion)
                         changelog testMigration root_ CheckStartAndEnd (JS.toJSON db) of
    Right (v, []) -> case dataMatchesAPI root_ endSchema v of
        Right _   -> succeeded
        Left  err -> failedBecause ("end data does not match API: "
                                    ++ prettyValueErrorPosition err)
    Right (_, ws) -> failedBecause ("migration generated warnings: " ++ show ws)
    Left err      -> failedBecause ("migration failed: " ++ prettyMigrateFailure err)
  where
    failedBecause e = failed { reason = e }

validMigrationProperty' :: DatabaseSnapshot -> P.Result
validMigrationProperty' db =
    case migrateDataDump' (startSchema, startVersion) (endSchema, DevVersion)
                         changelog testMigration' root_ CheckStartAndEnd db_generic of
    Right (v, []) -> case dataMatchesAPI root_ endSchema (JS.toJSON v) of
        Right _   -> succeeded
        Left  err -> failedBecause ("end data does not match API: "
                                    ++ prettyValueErrorPosition err)
    Right (_, ws) -> failedBecause ("migration generated warnings: " ++ show ws)
    Left err      -> failedBecause ("migration failed: " ++ prettyMigrateFailure err)
  where
    failedBecause e = failed { reason = e }

    db_generic = case CBOR.fromFlatTerm (Value.decode (apiNormalForm startSchema) (TyName root_))
                                        (CBOR.toFlatTerm (CBOR.encode db)) of
                   Right v  -> v
                   Left err -> error err

migrationTests :: TestTree
migrationTests = testGroup "Migration"
  [ testCase     "Basic migration using sample changelog" basicMigrationTest
  , testGroup    "Invalid changes"    $ map applyFailureTest   expectedApplyFailures
  , testGroup    "Invalid migrations" $ map migrateFailureTest expectedMigrateFailures
  , QC.testProperty "Valid migrations (JSON)" validMigrationProperty
  , QC.testProperty "Valid migrations (generic)" validMigrationProperty'
  ]
