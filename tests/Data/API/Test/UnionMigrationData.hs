{-# LANGUAGE QuasiQuotes #-}

-- | Data for union alternative migration tests
module Data.API.Test.UnionMigrationData
    ( startUnionSchema
    , endUnionSchema
    , testChangelog
    ) where

import           Data.API.Changes
import           Data.API.Parse
import           Data.API.Types


-- Initial schema with a union containing a record type
startUnionSchema :: API
startUnionSchema = [api|

testPrefix :: TestRecord
    = record
        id :: integer

testUnionPrefix :: TestUnion
    = union
        | alt :: TestRecord
|]


-- Final schema and changelog
endUnionSchema :: API
testChangelog :: APIChangelog
(endUnionSchema, testChangelog) = [apiWithChangelog|

testPrefix :: TestRecord
    = record
        id   :: integer
        name :: string

testUnionPrefix :: TestUnion
    = union
        | alt :: TestRecord

changes

version "1.0"
  changed union TestUnion
    alternative changed alt :: TestRecord migration AddNameToTestRecord

version "0"
|]
