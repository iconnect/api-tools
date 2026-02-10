{-# LANGUAGE QuasiQuotes #-}

-- | Data for union alternative migration tests
--
-- This module tests the 'alternative changed' changelog feature, which allows
-- swapping the type of a union alternative from one type to a completely
-- different type, with a custom migration function to transform the data.
module Data.API.Test.UnionMigrationData
    ( -- * Type swap scenario (PersonV1 -> PersonV2)
      startTypeSwapSchema
    , endTypeSwapSchema
    , typeSwapChangelog
    ) where

import           Data.API.Changes
import           Data.API.Parse
import           Data.API.Types


-- -----------------------------------------------------------------------------
-- Type Swap Scenario
--
-- This tests the primary use case: migrating a union alternative from one
-- type (PersonV1) to a completely different type (PersonV2).
--
-- PersonV1 has: name :: string
-- PersonV2 has: fullName :: string, age :: integer
--
-- The migration function transforms PersonV1 data to PersonV2 data.
-- -----------------------------------------------------------------------------

-- | Initial schema with PersonV1
startTypeSwapSchema :: API
startTypeSwapSchema = [api|

personV1Prefix :: PersonV1
    = record
        name :: string

containerPrefix :: Container
    = record
        person :: MyUnion

myUnionPrefix :: MyUnion
    = union
        | person :: PersonV1
        | other  :: integer
|]


-- | Final schema with PersonV2 and changelog
endTypeSwapSchema :: API
typeSwapChangelog :: APIChangelog
(endTypeSwapSchema, typeSwapChangelog) = [apiWithChangelog|

personV1Prefix :: PersonV1
    = record
        name :: string

personV2Prefix :: PersonV2
    = record
        fullName :: string
        age      :: integer

containerPrefix :: Container
    = record
        person :: MyUnion

myUnionPrefix :: MyUnion
    = union
        | person :: PersonV2
        | other  :: integer

changes

version "1.0"
  // Note: changes are processed bottom-up, so we must list the union change
  // before adding the new type it references
  changed union MyUnion
    alternative changed person :: PersonV2 migration MigratePersonV1ToV2
  added PersonV2 record
    fullName :: string
    age      :: integer

version "0"
|]
