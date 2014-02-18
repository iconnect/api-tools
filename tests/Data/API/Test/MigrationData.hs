{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}

-- This module provides data for Data.API.Test.Migration; it is
-- separated out because of the Template Haskell staging restriction.

module Data.API.Test.MigrationData where

import           Data.API.Changes
import           Data.API.JSON
import           Data.API.Parse
import           Data.API.Types

import qualified Data.Aeson as JS
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Version
import           Distribution.Text (simpleParse)


startSchema :: API
startSchema = [api|

id :: Id
    = basic integer

idId :: IdId
    = Id

an_enum :: AnEnum
    = enum
      | foo
      | bar

another_enum :: AnotherEnum
    = enum
      | foo
      | woo

a_union :: AUnion
    = union
      | bar :: Bar

another_union :: AnotherUnion
    = union
      | foo :: Foo
      | woo :: Id

dbs :: DatabaseSnapshot
    = record
        foo   :: ? [Foo]        nothing
        bar   :: ? [Bar]        []
        recur :: ? [Recursive]

fooPrefix :: Foo
    = record
        id   :: Id
        nest :: Nested
        en   :: AnEnum
        un   :: AUnion
        quux :: ? IdId

barPrefix :: Bar
    = record
       id :: Id

nestedPrefix :: Nested
    = record
       id :: Id

recursive :: Recursive
    = record
      id    :: Id
      recur :: ? Recursive

|]


endSchema :: API
changelog :: APIChangelog
(endSchema, changelog) = [apiWithChangelog|

id :: Id
    = basic integer

idId :: IdId
    = Id

bin :: Binary
    = basic binary

an_enum :: AnEnum
    = enum
      | foofoo
      | bar
      | baz

another_enum :: AnotherEnum
    = enum
      | foo
      | woo
      | barbar

a_union :: AUnion
    = union
      | barbar :: RenamedBar
      | baz :: Baz

another_union :: AnotherUnion
    = union
      | foo :: Foo
      | woo :: Id
      | barbar :: RenamedBar

dbs :: DatabaseSnapshot
    = record
        foo   :: ? [Foo]
        bar2  :: ? [Bar2]
        boz   :: ? [Baz]
        recur :: ? [Recursive]
        recur2 :: ? [DuplicateRecursive]

fooPrefix :: Foo
    = record
        id   :: Id
        nest :: RenamedNested
        en   :: AnEnum
        un   :: AUnion
        c    :: string
        nolist  :: [string]
        nomaybe :: ? string

barPrefix :: RenamedBar
    = record
       id :: Id

bar2Prefix :: Bar2
    = record
       id :: Id

nestedPrefix :: RenamedNested
    = record
       id :: Id
       new :: string

bazPrefix :: Baz
    = record
       id :: Id
       yy :: string

recursive :: Recursive
    = record
        renamed_id :: Id
        recur      :: ? Recursive
        new        :: string
        newnew     :: string

duplicaterecursive :: DuplicateRecursive
    = record
        renamed_id :: Id
        recur      :: ? Recursive
        new        :: string

new :: New
    = basic integer

changes

version "development"
  // changes since last release
  added New basic integer

version "2.6"
  // add fields with implicit default values
  changed record Foo
    field added nolist  :: [string]
    field added nomaybe :: ? string

version "2.5"
  // schema-changing custom record migration
  migration record Recursive DuplicateNew

version "2.4"
  // schema-changing custom database migration
  migration DuplicateRecursive

version "2.3"
  // changing a recursively defined record
  changed record Recursive
    field renamed id to renamed_id
    field added new :: string default "hello"

version "2.2"
  // changing a nested union
  changed union AUnion
    alternative renamed bar to barbar

version "2.1"
  // Changing a nested enum
  changed enum AnEnum
    alternative renamed foo to foofoo

version "2.0"
  // Changing a nested record
  changed record RenamedNested
    field added new :: string default "hello"

version "1.9"
  // Deleting a table
  changed record DatabaseSnapshot
    field removed bar

version "1.8"
  // Renaming a table
  changed record DatabaseSnapshot
    field renamed baz to boz

version "1.7"
  // Adding a table
  changed record DatabaseSnapshot
    field added baz :: ? [Baz] default []

version "1.6"
  // Custom change to the whole database
  migration DuplicateBar
  changed record DatabaseSnapshot
    field added bar2 :: ? [Bar2] default nothing
  added Bar2 record
    id :: Id

version "1.5"
  // Custom change to a record
  migration record Foo CopyIDtoC

version "1.4"
  // Renaming enum values
  changed enum AnotherEnum
    alternative renamed bar to barbar

version "1.3"
  // Deleting enum values
  changed enum AnotherEnum
    alternative removed baz

version "1.2"
  // Adding enum values
  changed enum AnEnum
    alternative added baz
  changed enum AnotherEnum
    alternative added bar
    alternative added baz

version "1.1"
  // Renaming union alternatives
  changed union AnotherUnion
    alternative renamed bar to barbar

version "1.0"
  // Deleting union alternatives
  changed union AnotherUnion
    alternative removed baz

version "0.9"
  // Adding union alternatives
  changed union AUnion
    alternative added baz :: Baz
  changed union AnotherUnion
    alternative added bar :: RenamedBar
    alternative added baz :: Baz

version "0.8"
  // Custom migration on fields
  changed record Foo
    field changed c :: string migration ConvertBinaryToString

version "0.7"
  // Renaming fields
  changed record Foo
    field renamed b to c
  changed record Baz
    field renamed y to yy

version "0.6"
  // Deleting fields
  changed record Foo
    field removed quux
  changed record Baz
    field removed x

version "0.5"
  // Adding fields
  changed record Foo
    field added b :: Binary default "Zm9vYmFy"
  changed record Baz
    field added x :: integer
    field added y :: string

version "0.4"
  // Renaming types
  renamed Nested to RenamedNested
  renamed Bar to RenamedBar

version "0.3"
  // Deleting types
  removed ASynonym

version "0.2"
  // Adding types
  added ASynonym Binary
  added Binary basic binary
  added Baz record
    id :: Id

version "0.1"
  // No changes

version "0"

|]


badChangelog :: APIChangelog
(_, badChangelog) = [apiWithChangelog|
changes

version "3.1"
  // Adding a table without a default
  changed record DatabaseSnapshot
    field added badtable :: Nested

version "3.0"
  // Renaming to an existing val
  changed enum AnotherEnum
    alternative renamed foo to woo

version "2.9"
  // Renaming a missing val
  changed enum AnotherEnum
    alternative renamed missing to also_missing

version "2.8"
  // Deleting a missing val
  changed enum AnotherEnum
    alternative removed missing

version "2.7"
  // Adding a val that already exists
  changed enum AnEnum
    alternative added bar

version "2.6"
  // Removing vals from a type in use
  changed enum AnEnum
    alternative removed foo

version "2.5"
  // Adding vals to a non-existent type
  changed enum NotThere
    alternative added oops

version "2.4"
  // Adding vals to a non-enum
  changed enum Id
    alternative added oops

version "2.3"
  // Renaming to an existing alt
  changed union AnotherUnion
    alternative renamed foo to woo

version "2.2"
  // Renaming a missing alt
  changed union AnotherUnion
    alternative renamed missing to also_missing

version "2.1"
  // Deleting a missing alt
  changed union AnotherUnion
    alternative removed missing

version "2.0"
  // Adding an alt that already exists
  changed union AUnion
    alternative added bar :: Bar

version "1.9"
  // Adding an alt with a malformed type
  changed union AUnion
    alternative added oops :: Missing

version "1.8"
  // Removing alts from a type in use
  changed union AUnion
    alternative removed foo

version "1.7"
  // Adding alts to a non-existent type
  changed union NotThere
    alternative added oops :: Id

version "1.6"
  // Adding alts to a non-union
  changed union Id
    alternative added oops :: Id

version "1.5"
  // Renaming to an existing field
  changed record Foo
    field renamed en to un

version "1.4"
  // Renaming a missing field
  changed record Foo
    field renamed missing to also_missing

version "1.3"
  // Deleting a missing field
  changed record Foo
    field removed missing

version "1.2"
  // Adding a field with an ill-typed default value
  changed record Foo
    field added oops :: Id default "hello"

version "1.1"
  // Adding a field without a default value
  changed record Foo
    field added oops :: Id

version "1.0"
  // Adding a field that already exists
  changed record Foo
    field added id :: Id

version "0.9"
  // Adding a field with a malformed type
  changed record Foo
    field added oops :: Missing

// // This is now legal:
// version "0.8"
//  // Adding fields to a type in use
//  changed record Nested
//    field added oops :: Id

version "0.7"
  // Adding fields to a non-existent type
  changed record NotThere
    field added oops :: Id

version "0.6"
  // Adding fields to a non-record
  changed record Id
    field added oops :: Id

version "0.5"
  // Renaming to an existing type
  renamed Id to AnEnum

version "0.4"
  // Renaming a missing type
  renamed NotThere to SomethingElse

version "0.3"
  // Deleting a missing type
  removed NotThere

version "0.2"
  // Adding an ill-formed type
  added New Missing

version "0.1"
  // Adding a type that already exists
  added Id basic binary

version "0"
|]

expectedApplyFailures :: [(Version, Version, ApplyFailure)]
expectedApplyFailures = map toVersions $
  [ ("0",   "0.1", TypeExists (TypeName "Id"))
  , ("0.1", "0.2", DeclMalformed (TypeName "New")
                       (NTypeSynonym (TyName (TypeName "Missing")))
                       (Set.singleton (TypeName "Missing")))
  , ("0.2", "0.3", TypeDoesNotExist (TypeName "NotThere"))
  , ("0.3", "0.4", TypeDoesNotExist (TypeName "NotThere"))
  , ("0.4", "0.5", TypeExists (TypeName "AnEnum"))
  , ("0.5", "0.6", TypeWrongKind (TypeName "Id") TKRecord)
  , ("0.6", "0.7", TypeDoesNotExist (TypeName "NotThere"))
--  , ("0.7", "0.8", TypeInUse (TypeName "Nested"))
  , ("0.8", "0.9", TypeMalformed
                       (TyName (TypeName "Missing"))
                       (Set.singleton (TypeName "Missing")))
  , ("0.9", "1.0", FieldExists (TypeName "Foo") TKRecord (FieldName "id"))
  , ("1.0", "1.1", DefaultMissing (TypeName "Foo") (FieldName "oops"))
  , ("1.1", "1.2", FieldBadDefaultValue (TypeName "Foo") (FieldName "oops")
                       (TyName (TypeName "Id")) (DefValString (T.pack "hello")))
  , ("1.2", "1.3", FieldDoesNotExist (TypeName "Foo") TKRecord (FieldName "missing"))
  , ("1.3", "1.4", FieldDoesNotExist (TypeName "Foo") TKRecord (FieldName "missing"))
  , ("1.4", "1.5", FieldExists (TypeName "Foo") TKRecord (FieldName "un"))
  , ("1.5", "1.6", TypeWrongKind (TypeName "Id") TKUnion)
  , ("1.6", "1.7", TypeDoesNotExist (TypeName "NotThere"))
  , ("1.7", "1.8", TypeInUse (TypeName "AUnion"))
  , ("1.8", "1.9", TypeMalformed
                       (TyName (TypeName "Missing"))
                       (Set.singleton (TypeName "Missing")))
  , ("1.9", "2.0", FieldExists (TypeName "AUnion") TKUnion (FieldName "bar"))
  , ("2.0", "2.1", FieldDoesNotExist (TypeName "AnotherUnion") TKUnion (FieldName "missing"))
  , ("2.1", "2.2", FieldDoesNotExist (TypeName "AnotherUnion") TKUnion (FieldName "missing"))
  , ("2.2", "2.3", FieldExists (TypeName "AnotherUnion") TKUnion (FieldName "woo"))
  , ("2.3", "2.4", TypeWrongKind (TypeName "Id") TKEnum)
  , ("2.4", "2.5", TypeDoesNotExist (TypeName "NotThere"))
  , ("2.5", "2.6", TypeInUse (TypeName "AnEnum"))
  , ("2.6", "2.7", FieldExists (TypeName "AnEnum") TKEnum (FieldName "bar"))
  , ("2.7", "2.8", FieldDoesNotExist (TypeName "AnotherEnum") TKEnum (FieldName "missing"))
  , ("2.8", "2.9", FieldDoesNotExist (TypeName "AnotherEnum") TKEnum (FieldName "missing"))
  , ("2.9", "3.0", FieldExists (TypeName "AnotherEnum") TKEnum (FieldName "woo"))
  , ("3.0", "3.1", DefaultMissing (TypeName "DatabaseSnapshot") (FieldName "badtable"))
  ]
  where
    toVersions (s, s', x)
      | Just v  <- simpleParse s
      , Just v' <- simpleParse s'
      , v < v'                    = (v, v', x)
      | otherwise = error $ "expectedApplyFailures: bad versions "
                            ++ show s ++ " and " ++ show s'

type MigrateFailureTest = ( String
                          , (API, Version)  -- Start version
                          , (API, VersionExtra)  -- End version
                          , APIChangelog
                          , JS.Value
                          , MigrateFailure -> Bool )

expectedMigrateFailures :: [MigrateFailureTest]
expectedMigrateFailures =
  [ ( "Out of order"
    , (startSchema, ver "0.1")
    , (endSchema, verRelease "0.2")
    , outOfOrder
    , startData
    , (== ValidateFailure (ChangelogOutOfOrder (verRelease "0.1") (verRelease "0.2")))
    )

  , ("Incomplete"
    , (startSchema, ver "0.1")
    , (endSchema, verRelease "2.5")
    , incomplete
    , startData
    , \ err -> case err of
                 ValidateFailure (ChangelogIncomplete _ _ _) -> True
                 _                                           -> False
    )

  , ( "Downgrade"
    , (startSchema, ver "0.2")
    , (startSchema, verRelease "0.1")
    , changelog
    , startData
    , (== ValidateFailure (CannotDowngrade (verRelease "0.2") (verRelease "0.1")))
    )

  , ("Bad custom migration"
    , (startSchema, ver "0.1")
    , (startSchema, verRelease "0.2")
    , badCustomMigration
    , startData
    , \ err -> case err of
                 ValueError (JSONError UnexpectedField) [InField t] -> t == "bar2"
                 _                                                  -> False
    )
  ]
  where
    ver s | Just v <- simpleParse s = v
          | otherwise = error $ "expectedValidateFailures: bad version " ++ show s
    verRelease s = Release $ ver s

    outOfOrder = snd [apiWithChangelog|
changes
version "0.1"
version "0.2"
|]

    incomplete = snd [apiWithChangelog|
changes
version "0.1"
|]

    badCustomMigration = snd [apiWithChangelog|
changes
version "0.2"
  migration DuplicateBar
version "0.1"
|]


startData, endData :: JS.Value
Just startData = JS.decode "{ \"foo\": [ {\"id\": 42, \"nest\": { \"id\": 3 }, \"en\": \"foo\", \"un\": { \"bar\": { \"id\": 43 } }, \"quux\": null } ], \"bar\": [ { \"id\": 4 } ], \"recur\": [{ \"id\": 9, \"recur\": { \"id\": 8, \"recur\": null} }] }"
Just endData = JS.decode "{ \"foo\": [ {\"id\":42, \"nest\": { \"id\": 3, \"new\": \"hello\" }, \"c\": \"foobar42\", \"en\": \"foofoo\", \"un\": { \"barbar\": { \"id\": 43 } }, \"nolist\": [], \"nomaybe\": null } ], \"boz\": [], \"bar2\": [ {\"id\": 4 } ], \"recur\": [{ \"renamed_id\": 9, \"new\": \"hello\", \"newnew\": \"hello\", \"recur\": { \"renamed_id\": 8, \"new\": \"hello\", \"newnew\": \"hello\", \"recur\": null} }], \"recur2\": [{ \"renamed_id\": 9, \"new\": \"hello\", \"recur\": { \"renamed_id\": 8, \"new\": \"hello\", \"newnew\": \"hello\", \"recur\": null} }] }"

startVersion :: Version
startVersion = changelogStartVersion changelog

root_ :: TypeName
root_ = TypeName "DatabaseSnapshot"
