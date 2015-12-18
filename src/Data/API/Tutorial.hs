{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Data.API.Tutorial (
    -- * Introduction
    -- $introduction

    -- * The api DSL
    -- $api

    -- * Generating types for an API
    -- $generate

    -- ** Custom representation types
    -- $with

    -- * Generating tools for an API
    -- $tools

    -- ** Custom API tools
    -- $custom-tools

    -- * Data migration
    -- $migration

    -- ** Custom migrations
    -- $custom

    -- * Documenting a REST-like API
    -- $docs
    ) where

import Data.API.Changes
import Data.API.Doc.Call
import Data.API.Doc.Dir
import Data.API.Doc.Types
import Data.API.JSON
import Data.API.Parse
import Data.API.Tools
import Data.API.Tools.Combinators
import Data.API.Types

import Data.Text
import Data.Time

{- $introduction

The @api-tools@ library provides a compact DSL for describing an API.
It uses Template Haskell to generate the corresponding data types and
assorted tools for working with it, including code for converting
between JSON and the generated types and writing unit tests.  It
supports maintaining a log of changes to the API and migrating data
between different versions.

-}

{- $api

An 'API' is a list of datatypes, which must be in one of five
simple forms:

 * Records, which have one constructor but many fields

 * Unions, which have many constructors each with a single argument

 * Enumerations, which have many nullary constructors

 * Newtypes, which wrap a built-in basic type

 * Type synonyms

To define an 'API', you can use the 'parseAPI' function or the 'api'
quasi-quoter, like this:

> example :: API
> example = [api|
>
> rec :: MyRecord
>    // A record type containing two fields
>    = record
>        x :: [integer]   // one field
>        y :: ? [utc]     // another field
>
> chc :: MyChoice
>     // A disjoint union
>     = union
>         | a :: MyRecord
>         | b :: string
>
> enm :: MyEnum
>     // An enumeration
>     = enum
>         | e1
>         | e2
>
> str :: MyString
>     // A newtype
>     = basic string
>
> flg :: MyFlag
>     // A type synonym
>     = boolean
>
> |]

The basic types available (and their Haskell representations) are
@string@ ('Text'), @binary@ ('Binary'), @integer@ ('Int'), @boolean@
('Bool') and @utc@ ('UTCTime').

The prefix (given before the @::@ on each type declaration) is used to
name record fields and enumeration/union constructors in the generated
Haskell code.  It must be unique throughout the API.  It is not a type
signature, despite the appearance!

-}

{- $generate

Once an API is defined, the 'generate' function can be used in a
Template Haskell splice to produce the corresponding Haskell datatype
declarations.  Thus @$(generate example)@ will produce something like:

> data MyRecord = MyRecord { rec_x :: [Int]
>                          , rec_y :: Maybe [UTCTime]
>                          }
>
> data MyChoice = CHC_a MyRecord | CHC_b String
>
> data MyEnum = ENM_e1 | ENM_e2
>
> newtype MyString = MyString { _MyString :: String }
>
> type MyFlag = Bool

The Template Haskell staging restriction means that @example@ must be
defined in one module and imported into another to call 'generate'.

-}

{- $with

For some types, it may be desirable to use a different datatype in the
Haskell code, rather than relying on the generated datatype.  For
example, this allows collection types (such as sets) to be used in
place of lists, or allows additional invariants to be enforced.  The
JSON serialization agrees with the schema (so the difference is
invisible to non-Haskell clients).  This is possible using a @with@
clause in the schema DSL, which follows the type declaration and gives
the names of a pair of conversion functions, like this:

> i :: IDSet
>     = record
>         ids :: [ID]
>     with inj_IDSet, prj_IDSet

When a @with@ clause is used, the usual generated type (the
representation type) will be given a @REP__@ prefix, so in this case
'generate' will produce:

> data REP__IDSet = REP__IDSet { _i_ids :: [ID] }

The Haskell code accompanying the call to 'generate' should define a
type called @IDSet@, and functions
@inj_IDSet :: 'Monad' m => REP__IDSet -> m IDSet@ and
@prj_IDSet :: IDSet -> REP__IDSet@, which will be used when converting
to and from JSON.  The monadic type allows parsing to 'fail' if
the representation type uses a value that is not permitted, but
converting back must be a pure function.

-}

{- $tools

Once the Haskell datatypes have been created by 'generate', additional
tools can be created with 'generateAPITools'.  See "Data.API.Tools"
for a list of tools supplied with the library.  For example, the call

> $(generateAPITools [enumTool, jsonTool, quickCheckTool] example)

will define:

* @_text_MyEnum :: MyEnum -> 'Text'@ for converting an enumeration to
  its textual representation;

* @_map_MyEnum :: Map 'Text' MyEnum@ for converting a textual
  representation back to an enumeration; and

* 'ToJSON', 'FromJSONWithErrs' and 'Arbitrary' instances for all the
  generated types.

Note that 'generate' must be used to create the datatypes first,
otherwise 'generateAPITools' will result in scope errors.  Moreover,
certain tools have dependencies, as described in the documentation for
each tool in "Data.API.Tools".  Dependent tools must be generated in
the same call to 'generateAPITools' or a previous call; if they are
missing unpleasant compilation errors will occur.  For example,
omitting 'enumTool' in the above to give

> $(generateAPITools [jsonTool, quickCheckTool] example)

will lead to errors about undefined symbols @_text_MyEnum@ and
@_map_MyEnum@ in the generated code.

-}

{- $custom-tools

An 'APITool' is essentially just a function that consumes an 'API' and
produces some Template Haskell declarations.  More generally, a
@'Tool' a@ consumes a value of type @a@ and generates some
declarations.  The utilities in "Data.API.Tools.Combinators" make it
slightly more convenient to construct such things.  In particular, you
can use @'apiNodeTool' ('apiSpecTool' n r u e t)@ to build a tool that
consumes an 'API' out of tools that explain what to do for newtypes,
records, unions, enumerations and type synonyms respectively.

Typically, tools generate declarations or class instances based on
each different node in the API. These generated declarations work with
the datatypes generated by 'datatypesTool'.  The module
"Data.API.Tools.Datatypes" provides functions to turn 'APINode's and
their components into Template Haskell references to the generated
datatypes.

-}

{- $migration

A key feature of @api-tools@ is support for migrating data between
different versions of an 'API'. The 'apiWithChangelog' quasi-quoter
allows an API to be followed by a changelog in a formal syntax,
providing a record of changes between versions.  For example:

> example :: API
> exampleChangelog :: APIChangelog
> (example, exampleChangelog) = [apiWithChangelog|
>
> // ...api specification as before...
>
> changes
>
> version "0.3"
>   added MyFlag boolean
>
> version "0.2"
>   changed record MyRecord
>     field added y :: ? [utc]
>
> // Initial version
> version "0.1"
> |]

The 'migrateDataDump' function can be used to migrate data, encoded
with JSON, from a previous API version to a more recent version.  The
old and new 'API's must be supplied, and the changes in the changelog
must describe how to get from the old to the new 'API'.  The
'validateChanges' function can be used to check that a changelog is
sufficient.

A changelog consists of the keyword @changes@ and a list of version
blocks. A version block consists of the keyword @version@ starting in
the leftmost column, a version number in double quotes, then a list of
changes. The following changes are available:

> added <Type name> <Specification>
> removed <Type name>
> renamed <Source type> to <Target type>
> changed record <Type name>
>   field added <field name> :: <Type> [default <value>]
>   field removed <field name>
>   field renamed <source field> to <target field>
>   field changed <field name> :: <New type> migration <Migration name>
> changed union <Type name>
>   alternative added <alternative name> :: <Type>
>   alternative removed <alternative name>
>   alternative renamed <source alternative> to <target alternative>
> changed enum <Type name>
>   alternative added <value name>
>   alternative removed <value name>
>   alternative renamed <source value> to <target value>
> migration <Migration name>
> migration record <Type name> <Migration name>

-}

{- $custom

For more extensive changes to the 'API' that cannot be expressed using
the primitive changes, /custom/ migrations can be used to migrate data
between versions.

Custom migrations can be applied to the whole dataset, a single type
or an individual record field, thus:

> version "0.42"
>   migration MigrateWholeDataset
>   migration record Widget MigrateWidgetType
>   changed record Widget where
>     field changed foo :: String migration MigrateFooField

The 'generateMigrationKinds' function creates enumeration types
corresponding to the custom migration names used in a changelog.
These types should then be used to create a 'CustomMigrations' record,
which describes how to transform the data (and 'API', if appropriate)
for each custom migration.  For example,

> $(generateMigrationKinds myChangelog "DatabaseMigration" "TypeMigration" "FieldMigration")

with the changelog fragment above would give

> data DatabaseMigration = MigrateWholeDatabase | ...
> data TypeMigration     = MigrateWidgetType    | ...
> data FieldMigration    = MigrateFooField      | ...

Calls to 'migrateDataDump' should include a suitable
'CustomMigrations' record, which includes functions to perform the
migrations on the underlying data, represented as an Aeson 'Value'.
For example, suppose the @foo@ field of the @Widget@ record previously
contained a boolean: a suitable 'fieldMigration' implementation might be:

> fieldMigration :: FieldMigration -> Value -> Either ValueError Value
> fieldMigration MigrateFooField (Bool b) = Right $ toJSON $ show b
> fieldMigration MigrateFooField v        = Left  $ CustomMigrationError "oops" v
> ...

A field migration may change the type of the field by listing the new
type in the changelog.  Whole-database and individual-type migrations
may describe the changes they make to the schema in the
'databaseMigrationSchema' and 'typeMigrationSchema' fields of the
'CustomMigrations' record.

In order to check that custom migrations result in data that matches
the schema, the 'DataChecks' parameter of 'migrateDataDump' can be set
to 'CheckCustom' or 'CheckAll'.  This will validate the data against
the schema after calling the custom migration code.

-}

{- $docs

A 'Call' is a description of a web resource, intended to be generated
in an application-specific way from the code of a web server.  The
'callHtml' function can be used to generate HTML documentation of
individual resources, and 'dirHtml' can be used to generate an index
page for the documentation of a collection of resources.

-}
