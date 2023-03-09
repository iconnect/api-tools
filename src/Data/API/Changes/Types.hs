{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Data.API.Changes.Types
    ( -- * Changelog representation
      APIWithChangelog
    , APIChangelog(..)
    , APIChange(..)
    , MigrationTag
    , VersionExtra(..)
    , showVersionExtra

    , UpdateTypePos(..)
    , UpdateDeclPos(..)
    , APITableChange(..)
    ) where

import           Data.API.PP
import           Data.API.NormalForm
import           Data.API.Types

import           Data.Map ( Map )
import           Data.Version


------------------
-- The key types
--

type APIWithChangelog = (API, APIChangelog)

-- | An API changelog, consisting of a list of versions with the
-- changes from one version to the next.  The versions must be in
-- descending order (according to the 'Ord' 'Version' instance).
data APIChangelog =
     -- | The changes from the previous version up to this version.
     ChangesUpTo VersionExtra [APIChange] APIChangelog
     -- | The initial version
   | ChangesStart Version
    deriving (Eq, Show)

-- | A single change within a changelog
data APIChange
    = ChAddType       TypeName NormTypeDecl
    | ChDeleteType    TypeName
    | ChRenameType    TypeName TypeName

      -- Specific changes for record types
    | ChAddField      TypeName FieldName APIType (Maybe DefaultValue)
    | ChDeleteField   TypeName FieldName
    | ChRenameField   TypeName FieldName FieldName
    | ChChangeField   TypeName FieldName APIType MigrationTag

      -- Changes for union types
    | ChAddUnionAlt    TypeName FieldName APIType
    | ChDeleteUnionAlt TypeName FieldName
    | ChRenameUnionAlt TypeName FieldName FieldName

      -- Changes for enum types
    | ChAddEnumVal    TypeName FieldName
    | ChDeleteEnumVal TypeName FieldName
    | ChRenameEnumVal TypeName FieldName FieldName

      -- Custom migrations
    | ChCustomType    TypeName MigrationTag
    | ChCustomAll     MigrationTag
    deriving (Eq, Show)

instance PPLines APIChange where
  ppLines (ChAddType t d)           = ("added " ++ pp t ++ " ") `inFrontOf` ppLines d
  ppLines (ChDeleteType t)          = ["removed " ++ pp t]
  ppLines (ChRenameType t t')       = ["renamed " ++ pp t ++ " to " ++ pp t']
  ppLines (ChAddField t f ty mb_v)  = [ "changed record " ++ pp t
                                      , "  field added " ++ pp f ++ " :: " ++ pp ty
                                        ++ maybe "" (\ v -> " default " ++ pp v) mb_v]
  ppLines (ChDeleteField t f)       = ["changed record " ++ pp t, "  field removed " ++ pp f]
  ppLines (ChRenameField t f f')    = [ "changed record " ++ pp t
                                      , "  field renamed " ++ pp f ++ " to " ++ pp f']
  ppLines (ChChangeField t f ty c)  = [ "changed record " ++ pp t
                                      , "  field changed " ++ pp f ++ " :: " ++ pp ty
                                        ++ " migration " ++ pp c]
  ppLines (ChAddUnionAlt t f ty)    = [ "changed union " ++ pp t
                                      , "  alternative added " ++ pp f ++ " :: " ++ pp ty]
  ppLines (ChDeleteUnionAlt t f)    = [ "changed union " ++ pp t
                                      , "  alternative removed " ++ pp f]
  ppLines (ChRenameUnionAlt t f f') = [ "changed union " ++ pp t
                                      , "  alternative renamed " ++ pp f ++ " to " ++ pp f']
  ppLines (ChAddEnumVal t f)        = [ "changed enum " ++ pp t
                                      , "  alternative added " ++ pp f]
  ppLines (ChDeleteEnumVal t f)     = [ "changed enum " ++ pp t
                                      , "  alternative removed " ++ pp f]
  ppLines (ChRenameEnumVal t f f')  = [ "changed enum " ++ pp t
                                      , "  alternative renamed " ++ pp f ++ " to " ++ pp f']
  ppLines (ChCustomType t c)        = ["migration record " ++ pp t ++ " " ++ pp c]
  ppLines (ChCustomAll c)           = ["migration " ++ pp c]

-- | Within the changelog, custom migrations are represented as
-- strings, so we have less type-safety.
type MigrationTag = String

-- | Represents either a released version (with a version number) or
-- the version under development, which is newer than any release
data VersionExtra = Release Version
                  | DevVersion
  deriving (Eq, Ord, Show)

showVersionExtra :: VersionExtra -> String
showVersionExtra (Release v) = showVersion v
showVersionExtra DevVersion  = "development"

instance PP VersionExtra where
  pp = showVersionExtra


------------------------
-- Representing updates
--

-- | Represents the positions in a declaration to apply an update
data UpdateDeclPos
    = UpdateHere   (Maybe UpdateDeclPos)
    | UpdateRecord (Map FieldName (Maybe UpdateTypePos))
    | UpdateUnion  (Map FieldName (Maybe UpdateTypePos))
    | UpdateType   UpdateTypePos
    deriving (Eq, Show)

-- | Represents the positions in a type to apply an update
data UpdateTypePos
    = UpdateList UpdateTypePos
    | UpdateSet  UpdateTypePos
    | UpdateMaybe UpdateTypePos
    | UpdateNamed TypeName
    deriving (Eq, Show)

data APITableChange
      -- | An initial API, an APIChange and the positions in which to apply it
    = APIChange NormAPI APIChange (Map TypeName UpdateDeclPos)
      -- | Request to validate the dataset against the given API
    | ValidateData NormAPI
    deriving (Eq, Show)

instance PPLines APITableChange where
  ppLines (APIChange _ c _)  = ppLines c
  ppLines (ValidateData _) = []
