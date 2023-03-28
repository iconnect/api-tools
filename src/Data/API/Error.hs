{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Data.API.Error
    ( -- * Representation of JSON parsing errors
      JSONError(..)
    , JSONWarning
    , Expected(..)
    , FormatExpected(..)
    , Position
    , Step(..)
    , inField
    , prettyJSONErrorPositions
    , prettyJSONError
    , prettyStep

      -- * JSON parse error construction
    , expectedArray
    , expectedSet
    , expectedBool
    , expectedInt
    , expectedObject
    , expectedString
    , badFormat

      -- * Validation and migration errors
    , ValueError(..)
    , ValidateFailure(..)
    , ValidateWarning
    , ApplyFailure(..)
    , TypeKind(..)
    , MigrateFailure(..)
    , MigrateWarning
    , prettyMigrateFailure
    , prettyValidateFailure
    , prettyValueError
    , prettyValueErrorPosition
    ) where

import           Data.API.Changes.Types
import           Data.API.PP
import           Data.API.NormalForm
import           Data.API.Types
import           Data.API.Utils

import qualified Data.Aeson                     as JS
import           Data.Aeson.TH
import qualified Data.Graph                     as Graph
import           Data.List
import           Data.Map ( Map )
import qualified Data.Map                       as Map
import qualified Data.SafeCopy                  as SC
import           Data.Set ( Set )
import qualified Data.Set                       as Set
import qualified Data.Text                      as T
import           Data.Time

----------------------------------------------------------
-- Representation of JSON parsing errors and positions
--

-- | Represents an error that can be encountered while parsing
data JSONError = Expected  Expected       String JS.Value
               | BadFormat FormatExpected String T.Text
               | MissingField
               | MissingAlt [String]
               | UnexpectedField
               | UnexpectedEnumVal [T.Text] T.Text
               | IntRangeError String Int IntRange
               | UTCRangeError String UTCTime UTCRange
               | RegexError String T.Text RegEx
               | SyntaxError String
  deriving (Eq, Show)

-- | At present, we do not distinguish between errors and warnings
type JSONWarning = JSONError

-- | JSON type expected at a particular position, when a value of a
-- different type was encountered
data Expected = ExpArray
              | ExpBool
              | ExpInt
              | ExpObject
              | ExpString
  deriving (Eq, Show)

-- | Special format expected of a string
data FormatExpected = FmtBinary
                    | FmtUTC
                    | FmtOther
  deriving (Eq, Show)

expectedArray, expectedSet, expectedBool, expectedInt, expectedObject, expectedString
  :: JS.Value -> JSONError
expectedArray  = Expected ExpArray    "Array"
expectedBool   = Expected ExpBool     "Bool"
expectedInt    = Expected ExpInt      "Int"
expectedObject = Expected ExpObject   "Object"
expectedString = Expected ExpString   "String"
expectedSet    = Expected ExpObject   "Set"

badFormat :: String -> T.Text -> JSONError
badFormat = BadFormat FmtOther

-- | Human-readable description of a JSON parse error
prettyJSONError :: JSONError -> String
prettyJSONError (Expected _ s v)      = "When expecting " ++ s ++ ", encountered "
                                        ++ x ++ " instead"
  where
    x = case v of
          JS.Object _ -> "Object"
          JS.Array _  -> "Array"
          JS.String _ -> "String"
          JS.Number _ -> "Number"
          JS.Bool _   -> "Boolean"
          JS.Null     -> "Null"
prettyJSONError (BadFormat _ s t)     = "Could not parse as " ++ s ++ " the string " ++ show t
prettyJSONError MissingField          = "Field missing from Object"
prettyJSONError (MissingAlt xs)       = "Missing alternative, expecting one of: "
                                          ++ intercalate ", " xs
prettyJSONError UnexpectedField       = "Unexpected field in Object"
prettyJSONError (UnexpectedEnumVal xs t) = "Unexpected enum value " ++ show t
                                           ++ ", expecting one of: "
                                           ++ T.unpack (T.intercalate ", " xs)
prettyJSONError (IntRangeError s i r) = s ++ ": " ++ show i ++ " not in range " ++ show r
prettyJSONError (UTCRangeError s u r) = s ++ ": " ++ show u ++ " not in range " ++ show r
prettyJSONError (RegexError s _ t)    = s ++ ": failed to match RE: " ++ show t
prettyJSONError (SyntaxError e)       = "JSON syntax error: " ++ e

-- | A position inside a JSON value is a list of steps, ordered
-- innermost first (so going inside an object prepends a step).
type Position = [Step]

-- | Each step may be into a field of an object, or a specific element
-- of an array.
data Step = InField T.Text | InElem Int
  deriving (Eq, Show)

inField :: FieldName -> Step
inField fn = InField (_FieldName fn)

-- | Human-readable description of a single step in a position
prettyStep :: Step -> String
prettyStep (InField f) = "  in the field " ++ show f
prettyStep (InElem i)  = "  in array index " ++ show i

instance PPLines Step where
  ppLines s = [prettyStep s]

-- | Human-readable presentation of a list of parse errors with their
-- positions
prettyJSONErrorPositions :: [(JSONError, Position)] -> String
prettyJSONErrorPositions xs = unlines $ concatMap help xs
  where
    help (e, pos) = prettyJSONError e : map prettyStep pos



----------------------------------------------------------
-- Validation and data migration errors
--

-- | Errors that can be discovered when migrating data values
data ValueError
    = JSONError JSONError                  -- ^ Data doesn't match schema
    | CustomMigrationError String JS.Value -- ^ Error generated during custom migration
    | InvalidAPI ApplyFailure              -- ^ An API change was invalid
    deriving (Eq, Show)

-- | Errors that may be discovered when validating a changelog
data ValidateFailure
        -- | the changelog must be in descending order of versions
    = ChangelogOutOfOrder { vfLaterVersion   :: VersionExtra
                          , vfEarlierVersion :: VersionExtra }
        -- | forbid migrating from one version to an earlier version
    | CannotDowngrade { vfFromVersion :: VersionExtra
                      , vfToVersion   :: VersionExtra }
        -- | an API uses types that are not declared
    | ApiInvalid { vfInvalidVersion      :: VersionExtra
                 , vfMissingDeclarations :: Set TypeName }
        -- | changelog entry does not apply
    | ChangelogEntryInvalid { vfSuccessfullyApplied :: [APITableChange]
                            , vfFailedToApply       :: APIChange
                            , vfApplyFailure        :: ApplyFailure }
        -- | changelog is incomplete
        --   (ie all entries apply ok but result isn't the target api)
    | ChangelogIncomplete { vfChangelogVersion :: VersionExtra
                          , vfTargetVersion    :: VersionExtra
                          , vfDifferences      :: Map TypeName (MergeResult NormTypeDecl NormTypeDecl) }
  deriving (Eq, Show)

data ValidateWarning = ValidateWarning -- add warnings about bits we cannot check (opaque custom)
  deriving Show

-- | Errors that may occur applying a single API change
data ApplyFailure
    = TypeExists           { afExistingType  :: TypeName }     -- ^ for adding or renaming type
    | TypeDoesNotExist     { afMissingType   :: TypeName }     -- ^ for deleting or renaming a type
    | TypeWrongKind        { afTypeName      :: TypeName
                           , afExpectedKind  :: TypeKind }     -- ^ e.g. it's not a record type
    | TypeInUse            { afTypeName      :: TypeName }     -- ^ cannot delete/modify types that are still used
    | TypeMalformed        { afType          :: APIType
                           , afMissingTypes  :: Set TypeName } -- ^ type refers to a non-existent type
    | DeclMalformed        { afTypeName      :: TypeName
                           , afDecl          :: NormTypeDecl
                           , afMissingTypes  :: Set TypeName } -- ^ decl refers to a non-existent type
    | FieldExists          { afTypeName      :: TypeName
                           , afTypeKind      :: TypeKind
                           , afExistingField :: FieldName }    -- ^ for adding or renaming a field
    | FieldDoesNotExist    { afTypeName      :: TypeName
                           , afTypeKind      :: TypeKind
                           , afMissingField  :: FieldName }    -- ^ for deleting or renaming a field
    | FieldBadDefaultValue { afTypeName      :: TypeName
                           , afFieldName     :: FieldName
                           , afFieldType     :: APIType
                           , afBadDefault    :: DefaultValue } -- ^ for adding a field, must be a default
                                                               --   value compatible with the type
    | DefaultMissing       { afTypeName      :: TypeName
                           , afFieldName     :: FieldName }    -- ^ for adding a field to a table
    | TableChangeError     { afCustomMessage :: String }       -- ^ custom error in tableChange
  deriving (Eq, Show)

data TypeKind = TKRecord | TKUnion | TKEnum | TKNewtype | TKTypeSynonym
  deriving (Eq, Show)


data MigrateFailure
    = ValidateFailure ValidateFailure
    | ValueError ValueError Position
    deriving (Eq, Show)

type MigrateWarning = ValidateWarning


-------------------------------------
-- Pretty-printing
--

prettyMigrateFailure :: MigrateFailure -> String
prettyMigrateFailure = unlines . ppLines

prettyValidateFailure :: ValidateFailure -> String
prettyValidateFailure = unlines . ppLines

prettyValueError :: ValueError -> String
prettyValueError = unlines . ppLines

prettyValueErrorPosition :: (ValueError, Position) -> String
prettyValueErrorPosition = unlines . ppLines


instance PP TypeKind where
  pp TKRecord      = "record"
  pp TKUnion       = "union"
  pp TKEnum        = "enum"
  pp TKNewtype     = "newtype"
  pp TKTypeSynonym = "type"

ppATypeKind :: TypeKind -> String
ppATypeKind TKRecord      = "a record"
ppATypeKind TKUnion       = "a union"
ppATypeKind TKEnum        = "an enum"
ppATypeKind TKNewtype     = "a newtype"
ppATypeKind TKTypeSynonym = "a type synonym"

ppMemberWord :: TypeKind -> String
ppMemberWord TKRecord      = "field"
ppMemberWord TKUnion       = "alternative"
ppMemberWord TKEnum        = "value"
ppMemberWord TKNewtype     = "member"
ppMemberWord TKTypeSynonym = "member"


instance PPLines MigrateFailure where
  ppLines (ValidateFailure x) = ppLines x
  ppLines (ValueError x ps)   = ppLines x ++ map prettyStep ps

instance PPLines ValidateFailure where
  ppLines (ChangelogOutOfOrder later earlier) =
      ["Changelog out of order: version " ++ pp later
           ++ " appears after version " ++ pp earlier]
  ppLines (CannotDowngrade from to) =
      ["Cannot downgrade from version " ++ pp from
           ++ " to version " ++ pp to]
  ppLines (ApiInvalid ver missing) =
      ["Missing declarations in API version " ++ pp ver ++ ": " ++ pp missing]
  ppLines (ChangelogEntryInvalid succs change af) =
      ppLines af ++ ("when applying the change" : indent (ppLines change))
          ++ if not (null succs)
             then "after successfully applying the changes:"
                  : indent (ppLines succs)
             else []
  ppLines (ChangelogIncomplete ver ver' diffs) =
      ("Changelog incomplete! Differences between log version ("
           ++ showVersionExtra ver ++ ") and latest version (" ++ showVersionExtra ver' ++ "):")
      : indent (ppDiffs diffs)


ppDiffs :: Map TypeName (MergeResult NormTypeDecl NormTypeDecl) -> [String]
ppDiffs  = concatMap (uncurry ppDiff) . sortDiffs . Map.toList

-- | Perform a topological sort of the differences, so that the
-- pretty-printed form can be copied directly into the changelog.
sortDiffs :: [(TypeName, MergeResult NormTypeDecl NormTypeDecl)]
          -> [(TypeName, MergeResult NormTypeDecl NormTypeDecl)]
sortDiffs = reverse . Graph.flattenSCCs . Graph.stronglyConnComp . map f
  where
    f (tn, mr) = ((tn, mr), tn, Set.toList (mergeResultFreeVars mr))

mergeResultFreeVars :: MergeResult NormTypeDecl NormTypeDecl -> Set TypeName
mergeResultFreeVars (OnlyInLeft  x) = typeDeclFreeVars x
mergeResultFreeVars (OnlyInRight x) = typeDeclFreeVars x
mergeResultFreeVars (InBoth x y)    = typeDeclFreeVars x `Set.union` typeDeclFreeVars y

ppDiff :: TypeName -> MergeResult NormTypeDecl NormTypeDecl -> [String]
ppDiff t (OnlyInLeft _)  = ["removed " ++ pp t]
ppDiff t (OnlyInRight d) = ("added " ++ pp t ++ " ") `inFrontOf` ppLines d
ppDiff t (InBoth (NRecordType flds) (NRecordType flds')) =
    ("changed record " ++ pp t)
    : (concatMap (uncurry (ppDiffFields "field")) $ Map.toList $ diffMaps flds flds')
ppDiff t (InBoth (NUnionType alts) (NUnionType alts')) =
    ("changed union " ++ pp t)
    : (concatMap (uncurry (ppDiffFields "alternative")) $ Map.toList $ diffMaps alts alts')
ppDiff t (InBoth (NEnumType vals) (NEnumType vals')) =
    ("changed enum " ++ pp t)
    :  (map (\ x -> "  alternative removed " ++ pp x) $ Set.toList $ vals  Set.\\ vals')
    ++ (map (\ x -> "  alternative added " ++ pp x)   $ Set.toList $ vals' Set.\\ vals)
ppDiff t (InBoth _ _) = ["incompatible definitions of " ++ pp t]

ppDiffFields :: String -> FieldName -> MergeResult APIType APIType -> [String]
ppDiffFields s f (OnlyInLeft _)   = ["  " ++ s ++ " removed " ++ pp f]
ppDiffFields s f (OnlyInRight ty) = ["  " ++ s ++ " added " ++ pp f ++ " :: " ++ pp ty]
ppDiffFields s f (InBoth ty ty')   = [ "  incompatible types for " ++ s ++ " " ++ pp f
                                     , "    changelog type:      " ++ pp ty
                                     , "    latest version type: " ++ pp ty' ]

instance PPLines ApplyFailure where
  ppLines (TypeExists t)                  = ["Type " ++ pp t ++ " already exists"]
  ppLines (TypeDoesNotExist t)            = ["Type " ++ pp t ++ " does not exist"]
  ppLines (TypeWrongKind t k)             = ["Type " ++ pp t ++ " is not " ++ ppATypeKind k]
  ppLines (TypeInUse t)                   = ["Type " ++ pp t ++ " is in use, so it cannot be modified"]
  ppLines (TypeMalformed ty xs)           = ["Type " ++ pp ty
                                             ++ " is malformed, missing declarations:"
                                            , "  " ++ pp xs]
  ppLines (DeclMalformed t _ xs)          = [ "Declaration of " ++ pp t
                                              ++ " is malformed, missing declarations:"
                                            , "  " ++ pp xs]
  ppLines (FieldExists t k f)             = ["Type " ++ pp t ++ " already has the "
                                             ++ ppMemberWord k ++ " " ++ pp f]
  ppLines (FieldDoesNotExist t k f)       = ["Type " ++ pp t ++ " does not have the "
                                             ++ ppMemberWord k ++ " " ++ pp f]
  ppLines (FieldBadDefaultValue _ _ ty v) = ["Default value " ++ pp v
                                             ++ " is not compatible with the type " ++ pp ty]
  ppLines (DefaultMissing t f)            = ["Field " ++ pp f ++ " does not have a default value, but "
                                             ++ pp t ++ " occurs in the database"]
  ppLines (TableChangeError s)            = ["Error when detecting changed tables:", "  " ++ s]


instance PPLines ValueError where
  ppLines (JSONError e)              = [prettyJSONError e]
  ppLines (CustomMigrationError e v) = [ "Custom migration error:", "  " ++ e
                                       , "when migrating value"] ++ indent (ppLines v)
  ppLines (InvalidAPI af)            = "Invalid API detected during value migration:"
                                       : indent (ppLines af)


$(deriveJSON defaultOptions ''Expected)
$(deriveJSON defaultOptions ''FormatExpected)
$(deriveJSON defaultOptions ''Step)
$(deriveJSON defaultOptions ''JSONError)
$(SC.deriveSafeCopy 1 'SC.base ''Step)
