{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}


module Data.API.Types
    ( API
    , Thing(..)
    , APINode(..)
    , TypeName(..)
    , FieldName(..)
    , MDComment
    , Prefix
    , Spec(..)
    , SpecNewtype(..)
    , SpecRecord(..)
    , FieldType(..)
    , SpecUnion(..)
    , SpecEnum(..)
    , Conversion
    , APIType(..)
    , DefaultValue(..)
    , BasicType(..)
    , Filter(..)
    , IntRange(..)
    , UTCRange(..)
    , RegEx(..)
    , Binary(..)
    , defaultValueAsJsValue
    ) where

import           Data.API.Utils

import qualified Data.CaseInsensitive           as CI
import           Data.String
import           Data.Time
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH
import           Data.Maybe
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.ByteString.Char8          as B
import           Test.QuickCheck                as QC
import           Control.Applicative
import qualified Data.ByteString.Base64         as B64
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Text.Regex


-- | an API spec is made up of a list of type/element specs, each
--   specifying a Haskell type and JSON wrappers

type API = [Thing]

data Thing
    = ThComment MDComment
    | ThNode    APINode
    deriving (Show)

-- | Specifies an individual element/type of the API

data APINode
    = APINode
        { anName    :: TypeName         -- ^ name of Haskell type
        , anComment :: MDComment        -- ^ comment describing type in Markdown
        , anPrefix  :: Prefix           -- ^ distinct short prefix (see below)
        , anSpec    :: Spec             -- ^ the type specification
        , anConvert :: Conversion       -- ^ optional conversion functions
        }
    deriving (Show)

-- | TypeName must contain a valid Haskell type constructor

newtype TypeName = TypeName { _TypeName :: String }
    deriving (Eq, Ord,Show, IsString)

-- | FieldName identifies recod fields and union alternatives
--   must contain a valid identifier valid in Haskell and
--   any API client wrappers (e.g., if Ruby wrappers are to be
--   generated the names should easily map into Ruby)

newtype FieldName = FieldName { _FieldName :: String }
    deriving (Show,Eq,Ord,IsString)

-- | Markdown comments are represented by strings

type MDComment = String

-- | a distinct case-insensitive short prefix used to form unique record field
--   names and data constructors:
--
--      * must be a valid Haskell identifier
--
--      * must be unique within the API

type Prefix = CI.CI String

-- | type/element specs are either simple type isomorphisms of basic JSON
--   types, records, unions or enumerated types

data Spec
    = SpNewtype SpecNewtype
    | SpRecord  SpecRecord
    | SpUnion   SpecUnion
    | SpEnum    SpecEnum
    | SpSynonym APIType
    deriving (Show)

-- | SpecNewtype elements are isomorphisms of string, inetgers or booleans

data SpecNewtype =
    SpecNewtype
        { snType   :: BasicType
        , snFilter :: Maybe Filter
        }
    deriving (Show)

data Filter
    = FtrStrg RegEx
    | FtrIntg IntRange
    | FtrUTC  UTCRange
    deriving (Show)

data IntRange
    = IntRange
        { ir_lo :: Maybe Int
        , ir_hi :: Maybe Int
        }
    deriving (Eq, Show)

instance Lift IntRange where
    lift (IntRange lo hi) = [e| IntRange lo hi |]

data UTCRange
    = UTCRange
        { ur_lo :: Maybe UTCTime
        , ur_hi :: Maybe UTCTime
        }
    deriving (Eq, Show)

instance Lift UTCRange where
    lift (UTCRange lo hi) = [e| UTCRange $(liftMaybeUTCTime lo) $(liftMaybeUTCTime hi) |]

liftUTC :: UTCTime -> ExpQ
liftUTC u = [e| fromMaybe (error "liftUTC") (parseUTC_ $(stringE (mkUTC_ u))) |]

liftMaybeUTCTime :: Maybe UTCTime -> ExpQ
liftMaybeUTCTime Nothing  = [e| Nothing |]
liftMaybeUTCTime (Just u) = [e| Just $(liftUTC u) |]


data RegEx =
    RegEx
        { re_text  :: T.Text
        , re_regex :: Regex
        }

mkRegEx :: T.Text -> RegEx
mkRegEx txt = RegEx txt $ mkRegexWithOpts (T.unpack txt) False True

instance ToJSON RegEx where
    toJSON RegEx{..} = String re_text

instance FromJSON RegEx where
    parseJSON = withText "RegEx" (return . mkRegEx)

instance Eq RegEx where
    r == s = re_text r == re_text s

instance Show RegEx where
    show = T.unpack . re_text

instance Lift RegEx where
    lift re = [e| mkRegEx $(stringE (T.unpack (re_text re))) |]

-- | SpecRecord is your classsic product type.

data SpecRecord = SpecRecord
    { srFields :: [(FieldName, FieldType)]
    }
    deriving (Show)

-- | In addition to the type and comment, record fields may carry a
-- flag indicating that they are read-only, and may have a default
-- value, which must be of a compatible type.

data FieldType = FieldType
    { ftType     :: APIType
    , ftReadOnly :: Bool
    , ftDefault  :: Maybe DefaultValue
    , ftComment  :: MDComment
    }
    deriving (Show)

-- | SpecUnion is your classsic union type

data SpecUnion = SpecUnion
    { suFields :: [(FieldName,(APIType,MDComment))]
    }
    deriving (Show)

-- | SpecEnum is your classic enumerated type

data SpecEnum = SpecEnum
    { seAlts :: [(FieldName,MDComment)]
    }
    deriving (Show)

-- Conversion possibly converts to an internal representation

type Conversion = Maybe (FieldName,FieldName)

-- | Type is either a list, Maybe, a named element of the API or a basic type
data APIType
    = TyList  APIType       -- ^ list elements are types
    | TyMaybe APIType       -- ^ Maybe elements are types
    | TyName  TypeName      -- ^ the referenced type must be defined by the API
    | TyBasic BasicType     -- ^ a JSON string, int, bool etc.
    | TyJSON                -- ^ a generic JSON value
    deriving (Eq, Show)

-- | the basic JSON types (N.B., no floating point numbers, yet)
data BasicType
    = BTstring -- ^ a JSON UTF-8 string
    | BTbinary -- ^ a base-64-encoded byte string
    | BTbool   -- ^ a JSON bool
    | BTint    -- ^ a JSON integral number
    | BTutc    -- ^ a JSON UTC string
    deriving (Eq, Show)

-- | A default value for a field
data DefaultValue
    = DefValList
    | DefValMaybe
    | DefValString T.Text  -- used for binary fields (base64 encoded)
    | DefValBool   Bool
    | DefValInt    Int
    | DefValUtc    UTCTime
    deriving (Eq, Show)

instance Lift DefaultValue where
  lift DefValList       = [e| DefValList     |]
  lift DefValMaybe      = [e| DefValMaybe    |]
  lift (DefValString s) = [e| DefValString (T.pack $(lift (T.unpack s))) |]
  lift (DefValBool   b) = [e| DefValBool b   |]
  lift (DefValInt    i) = [e| DefValInt i    |]
  lift (DefValUtc    u) = [e| DefValUtc $(liftUTC u) |]

-- | Convert a default value to an Aeson 'Value'.  This differs from
-- 'toJSON' as it will not round-trip with 'fromJSON': UTC default
-- values are turned into strings.
defaultValueAsJsValue :: DefaultValue -> Value
defaultValueAsJsValue  DefValList                = toJSON ([] :: [()])
defaultValueAsJsValue  DefValMaybe               = Null
defaultValueAsJsValue (DefValString s)           = String s
defaultValueAsJsValue (DefValBool   b)           = Bool b
defaultValueAsJsValue (DefValInt    n)           = Number (fromIntegral n)
defaultValueAsJsValue (DefValUtc    t)           = mkUTC t


-- | Binary data is represented in JSON format as a base64-encoded
-- string
newtype Binary = Binary { _Binary :: B.ByteString }
    deriving (Show,Eq,Ord)

instance ToJSON Binary where
    toJSON = String . T.decodeLatin1 . B64.encode . _Binary

instance FromJSON Binary where
    parseJSON = withBinary "Binary" return

instance QC.Arbitrary T.Text where
    arbitrary = T.pack <$> QC.arbitrary

instance QC.Arbitrary Binary where
    arbitrary = Binary <$> B.pack <$> QC.arbitrary

withBinary :: String -> (Binary->Parser a) -> Value -> Parser a
withBinary lab f = withText lab g
  where
    g t =
        case B64.decode $ T.encodeUtf8 t of
          Left  _  -> typeMismatch lab (String t)
          Right bs -> f $ Binary bs


deriveJSON defaultOptions ''Thing
deriveJSON defaultOptions ''APINode
deriveJSON defaultOptions ''TypeName
deriveJSON defaultOptions ''FieldName
deriveJSON defaultOptions ''Spec
deriveJSON defaultOptions ''APIType
deriveJSON defaultOptions ''DefaultValue
deriveJSON defaultOptions ''SpecEnum
deriveJSON defaultOptions ''SpecUnion
deriveJSON defaultOptions ''SpecRecord
deriveJSON defaultOptions ''FieldType
deriveJSON defaultOptions ''SpecNewtype
deriveJSON defaultOptions ''Filter
deriveJSON defaultOptions ''IntRange
deriveJSON defaultOptions ''UTCRange
deriveJSON defaultOptions ''BasicType
deriveJSON defaultOptions ''CI.CI
