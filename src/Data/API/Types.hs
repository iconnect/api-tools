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
    , mkRegEx
    , mkRegEx'
    , mkUTC
    , mkUTC'
    , mkUTC_
    , withUTC
    , parseUTC'
    , parseUTC_
    , utcFormat
    , utcFormats
    , Binary(..)
    , defaultValueAsJsValue
    ) where

import qualified Data.CaseInsensitive           as CI
import           Data.String
import           Data.Time
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH
import qualified Data.Text                      as T
import qualified Data.ByteString.Char8          as B
import           System.Locale
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

liftMaybeUTCTime :: Maybe UTCTime -> ExpQ
liftMaybeUTCTime Nothing  = [e| Nothing |]
liftMaybeUTCTime (Just u) = [e| parseUTC_ $(stringE (mkUTC_ u)) |]


data RegEx =
    RegEx
        { re_text  :: T.Text
        , re_regex :: Regex
        }

mkRegEx :: T.Text -> RegEx
mkRegEx txt = RegEx txt $ mkRegexWithOpts (T.unpack txt) False True

mkRegEx' :: String -> RegEx
mkRegEx' s = RegEx (T.pack s) $ mkRegexWithOpts s False True

instance ToJSON RegEx where
    toJSON RegEx{..} = String re_text

instance FromJSON RegEx where
    parseJSON = withText "RegEx" (return . mkRegEx)

instance Show RegEx where
    show = T.unpack . re_text

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

-- | Convert a default value to an Aeson Value
defaultValueAsJsValue :: DefaultValue -> Value
defaultValueAsJsValue  DefValList                = toJSON ([] :: [()])
defaultValueAsJsValue  DefValMaybe               = Null
defaultValueAsJsValue (DefValString s)           = String s
defaultValueAsJsValue (DefValBool   b)           = Bool b
defaultValueAsJsValue (DefValInt    n)           = Number (fromIntegral n)
defaultValueAsJsValue (DefValUtc    t)           = mkUTC t


-- Inject and project UTC Values from Text values

mkUTC :: UTCTime -> Value
mkUTC = String . mkUTC'

withUTC :: String -> (UTCTime->Parser a) -> Value -> Parser a
withUTC lab f = withText lab g
  where
    g t = maybe (typeMismatch lab (String t)) f $ parseUTC' t

utcFormat :: String
utcFormat =               "%Y-%m-%dT%H:%M:%SZ"

utcFormats :: [String]
utcFormats =
                        [ "%Y-%m-%dT%H:%M:%S%z"
                        , "%Y-%m-%dT%H:%M:%S%Z"
                        , "%Y-%m-%dT%H:%M%Z"
                        , "%Y-%m-%dT%H:%M:%S%QZ"
                        , utcFormat
                        ]

mkUTC' :: UTCTime -> T.Text
mkUTC' = T.pack . mkUTC_

mkUTC_ :: UTCTime -> String
mkUTC_ utct = formatTime defaultTimeLocale utcFormat utct

parseUTC' :: T.Text -> Maybe UTCTime
parseUTC' t = parseUTC_ $ T.unpack t

parseUTC_ :: String -> Maybe UTCTime
parseUTC_ s = listToMaybe $ catMaybes $
            map (\fmt->parseTime defaultTimeLocale fmt s) utcFormats


-- | Binary data is represented in JSON format as a base64-encoded
-- string
newtype Binary = Binary { _Binary :: B.ByteString }
    deriving (Show,Eq,Ord)

instance ToJSON Binary where
    toJSON = String . b2t . B64.encode . _Binary

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
        case B64.decode $ t2b t of
          Left  _  -> typeMismatch lab (String t)
          Right bs -> f $ Binary bs

b2t :: B.ByteString -> T.Text
b2t = T.pack . B.unpack

t2b :: T.Text -> B.ByteString
t2b = B.pack . T.unpack


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
