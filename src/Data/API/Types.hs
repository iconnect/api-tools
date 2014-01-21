{-# LANGUAGE DefaultSignatures          #-}
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
    , Vrn(..)
    , APIType(..)
    , DefaultValue(..)
    , BasicType(..)
    , Filter(..)
    , IntRange(..)
    , UTCRange(..)
    , RegEx(..)
    , Example(..)
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
    , mkBinary
    , withBinary
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
import           Text.Regex
import           Safe


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

          --TODO: delete these
        , anVersion :: Vrn              -- ^ version number of the data
        , anLog     :: MDComment        -- ^ conversion log
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

data UTCRange
    = UTCRange
        { ur_lo :: Maybe UTCTime
        , ur_hi :: Maybe UTCTime
        }
    deriving (Eq, Show)

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

-- Vrn records the (safe-copy) version of a data type

newtype Vrn = Vrn { _Vrn :: Int }
    deriving (Eq,Show,Num)

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


-- | the Example class is used to generate a documentation-friendly
--   example for each type in the model

class Example a where
    example :: Gen a
    default example :: Arbitrary a => Gen a
    example = arbitrary

instance Example a => Example (Maybe a) where
    example = oneof [return Nothing, Just <$> example]

instance Example a => Example [a] where
    example = listOf example

instance Example Int where
    example = arbitrarySizedBoundedIntegral `suchThat` (> 0)

instance Example Bool where
    example = choose (False, True)

instance Example T.Text where
    example = return "Mary had a little lamb"

instance Example Binary where
    example = return $ Binary $ B.pack "lots of 1s and 0s"

instance Example Value where
    example = return $ String "an example JSON value"


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

instance Example UTCTime where
    example = return $ fromJustNote dg $ parseUTC_ "2013-06-09T15:52:30Z"
      where
        dg = "Data.API.Types.Example-UTCTime"

newtype Binary = Binary { _Binary :: B.ByteString }
    deriving (Show,Eq,Ord)

instance ToJSON Binary where
    toJSON = mkBinary

instance FromJSON Binary where
    parseJSON = withBinary "Binary" return

instance QC.Arbitrary T.Text where
    arbitrary = T.pack <$> QC.arbitrary

instance QC.Arbitrary Binary where
    arbitrary = Binary <$> B.pack <$> QC.arbitrary

-- Inject and project binary Values from Text values using the base64 codec

mkBinary :: Binary -> Value
mkBinary = String . b2t . B64.encode . _Binary

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
deriveJSON defaultOptions ''Vrn
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
