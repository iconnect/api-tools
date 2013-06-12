{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
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
    , SpecUnion(..)
    , SpecEnum(..)
    , Conversion
    , Vrn(..)
    , APIType(..)
    , BasicType(..)
    , Example(..)
    , mkUTC
    , mkUTC'
    , withUTC
    , parseUTC'
    , parseUTC_
    , utcFormat
    , utcFormats
    , Binary(..)
    , mkBinary
    , withBinary
    ) where

import qualified Data.CaseInsensitive           as CI
import           Data.String
import           Data.Time
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text                      as T
import qualified Data.ByteString.Char8          as B
import           System.Locale
import qualified Test.QuickCheck                as QC
import           Control.Applicative
import qualified Data.ByteString.Base64         as B64
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
        { anName    :: TypeName         -- | name of Haskell type
        , anComment :: MDComment        -- | comment describing typ ein Markdown 
        , anPrefix  :: Prefix           -- | distinct short prefix (see below)
        , anSpec    :: Spec             -- | the type specification
        , anConvert :: Conversion       -- | optional conversion functions
        , anVersion :: Vrn              -- | version number of the data
        , anLog     :: MDComment        -- | conversion log
        }
    deriving (Show)

-- | TypeName must contain a valid Haskell type constructor

newtype TypeName = TypeName { _TypeName :: String }
    deriving (Show,IsString)

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

-- | SpecNewtype elements are isomorphisms of string inetgers or booleans

data SpecNewtype = SpecNewtype
    { snType   :: BasicType 
    }
    deriving (Show)

-- | SpecRecord is your classsic product type

data SpecRecord = SpecRecord
    { srFields :: [(FieldName,(APIType,MDComment))]
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
    = TyList  APIType       -- | list elements are types
    | TyMaybe APIType       -- | Maybe elements are types
    | TyName  TypeName (Maybe BasicType)
                            -- | the referenced type must be defined by the API
                            -- | the BasicType is only given if it carries an
                            -- | example, in which case the type must be a
                            -- | compatible newtype
    | TyBasic BasicType     -- | a JSON string, int, bool etc.
    | TyJSON                -- | a generic JSON value
    deriving (Show)

-- the basic JSON types (N.B., no floating point numbers, yet)

data BasicType
    = BTstring (Maybe T.Text ) -- | a JSON UTF-8 string
    | BTbinary (Maybe Binary ) -- | a base-64-encoded byte string
    | BTbool   (Maybe Bool   ) -- | a JSON bool
    | BTint    (Maybe Int    ) -- | a JSON integral number
    | BTutc    (Maybe UTCTime) -- | a JSON UTC string
    deriving (Show)



-- | the Example class is used to generate a documentation-friendly
--   example for each type in the model

class Example a where
    example :: a

instance Example a => Example (Maybe a) where
    example = Just example
    
instance Example a => Example [a] where
    example = [example]

instance Example Int where
    example = 42
    
instance Example Bool where
    example = True

instance Example T.Text where
    example = "Mary had a little lamb"

instance Example Binary where
    example = Binary $ B.pack "lots of 1s and 0s"



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
                        , utcFormat
                        ]

mkUTC' :: UTCTime -> T.Text
mkUTC' utct = T.pack $ formatTime defaultTimeLocale utcFormat utct

parseUTC' :: T.Text -> Maybe UTCTime
parseUTC' t = parseUTC_ $ T.unpack t

parseUTC_ :: String -> Maybe UTCTime
parseUTC_ s = listToMaybe $ catMaybes $
            map (\fmt->parseTime defaultTimeLocale fmt s) utcFormats  

instance Example UTCTime where
    example = fromJustNote dg $ parseUTC_ "2013-06-09T15:52:30Z"
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
