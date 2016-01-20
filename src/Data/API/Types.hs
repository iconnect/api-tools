{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

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
    , mkRegEx
    , inIntRange
    , inUTCRange
    , base64ToBinary
    ) where

import           Data.API.Utils

import           Control.DeepSeq
import qualified Data.CaseInsensitive           as CI
import           Data.String
import           Data.Time
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH
import qualified Data.Binary.Serialise.CBOR     as CBOR
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
import           Prelude


-- | an API spec is made up of a list of type/element specs, each
--   specifying a Haskell type and JSON wrappers

type API = [Thing]

data Thing
    = ThComment MDComment
    | ThNode    APINode
    deriving (Eq,Show)

instance NFData Thing where
  rnf (ThComment x) = rnf x
  rnf (ThNode    x) = rnf x

-- | Specifies an individual element/type of the API

data APINode
    = APINode
        { anName    :: TypeName         -- ^ name of Haskell type
        , anComment :: MDComment        -- ^ comment describing type in Markdown
        , anPrefix  :: Prefix           -- ^ distinct short prefix (see below)
        , anSpec    :: Spec             -- ^ the type specification
        , anConvert :: Conversion       -- ^ optional conversion functions
        }
    deriving (Eq,Show)

instance NFData APINode where
  rnf (APINode a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

-- | TypeName must contain a valid Haskell type constructor
newtype TypeName = TypeName { _TypeName :: T.Text }
    deriving (Eq, Ord, Show, NFData, IsString)

-- | FieldName identifies recod fields and union alternatives
--   must contain a valid identifier valid in Haskell and
--   any API client wrappers (e.g., if Ruby wrappers are to be
--   generated the names should easily map into Ruby)
newtype FieldName = FieldName { _FieldName :: T.Text }
    deriving (Eq, Ord, Show, NFData, IsString)

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
    deriving (Eq,Show)

instance NFData Spec where
  rnf (SpNewtype x) = rnf x
  rnf (SpRecord  x) = rnf x
  rnf (SpUnion   x) = rnf x
  rnf (SpEnum    x) = rnf x
  rnf (SpSynonym x) = rnf x

-- | SpecNewtype elements are isomorphisms of string, inetgers or booleans

data SpecNewtype =
    SpecNewtype
        { snType   :: BasicType
        , snFilter :: Maybe Filter
        }
    deriving (Eq,Show)

instance NFData SpecNewtype where
  rnf (SpecNewtype x y) = rnf x `seq` rnf y

data Filter
    = FtrStrg RegEx
    | FtrIntg IntRange
    | FtrUTC  UTCRange
    deriving (Eq,Show)

instance NFData Filter where
  rnf (FtrStrg x) = rnf x
  rnf (FtrIntg x) = rnf x
  rnf (FtrUTC  x) = rnf x

data IntRange
    = IntRange
        { ir_lo :: Maybe Int
        , ir_hi :: Maybe Int
        }
    deriving (Eq, Show)

instance NFData IntRange where
  rnf (IntRange x y) = rnf x `seq` rnf y

inIntRange :: Int -> IntRange -> Bool
_ `inIntRange` IntRange Nothing   Nothing   = True
i `inIntRange` IntRange (Just lo) Nothing   = lo <= i
i `inIntRange` IntRange Nothing   (Just hi) = i <= hi
i `inIntRange` IntRange (Just lo) (Just hi) = lo <= i && i <= hi

data UTCRange
    = UTCRange
        { ur_lo :: Maybe UTCTime
        , ur_hi :: Maybe UTCTime
        }
    deriving (Eq, Show)

instance NFData UTCRange where
  rnf (UTCRange x y) = rnf x `seq` rnf y

inUTCRange :: UTCTime -> UTCRange -> Bool
_ `inUTCRange` UTCRange Nothing   Nothing   = True
u `inUTCRange` UTCRange (Just lo) Nothing   = lo <= u
u `inUTCRange` UTCRange Nothing   (Just hi) = u <= hi
u `inUTCRange` UTCRange (Just lo) (Just hi) = lo <= u && u <= hi


data RegEx =
    RegEx
        { re_text  :: T.Text
        , re_regex :: Regex
        }

mkRegEx :: T.Text -> RegEx
mkRegEx txt = RegEx txt $ mkRegexWithOpts (T.unpack txt) False True

instance NFData RegEx where
  rnf (RegEx x !_) = rnf x

instance ToJSON RegEx where
    toJSON RegEx{..} = String re_text

instance FromJSON RegEx where
    parseJSON = withText "RegEx" (return . mkRegEx)

instance Eq RegEx where
    r == s = re_text r == re_text s

instance Show RegEx where
    show = T.unpack . re_text

-- | SpecRecord is your classsic product type.

data SpecRecord = SpecRecord
    { srFields :: [(FieldName, FieldType)]
    }
    deriving (Eq,Show)

instance NFData SpecRecord where
  rnf (SpecRecord x) = rnf x

-- | In addition to the type and comment, record fields may carry a
-- flag indicating that they are read-only, and may have a default
-- value, which must be of a compatible type.

data FieldType = FieldType
    { ftType     :: APIType
    , ftReadOnly :: Bool
    , ftDefault  :: Maybe DefaultValue
    , ftComment  :: MDComment
    }
    deriving (Eq,Show)

instance NFData FieldType where
  rnf (FieldType a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

-- | SpecUnion is your classsic union type

data SpecUnion = SpecUnion
    { suFields :: [(FieldName,(APIType,MDComment))]
    }
    deriving (Eq,Show)

instance NFData SpecUnion where
  rnf (SpecUnion x) = rnf x

-- | SpecEnum is your classic enumerated type

data SpecEnum = SpecEnum
    { seAlts :: [(FieldName,MDComment)]
    }
    deriving (Eq,Show)

instance NFData SpecEnum where
  rnf (SpecEnum x) = rnf x

-- | Conversion possibly converts to an internal representation.  If
-- specified, a conversion is a pair of an injection function name and
-- a projection function name.
type Conversion = Maybe (FieldName,FieldName)

-- | Type is either a list, Maybe, a named element of the API or a basic type
data APIType
    = TyList  APIType       -- ^ list elements are types
    | TyMaybe APIType       -- ^ Maybe elements are types
    | TyName  TypeName      -- ^ the referenced type must be defined by the API
    | TyBasic BasicType     -- ^ a JSON string, int, bool etc.
    | TyJSON                -- ^ a generic JSON value
    deriving (Eq, Show)

-- | It is sometimes helpful to write a type name directly as a string
instance IsString APIType where
  fromString = TyName . fromString

instance NFData APIType where
  rnf (TyList  ty) = rnf ty
  rnf (TyMaybe ty) = rnf ty
  rnf (TyName  tn) = rnf tn
  rnf (TyBasic bt) = rnf bt
  rnf TyJSON       = ()

-- | the basic JSON types (N.B., no floating point numbers, yet)
data BasicType
    = BTstring -- ^ a JSON UTF-8 string
    | BTbinary -- ^ a base-64-encoded byte string
    | BTbool   -- ^ a JSON bool
    | BTint    -- ^ a JSON integral number
    | BTutc    -- ^ a JSON UTC string
    deriving (Eq, Show)

instance NFData BasicType where
  rnf !_ = ()

-- | A default value for a field
data DefaultValue
    = DefValList
    | DefValMaybe
    | DefValString T.Text  -- used for binary fields (base64 encoded)
    | DefValBool   Bool
    | DefValInt    Int
    | DefValUtc    UTCTime
    deriving (Eq, Show)

instance NFData DefaultValue where
  rnf DefValList       = ()
  rnf DefValMaybe      = ()
  rnf (DefValString t) = rnf t
  rnf (DefValBool   b) = rnf b
  rnf (DefValInt    i) = rnf i
  rnf (DefValUtc    u) = rnf u

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
    deriving (Show,Eq,Ord,NFData,CBOR.Serialise)

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
        case base64ToBinary t of
          Left  _  -> typeMismatch lab (String t)
          Right bs -> f bs

base64ToBinary :: T.Text -> Either String Binary
base64ToBinary t = Binary <$> B64.decode (T.encodeUtf8 t)


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


instance Lift Thing where
  lift (ThComment c) = [e| ThComment c |]
  lift (ThNode    n) = [e| ThNode    n |]

instance Lift APINode where
  lift (APINode a b c d e) = [e| APINode a b $(liftPrefix c) d e |]

liftPrefix :: Prefix -> ExpQ
liftPrefix ci = let s = CI.original ci in [e| CI.mk s |]

instance Lift TypeName where
  lift (TypeName s) = [e| TypeName $(litE (stringL (T.unpack s))) |]

instance Lift FieldName where
  lift (FieldName s) = [e| FieldName $(litE (stringL (T.unpack s))) |]

instance Lift Spec where
  lift (SpNewtype s) = [e| SpNewtype s |]
  lift (SpRecord  s) = [e| SpRecord  s |]
  lift (SpUnion   s) = [e| SpUnion   s |]
  lift (SpEnum    s) = [e| SpEnum    s |]
  lift (SpSynonym s) = [e| SpSynonym s |]

instance Lift SpecNewtype where
  lift (SpecNewtype a b) = [e| SpecNewtype a b |]

instance Lift Filter where
  lift (FtrStrg re) = [e| FtrStrg re |]
  lift (FtrIntg ir) = [e| FtrIntg ir |]
  lift (FtrUTC  ur) = [e| FtrUTC  ur |]

instance Lift IntRange where
    lift (IntRange lo hi) = [e| IntRange lo hi |]

instance Lift UTCRange where
    lift (UTCRange lo hi) = [e| UTCRange $(liftMaybeUTCTime lo) $(liftMaybeUTCTime hi) |]

liftUTC :: UTCTime -> ExpQ
liftUTC u = [e| fromMaybe (error "liftUTC") (parseUTC_ $(stringE (mkUTC_ u))) |]

liftMaybeUTCTime :: Maybe UTCTime -> ExpQ
liftMaybeUTCTime Nothing  = [e| Nothing |]
liftMaybeUTCTime (Just u) = [e| Just $(liftUTC u) |]

instance Lift RegEx where
    lift re = [e| mkRegEx $(stringE (T.unpack (re_text re))) |]

instance Lift SpecRecord where
  lift (SpecRecord s) = [e| SpecRecord s |]

instance Lift FieldType where
  lift (FieldType a b c d) = [e| FieldType a b c d |]

instance Lift SpecUnion where
  lift (SpecUnion s) = [e| SpecUnion s |]

instance Lift SpecEnum where
  lift (SpecEnum s) = [e| SpecEnum s |]

instance Lift APIType where
  lift (TyList  t) = [e| TyList  t |]
  lift (TyMaybe t) = [e| TyMaybe t |]
  lift (TyName  t) = [e| TyName  t |]
  lift (TyBasic t) = [e| TyBasic t |]
  lift TyJSON      = [e| TyJSON    |]

instance Lift BasicType where
  lift BTstring = [e| BTstring |]
  lift BTbinary = [e| BTbinary |]
  lift BTbool   = [e| BTbool   |]
  lift BTint    = [e| BTint    |]
  lift BTutc    = [e| BTutc    |]

instance Lift DefaultValue where
  lift DefValList       = [e| DefValList     |]
  lift DefValMaybe      = [e| DefValMaybe    |]
  lift (DefValString s) = [e| DefValString (T.pack $(lift (T.unpack s))) |]
  lift (DefValBool   b) = [e| DefValBool b   |]
  lift (DefValInt    i) = [e| DefValInt i    |]
  lift (DefValUtc    u) = [e| DefValUtc $(liftUTC u) |]
