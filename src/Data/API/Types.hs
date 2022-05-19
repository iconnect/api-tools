{-# LANGUAGE CPP                        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveLift                 #-}
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

import           Data.API.Time

import           Control.DeepSeq
import qualified Data.CaseInsensitive           as CI
import           Data.String
import           Data.Time
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH
import qualified Codec.Serialise     as CBOR
import           Data.Maybe
import           Data.SafeCopy
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
    deriving (Eq,Lift,Show)

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
    deriving (Eq,Lift,Show)

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
    deriving (Eq,Lift,Show)

instance NFData SpecNewtype where
  rnf (SpecNewtype x y) = rnf x `seq` rnf y

data Filter
    = FtrStrg RegEx
    | FtrIntg IntRange
    | FtrUTC  UTCRange
    deriving (Eq,Lift,Show)

instance NFData Filter where
  rnf (FtrStrg x) = rnf x
  rnf (FtrIntg x) = rnf x
  rnf (FtrUTC  x) = rnf x

data IntRange
    = IntRange
        { ir_lo :: Maybe Int
        , ir_hi :: Maybe Int
        }
    deriving (Eq, Lift, Show)

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
    deriving (Eq,Lift,Show)

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
    deriving (Eq,Lift,Show)

instance NFData FieldType where
  rnf (FieldType a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

-- | SpecUnion is your classsic union type

data SpecUnion = SpecUnion
    { suFields :: [(FieldName,(APIType,MDComment))]
    }
    deriving (Eq,Lift,Show)

instance NFData SpecUnion where
  rnf (SpecUnion x) = rnf x

-- | SpecEnum is your classic enumerated type

data SpecEnum = SpecEnum
    { seAlts :: [(FieldName,MDComment)]
    }
    deriving (Eq,Lift,Show)

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
    deriving (Eq, Lift, Show)

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
    deriving (Eq, Lift, Show)

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
defaultValueAsJsValue (DefValUtc    t)           = String (printUTC t)


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


instance Lift APINode where
  lift (APINode a b c d e) = [e| APINode a b $(liftPrefix c) d e |]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (APINode a b c d e) = [e|| APINode a b $$(liftTypedPrefix c) d e ||]
#endif


#if MIN_VERSION_template_haskell(2,17,0)
liftPrefix :: Quote m => Prefix -> m Exp
liftText :: Quote m => T.Text -> m Exp
liftUTC :: Quote m => UTCTime -> m Exp
liftMaybeUTCTime :: Quote m => Maybe UTCTime -> m Exp
#else
liftPrefix :: Prefix -> ExpQ
liftText :: T.Text -> ExpQ
liftUTC :: UTCTime -> ExpQ
liftMaybeUTCTime :: Maybe UTCTime -> ExpQ
#endif

liftPrefix ci = let s = CI.original ci in [e| CI.mk s |]

liftText s = [e| T.pack $(litE (stringL (T.unpack s))) |]

liftUTC u = [e| unsafeParseUTC $(liftText (printUTC u)) |]

liftMaybeUTCTime Nothing  = [e| Nothing |]
liftMaybeUTCTime (Just u) = [e| Just $(liftUTC u) |]



#if MIN_VERSION_template_haskell(2,17,0)
liftTypedPrefix :: Quote m => Prefix -> Code m Prefix
liftTypedPrefix ci = let s = CI.original ci in [e|| CI.mk s ||]

liftTypedText :: Quote m => T.Text -> Code m T.Text
liftTypedText s = [e|| T.pack $$(liftTyped (T.unpack s)) ||]

liftTypedUTC :: Quote m => UTCTime -> Code m UTCTime
liftTypedUTC u = [e|| unsafeParseUTC $$(liftTypedText (printUTC u)) ||]

liftTypedMaybeUTCTime :: Quote m => Maybe UTCTime -> Code m (Maybe UTCTime)
liftTypedMaybeUTCTime Nothing  = [e|| Nothing ||]
liftTypedMaybeUTCTime (Just u) = [e|| Just $$(liftTypedUTC u) ||]
#elif MIN_VERSION_template_haskell(2,16,0)
liftTypedPrefix :: Prefix -> TExpQ Prefix
liftTypedPrefix ci = let s = CI.original ci in [e|| CI.mk s ||]

liftTypedText :: T.Text -> TExpQ T.Text
liftTypedText s = [e|| T.pack $$(liftTyped (T.unpack s)) ||]

liftTypedUTC :: UTCTime -> TExpQ UTCTime
liftTypedUTC u = [e|| unsafeParseUTC $$(liftTypedText (printUTC u)) ||]

liftTypedMaybeUTCTime :: Maybe UTCTime -> TExpQ (Maybe UTCTime)
liftTypedMaybeUTCTime Nothing  = [e|| Nothing ||]
liftTypedMaybeUTCTime (Just u) = [e|| Just $$(liftTypedUTC u) ||]
#endif

instance Lift TypeName where
  lift (TypeName s) = [e| TypeName $(liftText s) |]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (TypeName s) = [e|| TypeName $$(liftTypedText s) ||]
#endif

instance Lift FieldName where
  lift (FieldName s) = [e| FieldName $(liftText s) |]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (FieldName s) = [e|| FieldName $$(liftTypedText s) ||]
#endif

instance Lift UTCRange where
  lift (UTCRange lo hi) = [e| UTCRange $(liftMaybeUTCTime lo) $(liftMaybeUTCTime hi) |]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (UTCRange lo hi) = [e|| UTCRange $$(liftTypedMaybeUTCTime lo) $$(liftTypedMaybeUTCTime hi) ||]
#endif

instance Lift RegEx where
  lift re = [e| mkRegEx $(liftText (re_text re)) |]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped re = [e|| mkRegEx $$(liftTypedText (re_text re)) ||]
#endif

instance Lift DefaultValue where
  lift DefValList       = [e| DefValList |]
  lift DefValMaybe      = [e| DefValMaybe |]
  lift (DefValString s) = [e| DefValString $(liftText s) |]
  lift (DefValBool   b) = [e| DefValBool b |]
  lift (DefValInt    i) = [e| DefValInt i |]
  lift (DefValUtc    u) = [e| DefValUtc $(liftUTC u) |]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped DefValList       = [e|| DefValList ||]
  liftTyped DefValMaybe      = [e|| DefValMaybe ||]
  liftTyped (DefValString s) = [e|| DefValString $$(liftTypedText s) ||]
  liftTyped (DefValBool   b) = [e|| DefValBool b ||]
  liftTyped (DefValInt    i) = [e|| DefValInt i ||]
  liftTyped (DefValUtc    u) = [e|| DefValUtc $$(liftTypedUTC u) ||]
#endif

$(deriveSafeCopy 0 'base ''Binary)

$(let deriveJSONs = fmap concat . mapM (deriveJSON defaultOptions)
  in deriveJSONs [ ''CI.CI
                 , ''TypeName
                 , ''FieldName
                 , ''DefaultValue
                 , ''SpecEnum
                 , ''SpecUnion
                 , ''SpecRecord
                 , ''FieldType
                 , ''SpecNewtype
                 , ''Filter
                 , ''IntRange
                 , ''UTCRange
                 , ''BasicType
                 , ''APIType
                 , ''Spec
                 , ''APINode
                 , ''Thing
                 ])
