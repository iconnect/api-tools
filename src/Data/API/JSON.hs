{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | This module defines a JSON parser, like Aeson's 'FromJSON', but
-- with more detailed error-reporting capabilities.  In particular, it
-- reports errors in a structured format, and can report multiple
-- independent errors rather than stopping on the first one
-- encountered.
module Data.API.JSON
    ( -- * Representation of JSON parsing errors
      JSONError(..)
    , Expected(..)
    , FormatExpected(..)
    , Position
    , Step(..)
    , prettyJSONErrorPositions
    , prettyJSONError
    , prettyStep

      -- * Parser with multiple error support
    , ParserWithErrs
    , ParseFlags(useDefaults, enforceReadOnlyFields)
    , defaultParseFlags
    , runParserWithErrsTop

      -- * FromJSON class with multiple error support
    , FromJSONWithErrs(..)
    , fromJSONWithErrs
    , fromJSONWithErrs'
    , decodeWithErrs
    , decodeWithErrs'

      -- * ParserWithErrs combinators
    , withParseFlags
    , failWith
    , expectedArray
    , expectedBool
    , expectedInt
    , expectedObject
    , expectedString
    , badFormat
    , withInt
    , withNum
    , withIntRange
    , withBinary
    , withBool
    , withText
    , withRegEx
    , withUTC
    , withUTCRange
    , withVersion
    , withField
    , withDefaultField
    , (.:.)
    , (.::)
    ) where

import           Data.API.Types
import           Data.API.Utils

import           Control.Applicative
import qualified Data.Aeson                     as JS
import           Data.Aeson.TH
import           Data.Attoparsec.Number
import           Data.Attoparsec.Text
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Base64         as B64
import qualified Data.ByteString.Lazy           as BL
import qualified Data.HashMap.Strict            as HMap
import           Data.List
import           Data.Maybe
import qualified Data.SafeCopy                  as SC
import qualified Data.Text                      as T
import           Data.Time
import           Data.Traversable
import qualified Data.Vector                    as V
import           Data.Version
import           Distribution.Text
import           Text.Regex

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

expectedArray, expectedBool, expectedInt, expectedObject, expectedString
  :: JS.Value -> JSONError
expectedArray  = Expected ExpArray    "Array"
expectedBool   = Expected ExpBool     "Bool"
expectedInt    = Expected ExpInt      "Int"
expectedObject = Expected ExpObject   "Object"
expectedString = Expected ExpString   "String"

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

-- | Human-readable description of a single step in a position
prettyStep :: Step -> String
prettyStep (InField f) = "  in the field " ++ show f
prettyStep (InElem i)  = "  in array index " ++ show i

-- | Human-readable presentation of a list of parse errors with their
-- positions
prettyJSONErrorPositions :: [(JSONError, Position)] -> String
prettyJSONErrorPositions xs = unlines $ concatMap help xs
  where
    help (e, pos) = prettyJSONError e : map prettyStep pos


----------------------------------------
-- Parser with multiple error support
--

-- | Like 'Parser', but keeping track of locations within the JSON
-- structure and able to report multiple errors.
--
-- Careful! The 'Monad' instance does not agree with the 'Applicative'
-- instance in all circumstances, and you should use the 'Applicative'
-- instance where possible.  In particular:
--
--    * @pf \<*\> ps@ returns errors from both arguments
--
--    * @pf \`ap\` ps@  returns errors from @pf@ only
newtype ParserWithErrs a = ParserWithErrs {
    runParserWithErrs :: ParseFlags -> Position -> Either [(JSONError, Position)] a }
  deriving Functor

instance Applicative ParserWithErrs where
  pure x    = ParserWithErrs $ \ _ _ -> Right x
  pf <*> ps = ParserWithErrs $ \ q z ->
                  case (runParserWithErrs pf q z, runParserWithErrs ps q z) of
                      (Right f, Right s)  -> Right $ f s
                      (Left es, Right _)  -> Left es
                      (Right _, Left es)  -> Left es
                      (Left es, Left es') -> Left $ es ++ es'

instance Alternative ParserWithErrs where
  empty   = failWith $ SyntaxError "No alternative"
  px <|> py = ParserWithErrs $ \ q z -> case runParserWithErrs px q z of
                                        Right v -> Right v
                                        Left  _ -> runParserWithErrs py q z

instance Monad ParserWithErrs where
  return   = pure
  px >>= f = ParserWithErrs $ \ q z ->
                  case runParserWithErrs px q z of
                    Right x -> runParserWithErrs (f x) q z
                    Left es -> Left es
  fail     = failWith . SyntaxError


-- | Options to modify the behaviour of the JSON parser
data ParseFlags = ParseFlags
    { useDefaults           :: Bool
      -- ^ If true, default values from the schema will be used when a
      -- field is missing from the JSON data
    , enforceReadOnlyFields :: Bool
      -- ^ If true, fields in the schema marked read-only will be
      -- overwritten with default values
    }

-- | Use this as a basis for overriding individual fields of the
-- 'ParseFlags' record, in case more flags are added in the future.
defaultParseFlags :: ParseFlags
defaultParseFlags = ParseFlags { useDefaults           = False
                               , enforceReadOnlyFields = False
                               }

runParserWithErrsTop :: ParseFlags -> ParserWithErrs a -> Either [(JSONError, Position)] a
runParserWithErrsTop q p = runParserWithErrs p q []


--------------------------------------------------
-- FromJSON class with multiple error support
--

-- | Like 'FromJSON', but keeping track of multiple errors and their
-- positions.  Moreover, this class is more liberal in accepting
-- invalid inputs:
--
-- * a string like @\"3\"@ is accepted as an integer; and
--
-- * the integers @0@ and @1@ are accepted as booleans.

class FromJSONWithErrs a where
  -- | Parse a JSON value with structured error-reporting support.  If
  -- this method is omitted, 'fromJSON' will be used instead: note
  -- that this will result in less precise errors.
  parseJSONWithErrs :: JS.Value -> ParserWithErrs a
  default parseJSONWithErrs :: JS.FromJSON a => JS.Value -> ParserWithErrs a
  parseJSONWithErrs v = case JS.fromJSON v of
                      JS.Error e   -> failWith $ SyntaxError e
                      JS.Success a -> pure a

instance FromJSONWithErrs JS.Value where
  parseJSONWithErrs = pure

instance FromJSONWithErrs () where
  parseJSONWithErrs (JS.Array a) | V.null a  = pure ()
  parseJSONWithErrs _                        = failWith $ SyntaxError "Expected empty array"

instance FromJSONWithErrs a => FromJSONWithErrs (Maybe a) where
  parseJSONWithErrs JS.Null = pure Nothing
  parseJSONWithErrs v       = Just <$> parseJSONWithErrs v

instance FromJSONWithErrs a => FromJSONWithErrs [a] where
  parseJSONWithErrs (JS.Array a) = traverse help $ zip (V.toList a) [0..]
    where
      help (x, i) = stepInside (InElem i) $ parseJSONWithErrs x
  parseJSONWithErrs JS.Null      = pure []
  parseJSONWithErrs v            = failWith $ expectedArray v

instance FromJSONWithErrs Int where
  parseJSONWithErrs = withInt "Int" pure

instance FromJSONWithErrs Integer where
  parseJSONWithErrs = withNum "Integer" pure

instance FromJSONWithErrs Bool where
  parseJSONWithErrs = withBool "Bool" pure

instance FromJSONWithErrs Binary where
  parseJSONWithErrs = withBinary "Binary" pure

instance FromJSONWithErrs T.Text where
  parseJSONWithErrs = withText "Text" pure

instance FromJSONWithErrs UTCTime where
  parseJSONWithErrs = withUTC "UTC" pure

instance FromJSONWithErrs Version where
  parseJSONWithErrs = withVersion "Version" pure


-- | Run the JSON parser on a value to produce a result or a list of
-- errors with their positions.  This should not be used inside an
-- implementation of 'parseJSONWithErrs' as it will not pass on the
-- current position.
fromJSONWithErrs :: FromJSONWithErrs a => JS.Value -> Either [(JSONError, Position)] a
fromJSONWithErrs = fromJSONWithErrs' defaultParseFlags

-- | Run the JSON parser on a value to produce a result or a list of
-- errors with their positions.  This version allows the 'ParseFlags'
-- to be specified.
fromJSONWithErrs' :: FromJSONWithErrs a => ParseFlags -> JS.Value -> Either [(JSONError, Position)] a
fromJSONWithErrs' q = runParserWithErrsTop q . parseJSONWithErrs


-- | Decode a 'ByteString' and run the JSON parser
decodeWithErrs :: FromJSONWithErrs a => BL.ByteString -> Either [(JSONError, Position)] a
decodeWithErrs = decodeWithErrs' defaultParseFlags

-- | Decode a 'ByteString' and run the JSON parser, allowing the
-- 'ParseFlags' to be specified
decodeWithErrs' :: FromJSONWithErrs a => ParseFlags -> BL.ByteString -> Either [(JSONError, Position)] a
decodeWithErrs' q x = case JS.eitherDecode x of
                     Left e  -> Left [(SyntaxError e, [])]
                     Right v -> fromJSONWithErrs' q v


---------------------------------
-- ParserWithErrs combinators
--

withParseFlags :: (ParseFlags -> ParserWithErrs a) -> ParserWithErrs a
withParseFlags k = ParserWithErrs $ \ q -> runParserWithErrs (k q) q

failWith :: JSONError -> ParserWithErrs a
failWith e = ParserWithErrs $ \ _ z -> Left [(e, z)]

stepInside :: Step -> ParserWithErrs a -> ParserWithErrs a
stepInside s p = ParserWithErrs $ \ q z -> runParserWithErrs p q (s:z)

-- | If this parser returns any errors at the current position, modify
-- them using the supplied function.
modifyTopError :: (JSONError -> JSONError)
               -> ParserWithErrs a -> ParserWithErrs a
modifyTopError f p = ParserWithErrs $ \ q z -> case runParserWithErrs p q z of
                                               Left es -> Left $ map (modifyIfAt z) es
                                               r       -> r
  where
    modifyIfAt z x@(e, z') | z == z'   = (f e, z')
                           | otherwise = x


-- It's contrary to my principles, but I'll accept a string containing
-- a number instead of an actual number...
withInt :: String -> (Int -> ParserWithErrs a) -> JS.Value -> ParserWithErrs a
withInt = withNum

withNum :: Num n => String -> (n -> ParserWithErrs a) -> JS.Value -> ParserWithErrs a
withNum _ f (JS.Number (I n)) = f (fromInteger n)
withNum _ f (JS.String s)
  | Right (I n) <- parseOnly (number <* endOfInput) s = f (fromInteger n)
withNum s _ v = failWith $ Expected ExpInt s v

withIntRange :: IntRange -> String -> (Int -> ParserWithErrs a)
             -> JS.Value -> ParserWithErrs a
withIntRange ir dg f = withInt dg g
  where
    g i | i `inIntRange` ir = f i
        | otherwise         = failWith $ IntRangeError dg i ir

    _ `inIntRange` IntRange Nothing   Nothing   = True
    i `inIntRange` IntRange (Just lo) Nothing   = lo <= i
    i `inIntRange` IntRange Nothing   (Just hi) = i <= hi
    i `inIntRange` IntRange (Just lo) (Just hi) = lo <= i && i <= hi

withBinary :: String -> (Binary -> ParserWithErrs a) -> JS.Value -> ParserWithErrs a
withBinary lab f = withText lab g
  where
    g t =
        case B64.decode $ B.pack $ T.unpack t of
          Left  _  -> failWith $ BadFormat FmtBinary lab t
          Right bs -> f $ Binary bs

-- Everyone knows 0 and 1 are booleans really...
withBool :: String -> (Bool -> ParserWithErrs a)
         -> JS.Value -> ParserWithErrs a
withBool _ f (JS.Bool b) = f b
withBool _ f (JS.Number (I i)) | i == 0 = f False
                               | i == 1 = f True
withBool s _ v           = failWith $ Expected ExpBool s v

withText :: String -> (T.Text -> ParserWithErrs a)
         -> JS.Value -> ParserWithErrs a
withText _ f (JS.String t) = f t
withText s _ v             = failWith $ Expected ExpString s v

withRegEx :: RegEx -> String -> (T.Text -> ParserWithErrs a)
               -> JS.Value -> ParserWithErrs a
withRegEx re dg f = withText dg g
  where
    g txt = case matchRegex (re_regex re) $ T.unpack txt of
              Just _  -> f txt
              Nothing -> failWith $ RegexError dg txt re

withUTC :: String -> (UTCTime -> ParserWithErrs a)
        -> JS.Value -> ParserWithErrs a
withUTC lab f = withText lab g
  where
    g t = maybe (failWith $ BadFormat FmtUTC lab t) f $ parseUTC' t

withUTCRange :: UTCRange -> String -> (UTCTime -> ParserWithErrs a)
               -> JS.Value -> ParserWithErrs a
withUTCRange ur dg f = withUTC dg g
  where
    g u | u `inUTCRange` ur = f u
        | otherwise         = failWith $ UTCRangeError dg u ur

    _ `inUTCRange` UTCRange Nothing   Nothing   = True
    u `inUTCRange` UTCRange (Just lo) Nothing   = lo <= u
    u `inUTCRange` UTCRange Nothing   (Just hi) = u <= hi
    u `inUTCRange` UTCRange (Just lo) (Just hi) = lo <= u && u <= hi

withVersion :: String -> (Version -> ParserWithErrs a)
            -> JS.Value -> ParserWithErrs a
withVersion lab f (JS.String s) = case simpleParse $ T.unpack s of
                                    Just ver -> f ver
                                    Nothing  -> failWith $ badFormat lab s
withVersion lab _ v             = failWith $ Expected ExpString lab v

-- | Look up the value of a field, treating missing fields as null
withField :: T.Text -> (JS.Value -> ParserWithErrs a)
          -> JS.Object -> ParserWithErrs a
withField k f m = stepInside (InField k) $ modifyTopError treatAsMissing $ f v
  where
    v = fromMaybe JS.Null $ HMap.lookup k m

treatAsMissing :: JSONError -> JSONError
treatAsMissing (Expected _ _ JS.Null) = MissingField
treatAsMissing e                      = e

-- | Look up the value of a field, which may be read-only or use a
-- default value (depending on the 'ParseFlags').
withDefaultField :: Bool -> Maybe JS.Value -> T.Text -> (JS.Value -> ParserWithErrs a)
                 -> JS.Object -> ParserWithErrs a
withDefaultField readOnly mb_defVal k f m =
    stepInside (InField k) $ modifyTopError treatAsMissing $ withParseFlags foo
  where
    foo q | readOnly && enforceReadOnlyFields q = f defVal
          | useDefaults q                       = f $ fromMaybe defVal  $ HMap.lookup k m
          | otherwise                           = f $ fromMaybe JS.Null $ HMap.lookup k m

    defVal = fromMaybe JS.Null mb_defVal

-- | Look up the value of a field, failing on missing fields
withStrictField :: T.Text -> (JS.Value -> ParserWithErrs a)
          -> JS.Object -> ParserWithErrs a
withStrictField k f m = stepInside (InField k) $ case HMap.lookup k m of
                            Nothing -> failWith MissingField
                            Just r  -> f r

-- | Parse the value of a field, treating missing fields as null
(.:.) :: FromJSONWithErrs a => JS.Object -> T.Text -> ParserWithErrs a
m .:. k = withField k parseJSONWithErrs m

-- | Parse the value of a field, failing on missing fields
(.::) :: FromJSONWithErrs a => JS.Object -> T.Text -> ParserWithErrs a
m .:: k = withStrictField k parseJSONWithErrs m


deriveJSON defaultOptions ''JSONError
deriveJSON defaultOptions ''Expected
deriveJSON defaultOptions ''FormatExpected
deriveJSON defaultOptions ''Step
$(SC.deriveSafeCopy 1 'SC.base ''Step)
