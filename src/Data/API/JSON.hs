{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}


module Data.API.JSON
    ( JSONError(..)
    , Expected(..)
    , FormatExpected(..)
    , prettyJSONError
    , prettyStep
    , prettyJSONErrorPositions
    , Position
    , Step(..)
    , ParserWithErrs
    , FromJSONWithErrs(..)
    , promoteFromJSON
    , fromJSONWithErrs
    , decodeWithErrs
    , failWith
    , expectedArray
    , expectedBool
    , expectedInt
    , expectedObject
    , expectedString
    , badFormat
    , withInt
    , with_int_fr, with_int_to, with_int_fr_to
    , withBinary
    , withBool
    , withText
    , with_txt_re
    , withUTC
    , with_utc_fr, with_utc_to, with_utc_fr_to
    , withVersion
    , withField
    , (.:.)
    , (.::)
    , prop_decodesTo, prop_resultsMatchRoundtrip
    ) where

import           Data.API.Types hiding (withBinary, withUTC)

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

data JSONError = Expected  Expected       String JS.Value
               | BadFormat FormatExpected String T.Text
               | MissingField
               | MissingAlt [String]
               | UnexpectedField
               | UnexpectedEnumVal [T.Text] T.Text
               | IntRangeError String Int IntRange
               | UTCRangeError String UTCTime UTCRange
               | RegexError String T.Text String
               | SyntaxError String
  deriving (Eq, Show)

data Expected = ExpArray
              | ExpBool
              | ExpInt
              | ExpObject
              | ExpString
  deriving (Eq, Show)

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
prettyJSONError (RegexError s _ t)    = s ++ ": failed to match RE: " ++ t
prettyJSONError (SyntaxError e)       = "JSON syntax error: " ++ e

-- | A position inside a JSON value is a list of steps, ordered
-- innermost first (so going inside an object prepends a step).
type Position = [Step]

-- | Each step may be into a field of an object, or a specific element
-- of an array.
data Step = InField T.Text | InElem Int
  deriving (Eq, Show)

prettyStep :: Step -> String
prettyStep (InField f) = "  in the field " ++ show f
prettyStep (InElem i)  = "  in array index " ++ show i

prettyJSONErrorPositions :: [(JSONError, Position)] -> String
prettyJSONErrorPositions xs = unlines $ concatMap help xs
  where
    help (e, pos) = prettyJSONError e : map prettyStep pos

----------------------------------------
-- Parser with multiple error support
--

newtype ParserWithErrs a = ParserWithErrs {
    runParserWithErrs :: Position -> Either [(JSONError, Position)] a }
  deriving Functor

instance Applicative ParserWithErrs where
  pure x    = ParserWithErrs $ const $ Right x
  pf <*> ps = ParserWithErrs $ \ z ->
                  case (runParserWithErrs pf z, runParserWithErrs ps z) of
                      (Right f, Right s)  -> Right $ f s
                      (Left es, Right _)  -> Left es
                      (Right _, Left es)  -> Left es
                      (Left es, Left es') -> Left $ es ++ es'

instance Alternative ParserWithErrs where
  empty   = failWith $ SyntaxError "No alternative"
  px <|> py = ParserWithErrs $ \ z -> case runParserWithErrs px z of
                                        Right v -> Right v
                                        Left  _ -> runParserWithErrs py z

-- | Careful! This Monad instance does not agree with the Applicative
-- instance in all circumstances, and you should use the Applicative
-- instance where possible. In particular:
--     pf <*> ps  returns errors from both arguments
--     pf `ap` ps returns errors from pf only
instance Monad ParserWithErrs where
  return   = pure
  px >>= f = ParserWithErrs $ \ z ->
                  case runParserWithErrs px z of
                    Right x -> runParserWithErrs (f x) z
                    Left es -> Left es
  fail     = failWith . SyntaxError

runParserWithErrsTop :: ParserWithErrs a -> Either [(JSONError, Position)] a
runParserWithErrsTop p = runParserWithErrs p []


--------------------------------------------------
-- FromJSON class with multiple error support
--

class FromJSONWithErrs a where
  parseJSONWithErrs :: JS.Value -> ParserWithErrs a

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
  parseJSONWithErrs v            = failWith $ expectedArray v

instance FromJSONWithErrs Int where
  parseJSONWithErrs = withInt "Int" pure

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

promoteFromJSON :: JS.FromJSON a => (JS.Value -> JSONError)
                -> JS.Value -> ParserWithErrs a
promoteFromJSON f v = case JS.fromJSON v of
                      JS.Error _   -> failWith $ f v
                      JS.Success a -> pure a

fromJSONWithErrs :: FromJSONWithErrs a => JS.Value -> Either [(JSONError, Position)] a
fromJSONWithErrs = runParserWithErrsTop . parseJSONWithErrs

decodeWithErrs :: FromJSONWithErrs a => BL.ByteString -> Either [(JSONError, Position)] a
decodeWithErrs x = case JS.eitherDecode x of
                     Left e  -> Left [(SyntaxError e, [])]
                     Right v -> fromJSONWithErrs v


---------------------------------
-- ParserWithErrs combinators
--

failWith :: JSONError -> ParserWithErrs a
failWith e = ParserWithErrs $ \ z -> Left [(e, z)]

stepInside :: Step -> ParserWithErrs a -> ParserWithErrs a
stepInside s p = ParserWithErrs $ \ z -> runParserWithErrs p (s:z)

-- | If this parser returns any errors at the current position, modify
-- them using the supplied function.
modifyTopError :: (JSONError -> JSONError)
               -> ParserWithErrs a -> ParserWithErrs a
modifyTopError f p = ParserWithErrs $ \ z -> case runParserWithErrs p z of
                                               Left es -> Left $ map (modifyIfAt z) es
                                               r       -> r
  where
    modifyIfAt z x@(e, z') | z == z'   = (f e, z')
                           | otherwise = x


with_int_fr    :: Int -> String -> (Int -> ParserWithErrs a)
               -> JS.Value -> ParserWithErrs a
with_int_to    :: Int -> String -> (Int -> ParserWithErrs a)
               -> JS.Value -> ParserWithErrs a
with_int_fr_to :: Int -> Int -> String -> (Int -> ParserWithErrs a)
               -> JS.Value -> ParserWithErrs a

with_int_fr lo dg f = withInt dg g
  where
    g i = case i>=lo of
            True  -> f i
            False -> failWith $ IntRangeError dg i $ IntRange (Just lo) Nothing

with_int_to hi dg f = withInt dg g
  where
    g i = case i<=hi of
            True  -> f i
            False -> failWith $ IntRangeError dg i $ IntRange Nothing (Just hi)

with_int_fr_to lo hi dg f = withInt dg g
  where
    g i = case lo<=i && i<=hi of
            True  -> f i
            False -> failWith $ IntRangeError dg i $ IntRange (Just lo) (Just hi)

-- It's contrary to my principles, but I'll accept a string containing
-- a number instead of an actual number...
withInt :: String -> (Int -> ParserWithErrs a) -> JS.Value -> ParserWithErrs a
withInt _ f (JS.Number (I n)) = f (fromInteger n)
withInt _ f (JS.String s)
  | Right (I n) <- parseOnly (number <* endOfInput) s = f (fromInteger n)
withInt s _ v = failWith $ Expected ExpInt s v

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

with_txt_re    :: String -> String -> (T.Text -> ParserWithErrs a)
               -> JS.Value -> ParserWithErrs a
with_txt_re re_s dg f = withText dg g
  where
    g txt = case matchRegex re $ T.unpack txt of
              Just _  -> f txt
              Nothing -> failWith $ RegexError dg txt re_s

    re = mkRegexWithOpts re_s False True

withUTC :: String -> (UTCTime -> ParserWithErrs a)
        -> JS.Value -> ParserWithErrs a
withUTC lab f = withText lab g
  where
    g t = maybe (failWith $ BadFormat FmtUTC lab t) f $ parseUTC' t

with_utc_fr    :: String -> String -> (UTCTime -> ParserWithErrs a)
               -> JS.Value -> ParserWithErrs a
with_utc_to    :: String -> String -> (UTCTime -> ParserWithErrs a)
               -> JS.Value -> ParserWithErrs a
with_utc_fr_to :: String -> String -> String -> (UTCTime -> ParserWithErrs a)
               -> JS.Value -> ParserWithErrs a

with_utc_fr lo_s dg f = withUTC dg g
  where
    g u = case u>=lo of
            True  -> f u
            False -> failWith $ UTCRangeError dg u $ UTCRange (Just lo) Nothing

    lo   = maybe err id $ parseUTC_ lo_s

    err  = error "with_utc_fr: bad UTC format"

with_utc_to hi_s dg f = withUTC dg g
  where
    g u = case u<=hi of
            True  -> f u
            False -> failWith $ UTCRangeError dg u $ UTCRange Nothing (Just hi)

    hi   = maybe err id $ parseUTC_ hi_s

    err  = error "with_utc_fr: bad UTC format"

with_utc_fr_to lo_s hi_s dg f = withUTC dg g
  where
    g u = case lo<=u && u<=hi of
            True  -> f u
            False -> failWith $ UTCRangeError dg u $ UTCRange (Just lo) (Just hi)

    lo   = maybe err id $ parseUTC_ lo_s
    hi   = maybe err id $ parseUTC_ hi_s

    err  = error "with_utc_fr: bad UTC format"

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
    treatAsMissing (Expected _ _ JS.Null) = MissingField
    treatAsMissing e                      = e

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


prop_decodesTo :: forall a . (Eq a, FromJSONWithErrs a)
               => JS.Value -> a -> Bool
prop_decodesTo v x = case fromJSONWithErrs v :: Either [(JSONError, Position)] a of
                       Right y | x == y -> True
                       _                -> False

prop_resultsMatchRoundtrip :: forall a . (Eq a, JS.ToJSON a, FromJSONWithErrs a )
                           => a -> Bool
prop_resultsMatchRoundtrip x = prop_decodesTo (JS.toJSON x) x

deriveJSON defaultOptions ''JSONError
deriveJSON defaultOptions ''Expected
deriveJSON defaultOptions ''FormatExpected
deriveJSON defaultOptions ''Step
$(SC.deriveSafeCopy 1 'SC.base ''Step)
