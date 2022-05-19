{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | This module defines a JSON parser, like Aeson's 'FromJSON', but
-- with more detailed error-reporting capabilities.  In particular, it
-- reports errors in a structured format, and can report multiple
-- independent errors rather than stopping on the first one
-- encountered.
module Data.API.JSON
    ( -- * Parser with multiple error support
      ParserWithErrs
    , ParseFlags(useDefaults, enforceReadOnlyFields, enforceFilters)
    , defaultParseFlags
    , runParserWithErrsTop

      -- * FromJSON class with multiple error support
    , FromJSONWithErrs(..)
    , fromJSONWithErrs
    , fromJSONWithErrs'
    , fromJSONWithErrs''
    , decodeWithErrs
    , decodeWithErrs'
    , parseJSONDefault

      -- * ParserWithErrs combinators
    , withParseFlags
    , withInt
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
    , withUnion

      -- * Representation of JSON parsing errors
    , JSONError(..)
    , JSONWarning
    , Expected(..)
    , FormatExpected(..)
    , Position
    , Step(..)
    , prettyJSONErrorPositions
    , prettyJSONError
    , prettyStep

      -- * Error construction
    , failWith
    , expectedArray
    , expectedBool
    , expectedInt
    , expectedObject
    , expectedString
    , badFormat
    ) where

import           Data.API.Error
import           Data.API.JSON.Compat
import           Data.API.Time
import           Data.API.Types
import           Data.API.Utils

import           Control.Applicative
import qualified Control.Monad.Fail as Fail
import qualified Data.Aeson                     as JS
import qualified Data.Aeson.Parser              as JS
import qualified Data.Aeson.Types               as JS
import           Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Base64         as B64
import qualified Data.ByteString.Lazy           as BL
import           Data.Maybe
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Time
import           Data.Traversable
import qualified Data.Vector                    as V
import           Data.Version
import           Text.Regex
import           Prelude


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
    runParserWithErrs :: ParseFlags -> Position -> ([(JSONError, Position)], Maybe a) }
  deriving Functor

instance Applicative ParserWithErrs where
  pure x    = ParserWithErrs $ \ _ _ -> ([], Just x)
  pf <*> ps = ParserWithErrs $ \ q z ->
                  let (es_f, mb_f) = runParserWithErrs pf q z
                      (es_s, mb_s) = runParserWithErrs ps q z
                  in (es_f ++ es_s, mb_f <*> mb_s)

instance Alternative ParserWithErrs where
  empty   = failWith $ SyntaxError "No alternative"
  px <|> py = ParserWithErrs $ \ q z -> case runParserWithErrs px q z of
                                          r@(_, Just _) -> r
                                          (_, Nothing)  -> runParserWithErrs py q z

instance Monad ParserWithErrs where
  return   = pure
  px >>= f = ParserWithErrs $ \ q z ->
                  case runParserWithErrs px q z of
                    (es, Just x ) -> let (es', r) = runParserWithErrs (f x) q z
                                     in (es ++ es', r)
                    (es, Nothing) -> (es, Nothing)
#if !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail
#endif

instance Fail.MonadFail ParserWithErrs where
  fail     = failWith . SyntaxError


-- | Options to modify the behaviour of the JSON parser
data ParseFlags = ParseFlags
    { useDefaults           :: Bool
      -- ^ If true, default values from the schema will be used when a
      -- field is missing from the JSON data
    , enforceReadOnlyFields :: Bool
      -- ^ If true, fields in the schema marked read-only will be
      -- overwritten with default values
    , enforceFilters        :: Bool
      -- ^ If true, parse errors will be generated when invalid values
      -- are supplied for filtered newtypes
    }

-- | Use this as a basis for overriding individual fields of the
-- 'ParseFlags' record, in case more flags are added in the future.
defaultParseFlags :: ParseFlags
defaultParseFlags = ParseFlags { useDefaults           = False
                               , enforceReadOnlyFields = False
                               , enforceFilters        = True
                               }

-- | Run a parser with given flags, starting in the outermost
-- location, and returning warnings even if the parse was successful
runParserWithErrsTop :: ParseFlags -> ParserWithErrs a
                      -> Either [(JSONError, Position)] (a, [(JSONWarning, Position)])
runParserWithErrsTop q p = case runParserWithErrs p q [] of
                              (es, Nothing) -> Left es
                              (es, Just v)  -> Right (v, es)


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
fromJSONWithErrs' q = fmap fst . fromJSONWithErrs'' q

-- | Run the JSON parser on a value to produce a result or a list of
-- errors with their positions.  This version allows the 'ParseFlags'
-- to be specified, and produces warnings even if the parse succeeded.
fromJSONWithErrs'' :: FromJSONWithErrs a => ParseFlags -> JS.Value
                   -> Either [(JSONError, Position)] (a, [(JSONWarning, Position)])
fromJSONWithErrs'' q = runParserWithErrsTop q . parseJSONWithErrs


-- | Decode a 'ByteString' and run the JSON parser
decodeWithErrs :: FromJSONWithErrs a => BL.ByteString -> Either [(JSONError, Position)] a
decodeWithErrs = decodeWithErrs' defaultParseFlags

-- | Decode a 'ByteString' and run the JSON parser, allowing the
-- 'ParseFlags' to be specified
decodeWithErrs' :: FromJSONWithErrs a => ParseFlags -> BL.ByteString -> Either [(JSONError, Position)] a
decodeWithErrs' q x = case JS.eitherDecode x of
                     Left e  -> Left [(SyntaxError e, [])]
                     Right v -> fromJSONWithErrs' q v


-- | Suitable as an implementation of 'parseJSON' that uses the
-- 'FromJSONWithErrs' instance (provided said instance was not defined
-- using 'fromJSON'!).
parseJSONDefault :: FromJSONWithErrs a => JS.Value -> JS.Parser a
parseJSONDefault v = case fromJSONWithErrs v of
                       Right x -> return x
                       Left es -> fail $ prettyJSONErrorPositions es


---------------------------------
-- ParserWithErrs combinators
--

withParseFlags :: (ParseFlags -> ParserWithErrs a) -> ParserWithErrs a
withParseFlags k = ParserWithErrs $ \ q -> runParserWithErrs (k q) q

failWith :: JSONError -> ParserWithErrs a
failWith e = ParserWithErrs $ \ _ z -> ([(e, z)], Nothing)

warning :: JSONError -> ParserWithErrs ()
warning e = ParserWithErrs $ \ _ z -> ([(e, z)], Just ())

stepInside :: Step -> ParserWithErrs a -> ParserWithErrs a
stepInside s p = ParserWithErrs $ \ q z -> runParserWithErrs p q (s:z)

-- | If this parser returns any errors at the current position, modify
-- them using the supplied function.
modifyTopError :: (JSONError -> JSONError)
               -> ParserWithErrs a -> ParserWithErrs a
modifyTopError f p = ParserWithErrs $ \ q z -> case runParserWithErrs p q z of
                                                 (es, r) -> (map (modifyIfAt z) es, r)
  where
    modifyIfAt z x@(e, z') | z == z'   = (f e, z')
                           | otherwise = x

-- | If the conditional is false, fail with an error (if filters are
-- not being enforced) or report a warning and continue (if they are).
withFilter :: Bool -> JSONError -> ParserWithErrs a -> ParserWithErrs a
withFilter p err m | p         = m
                   | otherwise = withParseFlags $ \ pf -> if enforceFilters pf then failWith err
                                                                               else warning err >> m


-- | It's contrary to my principles, but I'll accept a string containing
-- a number instead of an actual number, and will silently truncate
-- floating point numbers to integers...
withInt :: String -> (Int -> ParserWithErrs a) -> JS.Value -> ParserWithErrs a
withInt = withNum

withNum :: Integral n => String -> (n -> ParserWithErrs a) -> JS.Value -> ParserWithErrs a
withNum _ f (JS.Number n) = f (truncate n)
withNum s f (JS.String t)
  | Right v' <- parseOnly (JS.value <* endOfInput) (T.encodeUtf8 t) = withNum s f v'
withNum s _ v = failWith $ Expected ExpInt s v

withIntRange :: IntRange -> String -> (Int -> ParserWithErrs a)
             -> JS.Value -> ParserWithErrs a
withIntRange ir dg f = withInt dg $ \ i -> withFilter (i `inIntRange` ir) (IntRangeError dg i ir) (f i)

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
withBool _ f (JS.Number x) | x == 0 = f False
                           | x == 1 = f True
withBool s _ v                      = failWith $ Expected ExpBool s v

withText :: String -> (T.Text -> ParserWithErrs a)
         -> JS.Value -> ParserWithErrs a
withText _ f (JS.String t) = f t
withText s _ v             = failWith $ Expected ExpString s v

withRegEx :: RegEx -> String -> (T.Text -> ParserWithErrs a)
               -> JS.Value -> ParserWithErrs a
withRegEx re dg f = withText dg $ \ txt -> withFilter (ok txt) (RegexError dg txt re) (f txt)
  where
    ok txt = isJust $ matchRegex (re_regex re) $ T.unpack txt

withUTC :: String -> (UTCTime -> ParserWithErrs a)
        -> JS.Value -> ParserWithErrs a
withUTC lab f = withText lab g
  where
    g t = maybe (failWith $ BadFormat FmtUTC lab t) f $ parseUTC t

withUTCRange :: UTCRange -> String -> (UTCTime -> ParserWithErrs a)
               -> JS.Value -> ParserWithErrs a
withUTCRange ur dg f = withUTC dg $ \ u -> withFilter (u `inUTCRange` ur) (UTCRangeError dg u ur) (f u)

withVersion :: String -> (Version -> ParserWithErrs a)
            -> JS.Value -> ParserWithErrs a
withVersion lab f (JS.String s) = case simpleParseVersion (T.unpack s) of
                                    Just ver -> f ver
                                    Nothing  -> failWith $ badFormat lab s
withVersion lab _ v             = failWith $ Expected ExpString lab v

-- | Look up the value of a field, treating missing fields as null
withField :: T.Text -> (JS.Value -> ParserWithErrs a)
          -> JS.Object -> ParserWithErrs a
withField k f m = stepInside (InField k) $ modifyTopError treatAsMissing $ f v
  where
    v = fromMaybe JS.Null $ lookupKey k m

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
          | useDefaults q                       = f $ fromMaybe defVal  $ lookupKey k m
          | otherwise                           = f $ fromMaybe JS.Null $ lookupKey k m

    defVal = fromMaybe JS.Null mb_defVal

-- | Look up the value of a field, failing on missing fields
withStrictField :: T.Text -> (JS.Value -> ParserWithErrs a)
          -> JS.Object -> ParserWithErrs a
withStrictField k f m = stepInside (InField k) $ case lookupKey k m of
                            Nothing -> failWith MissingField
                            Just r  -> f r

-- | Parse the value of a field, treating missing fields as null
(.:.) :: FromJSONWithErrs a => JS.Object -> T.Text -> ParserWithErrs a
m .:. k = withField k parseJSONWithErrs m

-- | Parse the value of a field, failing on missing fields
(.::) :: FromJSONWithErrs a => JS.Object -> T.Text -> ParserWithErrs a
m .:: k = withStrictField k parseJSONWithErrs m


-- | Match an inhabitant of a disjoint union, which should be an
-- object with a single field, and call the continuation corresponding
-- to the field name.
withUnion :: [(T.Text, JS.Value -> ParserWithErrs a)] -> JS.Value -> ParserWithErrs a
withUnion xs (JS.Object hs) =
   case objectToList hs of
      [(k, v)] -> case lookup k xs of
                    Just c  -> stepInside (InField k) $ c v
                    Nothing -> failWith $ MissingAlt $ map (T.unpack . fst) xs
      []       -> failWith $ MissingAlt $ map (T.unpack . fst) xs
      _:_:_    -> failWith UnexpectedField
withUnion _ val = failWith $ Expected ExpObject "Union" val
