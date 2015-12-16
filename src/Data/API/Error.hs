{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Data.API.Error
    ( -- * Representation of JSON parsing errors
      JSONError(..)
    , JSONWarning
    , Expected(..)
    , FormatExpected(..)
    , Position
    , Step(..)
    , prettyJSONErrorPositions
    , prettyJSONError
    , prettyStep

      -- * Error construction
    , expectedArray
    , expectedBool
    , expectedInt
    , expectedObject
    , expectedString
    , badFormat
    ) where

import           Data.API.Types

import qualified Data.Aeson                     as JS
import           Data.Aeson.TH
import           Data.List
import qualified Data.SafeCopy                  as SC
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


deriveJSON defaultOptions ''JSONError
deriveJSON defaultOptions ''Expected
deriveJSON defaultOptions ''FormatExpected
deriveJSON defaultOptions ''Step
$(SC.deriveSafeCopy 1 'SC.base ''Step)
