{-# LANGUAGE FlexibleInstances #-}

-- | A cheap and cheerful pretty-printing library
module Data.API.PP
    ( PP(..)
    , PPLines(..)
    , inFrontOf
    , indent
    ) where

import           Data.API.JSON
import           Data.API.Types

import qualified Data.Aeson                     as JS
import qualified Data.Aeson.Encode.Pretty       as JS
import qualified Data.ByteString.Lazy.Char8     as BL
import           Data.List
import           Data.Set (Set)
import qualified Data.Set                       as Set
import qualified Data.Text                      as T
import           Data.Version


class PP t where
  pp :: t -> String

class PPLines t where
  ppLines :: t -> [String]


inFrontOf :: String -> [String] -> [String]
inFrontOf x []     = [x]
inFrontOf x (s:ss) = (x ++ s) : ss

indent :: [String] -> [String]
indent = map ("  " ++)


instance PP [Char] where
  pp = id

instance PP Version where
  pp = showVersion

instance PP t => PP (Set t) where
  pp s = intercalate ", " (map pp $ Set.toList s)

instance PP T.Text where
  pp = T.unpack

instance PPLines JS.Value where
  ppLines v = lines $ BL.unpack $ JS.encodePretty v

instance PP TypeName where
  pp = _TypeName

instance PP FieldName where
  pp = _FieldName

instance PP APIType where
  pp (TyList  ty) = "[" ++ pp ty ++ "]"
  pp (TyMaybe ty) = "? " ++ pp ty
  pp (TyName  t)  = pp t
  pp (TyBasic b)  = pp b
  pp  TyJSON      = "json"

instance PP BasicType where
  pp BTstring = "string"
  pp BTbinary = "binary"
  pp BTbool   = "bool"
  pp BTint    = "integer"
  pp BTutc    = "utc"

instance PP DefaultValue where
  pp DefValList           = "[]"
  pp DefValMaybe          = "nothing"
  pp (DefValString t)     = show t
  pp (DefValBool   True)  = "true"
  pp (DefValBool   False) = "false"
  pp (DefValInt    i)     = show i
  pp (DefValUtc    u)     = show u


instance PPLines t => PPLines [t] where
  ppLines = concatMap ppLines

instance (PPLines s, PPLines t) => PPLines (s, t) where
  ppLines (s, t) = ppLines s ++ ppLines t

instance PPLines Step where
  ppLines s = [prettyStep s]
