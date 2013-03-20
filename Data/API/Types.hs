{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Data.API.Types
    ( API
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
    , APIType(..)
    , BasicType(..)
    ) where

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.CaseInsensitive       as CI
import           Data.String


-- | an API spec is made up of a list of type/element specs, each
--   specifying a Haskell type and JSON wrappers

type API = [APINode]

-- | Specifies an individual element/type of the API 

data APINode
    = APINode
        { anName    :: TypeName         -- | name of Haskell type
        , anComment :: MDComment        -- | comment describing typ ein Markdown 
        , anPrefix  :: Prefix           -- | distinct short prefix (see below)
        , anSpec    :: Spec             -- | the type specification
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
    deriving (Show)

-- | SpecNewtype elements are isomorphisms of string inetgers or booleans

data SpecNewtype = SpecNewtype
    { snType   :: BasicType 
    }
    deriving (Show)

-- | SpecRecord is your classsic product type

data SpecRecord = SpecRecord
    { srFields :: Map.Map FieldName (APIType,MDComment)
    }
    deriving (Show)

-- | SpecUnion is your classsic union type

data SpecUnion = SpecUnion
    { suFields :: Map.Map FieldName (APIType,MDComment)
    }
    deriving (Show)

-- | SpecEnum is your classic enumerated type

data SpecEnum = SpecEnum
    { seAlts :: Set.Set FieldName
    }
    deriving (Show)

-- | Type is either a list, Maybe, a named element of the API or a basic type

data APIType
    = TyList  APIType       -- | list elements are types
    | TyMaybe APIType       -- | Maybe elements are types
    | TyName  TypeName      -- | the referenced type must be defined by the API
    | TyBasic BasicType     -- | a JSON string, int or bool
    deriving (Show)

-- the basic JSON types (N.B., no floating point numbers, yet)

data BasicType
    = BTstring              -- | a JSON UTF-8 string
    | BTbool                -- | a JSON bool
    | BTint                 -- | a JSON integral number
    deriving (Show)
