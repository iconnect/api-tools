{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Data.API.Doc.Types
    ( URL
    , HTTPMethod
    , StatusCode
    , Call(..)
    , Header(..)
    , Param(..)
    , View(..)
    , Sample(..)
    , Body(..)
    , DocInfo(..)
    , renderAPIType
    , renderBodyType
    , mk_link
    ) where

import           Data.API.PP
import           Data.API.Types

import qualified Data.Text                      as T
import           Text.Printf

type URL        = String
type HTTPMethod = String
type StatusCode = Int

-- | Documents a single method call on a resource in a web application
data Call
    = Call
        { call_http_method   :: HTTPMethod               -- ^ HTTP method being documented
        , call_path          :: [String]                 -- ^ Relative URL path of the resource
        , call_description   :: String                   -- ^ Free-form text description
        , call_auth_required :: Bool                     -- ^ Does the call require authentication?
        , call_headers       :: [Header]                 -- ^ HTTP headers relevant to the call
        , call_body          :: Maybe (APIType, String)  -- ^ Type and example of request body
        , call_params        :: [Param]                  -- ^ Query parameters relevant to the call
        , call_views         :: [View]                   -- ^ Available views of the result data
        , call_samples       :: [Sample]                 -- ^ Example responses
        }
    deriving (Show)

-- | Documents a HTTP header that may be supplied to a 'Call'
data Header
    = Header
        { header_name     :: String  -- ^ Header name
        , header_expl     :: String  -- ^ Example value for header
        , header_desc     :: String  -- ^ Free-form text description
        , header_type     :: APIType -- ^ Type of data in header
        , header_required :: Bool    -- ^ Is including the header with the request mandatory?
        } deriving (Show)

-- | Documents a URL query parameter that may be included with a 'Call'
data Param
    = Param
        { param_name     :: String                -- ^ Parameter name
        , param_expl     :: String                -- ^ Example value for parameter
        , param_desc     :: String                -- ^ Free-form text description
        , param_type     :: Either String APIType -- ^ Type of data in the parameter
        , param_required :: Bool                  -- ^ Is including the parameter mandatory?
        } deriving (Show)

-- | Documents a specific view of the result data available in a 'Call'
data View
    = View
        { view_id     :: String  -- ^ View name
        , view_type   :: APIType -- ^ Type of result data returned
        , view_doc    :: String  -- ^ Free-form text description
        , view_params :: [Param] -- ^ Query parameters that may be supplied for this view
        } deriving (Show)

-- | Example response data from a 'Call'
data Sample
    = Sample
        { sample_status   :: StatusCode    -- ^ HTTP status code for this example response
        , sample_type     :: Body APIType  -- ^ Type of example response
        , sample_response :: Maybe String  -- ^ Content of response, or 'Nothing' for empty response
        } deriving (Show)

-- | Type for 'Sample' response body, parameterised by possible JSON types
data Body t = EmptyBody        -- ^ An empty response
            | JSONBody  t      -- ^ A JSON response of the given type
            | OtherBody String -- ^ A non-empty, non-JSON response
  deriving (Functor, Show)

-- | Record of arguments that must be supplied to generate HTML
-- documentation for a 'Call'
data DocInfo
    = DocInfo
        { doc_info_call_url :: HTTPMethod -> [String] -> URL
          -- ^ URL for individual call documentation from the index
        , doc_info_type_url :: TypeName -> URL
          -- ^ URL for documentation of an API type
        }

renderBodyType :: DocInfo -> Body APIType -> String
renderBodyType _  EmptyBody     = "empty"
renderBodyType di (JSONBody ty) = "json&nbsp;&nbsp;" ++ renderAPIType di ty
renderBodyType _  (OtherBody s) = s

renderAPIType :: DocInfo -> APIType -> String
renderAPIType di (TyList  ty  ) = "[" ++ renderAPIType di ty ++ "]"
renderAPIType di (TySet  ty   ) = "{" ++ renderAPIType di ty ++ "}"
renderAPIType di (TyMaybe ty  ) = "?" ++ renderAPIType di ty
renderAPIType di (TyName  tn  ) = mk_link (doc_info_type_url di tn) (T.unpack (_TypeName tn))
renderAPIType _  (TyBasic bt  ) = pp bt
renderAPIType _  TyJSON         = "json"

mk_link :: URL -> String -> String
mk_link = printf "<b><a class='reflink' href='%s' >%s</a></b>"
