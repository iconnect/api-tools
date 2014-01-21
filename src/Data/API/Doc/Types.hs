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
    , htmlBody
    , renderAPIType
    , renderBodyType
    , mk_link
    ) where

import           Data.API.Types

import           Text.Printf

type URL        = String
type HTTPMethod = String
type StatusCode = Int

data Call
    = Call
        { call_http_method   :: HTTPMethod
        , call_path          :: [String]
        , call_description   :: String
        , call_auth_required :: Bool
        , call_headers       :: [Header]
        , call_body          :: Maybe (APIType, String)
        , call_params        :: [Param]
        , call_views         :: [View]
        , call_samples       :: [Sample]
        }
    deriving (Show)

data Header
    = Header
        { header_name     :: String
        , header_expl     :: String
        , header_desc     :: String
        , header_type     :: APIType
        , header_required :: Bool
        } deriving (Show)

data Param
    = Param
        { param_name :: String
        , param_expl :: String
        , param_desc :: String
        , param_type     :: Either String APIType
        , param_required :: Bool
        } deriving (Show)

data View
    = View
        { view_id   :: String
        , view_type :: APIType
        , view_doc  :: String
        , view_params :: [Param]
        } deriving (Show)

data Sample
    = Sample
        { sample_status   :: StatusCode
        , sample_type     :: Body APIType
        , sample_response :: Maybe String
        } deriving (Show)

data Body t = EmptyBody
            | JSONBody  t
            | OtherBody String
  deriving (Functor, Show)

data DocInfo
    = DocInfo
        { doc_info_call_url :: HTTPMethod -> [String] -> URL
        , doc_info_type_url :: TypeName -> URL
        }


htmlBody :: Body t
htmlBody = OtherBody "html"

renderBodyType :: DocInfo -> Body APIType -> String
renderBodyType _  EmptyBody     = "empty"
renderBodyType di (JSONBody ty) = "json&nbsp;&nbsp;" ++ renderAPIType di ty
renderBodyType _  (OtherBody s) = s

renderAPIType :: DocInfo -> APIType -> String
renderAPIType di (TyList  ty  ) = "[" ++ renderAPIType di ty ++ "]"
renderAPIType di (TyMaybe ty  ) = "?" ++ renderAPIType di ty
renderAPIType di (TyName  tn  ) = mk_link (doc_info_type_url di tn) (_TypeName tn)
renderAPIType _  (TyBasic bt  ) = renderBasicType bt
renderAPIType _  TyJSON         = "json"

renderBasicType :: BasicType -> String
renderBasicType BTstring{} = "string"
renderBasicType BTbinary{} = "binary"
renderBasicType BTbool  {} = "bool"
renderBasicType BTint   {} = "int"
renderBasicType BTutc   {} = "utc"

mk_link :: URL -> String -> String
mk_link = printf "<b><a class='reflink' href='%s' >%s</a></b>"
