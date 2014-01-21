{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS_GHC -XNoCPP                 #-}

module Data.API.API.DSL
    ( apiAPI
    ) where

import           Data.API.Parse
import           Data.API.Types


-- | API description of the api-tools API itself
apiAPI :: API
apiAPI = [api|

//
// #The api-tools API
//
// Here we make the DSP specs available in JSON so that clients
// working in non-Haskell frameworks can integrate API
// specs into their own environments.

ans :: APISpec
    // an API specification consists of a list of nodes,
    // each describing a named JSON thing which can be
    // referenced in other (object) nodes.
    = [APINode]

an :: APINode
    // Each node is named (with a name that conforms to Haskell's
    // Type identifier rules, starting with a capital letter) and some
    // comment text (like this comment). The JSON spec is all contained
    // in the {Spec}, the other fields being used behind the scenes
    // to manage the Haskell name space and migrations.
    = record
        name      :: string         // the name must start with a big letter
                                    // and conform to Haskell rules for type
                                    // identifiers
        comment   :: string         // natural language description of the node
        prefix    :: string         // (Haskell side only) this prefix used to
                                    // structure the Haskell name space
                                    // (which is uncomfortably flat where
                                    // record field names are concerned);
                                    // it has no effect on the JSON representation
        spec      :: Spec           // here we specify the JSON representation
        convert   :: ? Conversion   // (Haskell side only) sometimes we may
                                    // choose to not use the default internal
                                    // representation for the JSON
                                    // but instead supply a couple of functions
                                    // for injecting the default representation
                                    // into the actual type we will use and the
                                    // and project it back again; here we can check
                                    // some properties (which must be explained
                                    // in the comments) and reject a request
                                    // with a 400 error; has no effect on the
                                    // JSON representation
        'version' :: integer        // (Haskell side mostly) the version number
                                    // for handling server migrations
        log       :: string         // a log explaining the changes that have
                                    // been made through the migrations

sp :: Spec
    // the JSON representation is specified as a simple 'newtype', a record
    // type, a union type, an enumerated type or a (mere) type synonym.
    = union
      | newtype   :: SpecNewtype    // here we have a basic string or number type
      | 'record'  :: [Field]        // an object representing a record
      | 'union'   :: [Field]        // an object representing a number of alternatives
      | 'enum'    :: [string]       // a string type which must contain a number of
                                    // discrete values
      | synonym   :: APIType        // is just an alias for another type

sn :: SpecNewtype
    // 'newtype' specs are a basic type and an optional filter specification
    = record
        type      :: BasicType
        filter    :: ? Filter

ft :: Filter
    = union
      | 'string'  :: RegularExpression
      | 'integer' :: IntRange
      | 'utc'     :: UTCRange

re :: RegularExpression
    // regular expressions are represented by simple strings
    = basic string

ir :: IntRange
    // integer ranges are specified with their bounds and may open at
    // the bottom and top
    = record
        lo :: ? integer
        hi :: ? integer

ur :: UTCRange
    // UTC ranges are specified with their bounds and may open at
    // the bottom and top
    = record
        lo :: ? utc
        hi :: ? utc

cv :: Conversion
    // Conversions are just used on the Haskell side to map the concrete JSON
    // representation into an internal type and back again
    = record
        injection :: string         // the injection function for injecting
                                    // representations into the internal
                                    // representation; may return a failure
                                    // indicating that some API precondition
                                    // was not met
        projection:: string         // the projection function converts the
                                    // internal type back into the JSON
                                    // representation for communication
                                    // back to the client

fd :: Field
    // a field represent both a field in a record object and an alternative in
    // a union object (in which exactly one of the 'fields' is ever present in
    // the object)
    = record
        name     :: string          // the name of the method
        type     :: APIType         // the JSON type of the field
        readonly :: boolean         // read-only status
        default  :: ? DefaultValue  // the default value of the field
        comment  :: string          // a comment describing the field (like
                                    // this one)

ty :: APIType
    // this is used to represent JSON types in fields (or synonyms) and is one
    // one of the following:
    = union
      | list   :: APIType           // a JSON list of the given type
      | maybe  :: APIType           // either the given type or the null value
      | ref    :: TypeRef           // a named type (node) with possible example
      | 'basic':: BasicType         // a basic JSON type
      | 'json' :: integer           // a generic JSON value

tr :: TypeRef
    // reference to a type name
    = basic string

bt :: BasicType
    // finally we get down to the basic JSON types ('binary' is a string
    // in which the byte string has been encoded with base-64, safe for
    // embedding in a UTF-8-encoded JSON string
    = enum
        | 'string'
        | 'binary'
        | 'boolean'
        | 'integer'
        | 'utc'

dv :: DefaultValue
    // a default value
    = union
        | 'list'    :: integer
        | 'maybe'   :: integer
        | 'string'  :: string
        | 'boolean' :: boolean
        | 'integer' :: integer
        | 'utc'     :: utc
|]

{-
-}
