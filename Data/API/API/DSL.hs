{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS_GHC -XNoCPP                 #-}

module Data.API.API.DSL
    ( apiAPI
    ) where

import           Data.API.Types
import           Data.API.Generate


apiAPI :: API
apiAPI = [api|

an :: APINode
    = record
        name       : string
        comment    : string
        prefix     : string
        spec       : Spec
        convert    : ? Conversion
        "version"  : integer
        log        : string

sp :: Spec
    = union
      | newtype    : BasicType   
      | "record"   : [Field]
      | "union"    : [Field]
      | enum       : [string]
      | synonym    : APIType 

cv :: Conversion
    = record
        injection  : string
        projection : string

fd :: Field
    = record
        name    : string
        type    : APIType
        comment : string

ty :: APIType
    = union
      | list  : APIType
      | maybe : APIType
      | name  : string
      | basic : BasicType

bt :: BasicType
    = "union" | "string" | "binary" | "boolean" | "integer"
|]
