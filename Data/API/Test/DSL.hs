{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Data.API.Test.DSL where

import           Data.API.Types
import           Data.API.Generate
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set


example :: API
example =
    [ APINode "IsoS" "simple String newtype defn" "" $ SpNewtype $ SpecNewtype BTstring
    , APINode "IsoB" "simple Bool   newtype defn" "" $ SpNewtype $ SpecNewtype BTbool
    , APINode "IsoI" "simple Int    newtype defn" "" $ SpNewtype $ SpecNewtype BTint
    , APINode "Foo" "a test defn" "bAr_" $ SpRecord $ SpecRecord $ Map.fromList
            [ (,) "Baz" (TyBasic BTbool,"just a bool")
            , (,) "Qux" (TyBasic BTint ,"just an int")
            ]
    , APINode "Wibble" "another test defn" "dro" $ SpUnion $ SpecUnion $ Map.fromList
            [ (,) "wubble"  (TyList $ TyName $ "Foo","list of Foo")
            , (,) "flubble" (TyBasic BTstring        ,"a string"  )
            ]
    , APINode "Enumer" "enum test defn" "enm" $ SpEnum $ SpecEnum $ Set.fromList
            [ "wubble"
            , "flubble"
            ]
    ]

example2 :: API
example2 = [api|
poo :: Poo
    // A simple test example
    = record
        x : Poo
        y : Foo

_c_ :: Coord
    // A simple test example
    = record
        x : integer // the x coordinate
        y : integer // the y coordinate

twi_ :: Twiddle
    // A simple test example
    = record
        x : [integer]   // a field
        y : ? [binary]  // another field

ssn :: Ssn
    = integer

ide :: Ide
    = string

flg :: Flag
    = boolean

crt :: Cert
    = binary
|]