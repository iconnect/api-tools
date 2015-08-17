{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS_GHC -XNoCPP                 #-}

module Data.API.Test.DSL where

import           Data.API.Parse
import           Data.API.Types


example :: API
example = map ThNode 
    [ APINode "IsoS" "simple String newtype defn" ""
          (SpNewtype $ SpecNewtype BTstring Nothing) Nothing
    , APINode "IsoB" "simple Bool   newtype defn" ""
          (SpNewtype $ SpecNewtype BTbool Nothing) Nothing
    , APINode "IsoI" "simple Int    newtype defn" ""
          (SpNewtype $ SpecNewtype BTint Nothing) Nothing
    , APINode "Foo" "a test defn" "bAr_" (SpRecord $ SpecRecord
            [ (,) "Baz" (FieldType (TyBasic BTbool) False Nothing "just a bool")
            , (,) "Qux" (FieldType (TyBasic BTint)  False Nothing "just an int")
            ]) Nothing
    , APINode "Wibble" "another test defn" "dro" (SpUnion $ SpecUnion
            [ (,) "wubble"  (TyList $ TyName "Foo", "list of Foo")
            , (,) "flubble" (TyBasic BTstring     , "a string"   )
            ]) Nothing
    , APINode "Enumer" "enum test defn" "enm" (SpEnum $ SpecEnum
            [ ("wubble", "")
            , ("flubble", "")
            ]) Nothing
    ]

example2 :: API
example2 = [api|

ssn :: Ssn
    = basic integer
    with inj_ssn, prj_ssn

ide :: Ide
    (* here is a block comment *)
    // and a simple comment
    = string

flg :: Flag
    = boolean

crt :: Cert
    = binary

poo :: Poo
    // A simple test example
    = record
        x :: integer
        y :: Foo

c :: Coord
    // A simple test example
    = record
        x :: integer // the x coordinate
                    // with a multi-line comment
        y :: integer (* and here we have a multi-line
                       block comment 
                       and this 
                     *)
    with inj_coord, prj_coord

twi :: Twiddle
    // A simple test example
    = record
        x :: [integer]   // a field
        y :: ? [binary]  // another field

chc :: CHOICE
    = union
        | a :: integer
        | b :: string
    with inj_chc, prj_chc

enm :: ENUM
    = enum
        | e1
        | e2
    with inj_enum, prj_enum

uv :: MyUTC
    = utc

urec :: URec
    = record
        foo :: utc


fi :: FilteredInt
    = basic integer | >= 3, <= 5

fs :: FilteredString
    = basic string | "cab*age"

fu :: FilteredUTC
    = basic utc | >= 2014-10-13T15:20:11Z

bb :: BasicBinary
    = basic binary

j :: JSON
    = json

nr :: NewRec
    = record
        bb :: BasicBinary
        j  :: JSON

nu :: NewUnion
    = union
      | bb :: BasicBinary
      | j  :: JSON
|]
