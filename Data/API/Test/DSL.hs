{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS_GHC -XNoCPP                 #-}

module Data.API.Test.DSL where

import           Data.API.Types
import           Data.API.Generate


example :: API
example =
    [ APINode "IsoS" "simple String newtype defn" "" (SpNewtype $ SpecNewtype BTstring) Nothing 1 ""
    , APINode "IsoB" "simple Bool   newtype defn" "" (SpNewtype $ SpecNewtype BTbool  ) Nothing 1 ""
    , APINode "IsoI" "simple Int    newtype defn" "" (SpNewtype $ SpecNewtype BTint   ) Nothing 1 ""
    , APINode "Foo" "a test defn" "bAr_" (SpRecord $ SpecRecord
            [ (,) "Baz" (TyBasic BTbool,"just a bool")
            , (,) "Qux" (TyBasic BTint ,"just an int")
            ]) Nothing 1 ""
    , APINode "Wibble" "another test defn" "dro" (SpUnion $ SpecUnion
            [ (,) "wubble"  (TyList $ TyName $ "Foo","list of Foo")
            , (,) "flubble" (TyBasic BTstring       ,"a string"   )
            ]) Nothing 1 ""
    , APINode "Enumer" "enum test defn" "enm" (SpEnum $ SpecEnum
            [ "wubble"
            , "flubble"
            ]) Nothing 1 ""
    ]

example2 :: API
example2 = [api|

ssn :: Ssn
    = integer
    with inj_ssn, prj_ssn

ide :: Ide
    (* here is a block comment *)
    // and a simple comment
    = string
    version 1
    // need to add Migrate instances 
    // before we can bump the version 

flg :: Flag
    = boolean

crt :: Cert
    = binary

poo :: Poo
    // A simple test example
    = record
        x : integer
        y : Foo

c :: Coord
    // A simple test example
    = record
        x : integer // the x coordinate
                    // with a multi-line comment
        y : integer (* and here we have a multi-line
                       block comment 
                       and this 
                     *)
    with inj_coord, prj_coord

twi :: Twiddle
    // A simple test example
    = record
        x : [integer]   // a field
        y : ? [binary]  // another field

chc :: CHOICE
    = union
        | a : integer
        | b : string
    with inj_chc, prj_chc

enm :: ENUM
    = e1 | e2
    with inj_enum, prj_enum
|]
