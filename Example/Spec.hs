{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Example.Spec where

import           Data.API.Aeson.Spec
import           Data.API.Aeson.Generate
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set


example :: APISpec
example =
    [ APISpeclet "IsoS" "simple String newtype defn" "" $ SpNewtype $ SpecNewtype BTstring
    , APISpeclet "IsoB" "simple Bool   newtype defn" "" $ SpNewtype $ SpecNewtype BTbool
    , APISpeclet "IsoI" "simple Int    newtype defn" "" $ SpNewtype $ SpecNewtype BTint
    , APISpeclet "Foo" "a test defn" "bAr_" $ SpRecord $ SpecRecord $ Map.fromList
            [ (,) "Baz" (TyBasic BTbool,"just a bool")
            , (,) "Qux" (TyBasic BTint ,"just an int")
            ]
    , APISpeclet "Wibble" "another test defn" "dro" $ SpUnion $ SpecUnion $ Map.fromList
            [ (,) "wubble"  (TyList $ TyName $ "Foo","list of Foo")
            , (,) "flubble" (TyBasic BTstring        ,"a string"  )
            ]
    , APISpeclet "Enumer" "enum test defn" "enm" $ SpEnum $ SpecEnum $ Set.fromList
            [ "wubble"
            , "flubble"
            ]
    ]

example2 :: APISpec
example2 = [api|
poo :: Poo
    // A simple test example
    = record
        x : Poo
        y : Foo

_c_ :: Coord
    // A simple test example
    = record
        x : Int     // the x coordinate
        y : Int     // the y coordinate

coord_ :: Coordinate
    // A simple test example
    = record
        x : Int     // the x coordinate
        y : Int     // the y coordinate
|]