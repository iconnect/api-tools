{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.API.PerfTest
    ( main
    ) where

import           Data.API.API.DSL
import qualified Data.API.API.Gen               as Gen
import           Data.API.NormalForm
import           Data.API.Types
import qualified Data.API.Value                 as Value

import           Control.DeepSeq
import           Control.Exception
import qualified Data.Aeson                     as JS
import qualified Data.Binary.Serialise.CBOR     as CBOR
import           Data.Binary.Serialise.CBOR.Extra
import           Data.Time
import           System.Environment
import           Test.QuickCheck

tyDesc :: APIType

{-
type T = [UTCTime]
tyDesc = TyList (TyBasic BTutc)
-}

type T = Gen.APISpec
tyDesc = TyName "APISpec"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["read"        , fp] -> do x <- CBOR.readFileDeserialise fp
                               _ <- evaluate (force (x :: T))
                               return ()
    ["read-aeson"  , fp] -> do x <- CBOR.readFileDeserialise fp
                               _ <- evaluate (force (x :: JS.Value))
                               return ()
    ["read-generic", fp] -> do x <- readFileDeserialiseWith (Value.decode (apiNormalForm apiAPI) tyDesc) fp
                               _ <- evaluate (force x)
                               return ()
    ["write"        ,fp] -> do x <- generate (resize 500 arbitrary)
                               CBOR.writeFileSerialise fp (x :: T)
    ["write"      ,fp,i] -> do x <- generate (resize (read i) arbitrary)
                               CBOR.writeFileSerialise fp (x :: T)
    _ -> error "perf-test (read|read-generic|write) <file>"
