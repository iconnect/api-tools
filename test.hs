{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

import           Data.API.Test.Gen
import           Data.API.Test.DSL
import           Data.API.Types
import           Data.API.Markdown
import           Data.API.Generate
import           Data.API.Test.SimpleTest
import           Data.Aeson
import qualified Data.ByteString.Char8      as B
import qualified Data.Text                  as T
import           Control.Lens
import           Test.QuickCheck


main :: IO ()
main = 
 do print $ (decode $ encode test_IsoS     :: Maybe [IsoS])
    print $ (decode $ encode test_IsoB     :: Maybe [IsoB])
    print $ (decode $ encode test_IsoI     :: Maybe [IsoI])
    print $ (decode $ encode test_foo      :: Maybe Foo   )
    print $ (decode $ encode test_wibble_1 :: Maybe Wibble)
    print $ (decode $ encode test_wibble_2 :: Maybe Wibble)
    print $ (decode $ encode test_enumer   :: Maybe [Enumer])
    putStr $ markdown _TypeName example
    putStr $ markdown _TypeName example2
    writeFile "example.md"  $ markdown _TypeName example
    writeFile "example2.md" $ markdown _TypeName example2
    testAllWrappers

test_IsoS :: [IsoS]
test_IsoS = ["text newtype"]

test_IsoB :: [IsoB]
test_IsoB = [IsoB True]

test_IsoI :: [IsoI]
test_IsoI = [IsoI 42]

test_foo :: Foo
test_foo = Foo {_bar__Baz=True, _bar__Qux=42}

test_wibble_1 :: Wibble
test_wibble_1 = DRO_wubble [test_foo]

test_wibble_2 :: Wibble
test_wibble_2 = DRO_flubble "try this"

test_enumer :: [Enumer]
test_enumer = [ENM_wubble]


testAllWrappers :: IO ()
testAllWrappers =
 do putStrLn "example:"
    mkTestAll exampleSimpleTests
    putStrLn "example2:"
    mkTestAll example2SimpleTests


-- kill warnings

type R a = Data.Aeson.Result a

set' :: ASetter s t a b -> b -> s -> t
set' = set

quickCheck' :: Testable prop => prop -> IO ()
quickCheck' = quickCheck

type Binary_ = Binary

type BS = B.ByteString

bs :: String -> B.ByteString
bs = B.pack

pack :: String -> T.Text
pack = T.pack
