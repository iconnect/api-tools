{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

import           Data.API.API
import           Data.API.API.Gen
import           Data.API.API.DSL
import           Data.API.Test.Gen
import           Data.API.Test.DSL

import           Data.API.Markdown
import           Data.API.Generate
import           Data.API.Test.SimpleTest
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Char8      as B
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as BL
import           Control.Lens
import           Test.QuickCheck


main :: IO ()
main = 
 do print $ (decode $ encode test_IsoS     :: Maybe [IsoS]  )
    print $ (decode $ encode test_IsoB     :: Maybe [IsoB]  )
    print $ (decode $ encode test_IsoI     :: Maybe [IsoI]  )
    print $ (decode $ encode test_foo      :: Maybe Foo     )
    print $ (decode $ encode test_wibble_1 :: Maybe Wibble  )
    print $ (decode $ encode test_wibble_2 :: Maybe Wibble  )
    print $ (decode $ encode test_enumer   :: Maybe [Enumer])
    putStr $ markdown defaultMarkdownMethods example
    putStr $ markdown defaultMarkdownMethods example2
    writeFile "example.md"  $ markdown defaultMarkdownMethods example
    writeFile "example2.md" $ markdown defaultMarkdownMethods example2
    testAllWrappers

dump :: IO ()
dump = BL.putStrLn $ encodePretty $ extractAPI apiAPI

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
    putStrLn "apiAPI:"
    mkTestAll apiAPISimpleTests


-- kill warnings about unnecessary module imports (used for ghci
-- tests and experiments)

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
