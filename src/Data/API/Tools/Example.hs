{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Tool for generating documentation-friendly examples
module Data.API.Tools.Example
    ( Example(..)
    , exampleTool
    , samplesTool
    ) where

import           Data.API.TH
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Types

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Char8          as B
import           Data.Time
import           Language.Haskell.TH
import           Safe
import           Test.QuickCheck                as QC
import qualified Data.Text                      as T


-- | The Example class is used to generate a documentation-friendly
-- example for each type in the model

class Example a where
    -- | Generator for example values; defaults to 'arbitrary' if not
    -- specified
    example :: Gen a
    default example :: Arbitrary a => Gen a
    example = arbitrary

instance Example a => Example (Maybe a) where
    example = oneof [return Nothing, Just <$> example]

instance Example a => Example [a] where
    example = listOf example

instance Example Int where
    example = arbitrarySizedBoundedIntegral `suchThat` (> 0)

instance Example Bool where
    example = choose (False, True)

instance Example T.Text where
    example = return "Mary had a little lamb"

instance Example Binary where
    example = return $ Binary $ B.pack "lots of 1s and 0s"

instance Example Value where
    example = return $ String "an example JSON value"

instance Example UTCTime where
    example = return $ fromJustNote dg $ parseUTC_ "2013-06-09T15:52:30Z"
      where
        dg = "Data.API.Tools.Example-UTCTime"


-- | Generate a list of (type name, sample generator) pairs
-- corresponding to each type in the API, with samples encoded as
-- JSON.  This depends on the 'Example' instances generated by
-- 'exampleTool'.  It generates something like this:
--
-- > samples :: [(String, Gen Value)]
-- > samples = [("Foo", fmap toJSON (example :: Gen Foo)), ... ]

samplesTool :: Name -> APITool
samplesTool nm api' = simpleSigD nm [t| [(String, Gen Value)] |]
                                 (listE [ gen_sample nd | ThNode nd <- api' ])
  where
    gen_sample :: APINode -> ExpQ
    gen_sample an = [e| ($str, fmap toJSON (example :: Gen $(nodeT an))) |]
      where
        str = stringE $ _TypeName $ anName an


-- | Tool to generate 'Example' instances for types generated by
-- 'datatypesTool'.  This depends on 'quickCheckTool'.
exampleTool :: APITool
exampleTool = apiNodeTool $ apiSpecTool gen_sn_ex gen_sr_ex gen_su_ex gen_se_ex (const emptyTool)


-- | Generate an 'Example' instance for a newtype.  If there is no
-- filter, call 'example' on the underlying type; otherwise, use
-- 'arbitrary'.  Like 'Arbitrary', if a regular expression filter is
-- applied the instance must be defined manually.
gen_sn_ex :: APINode -> SpecNewtype -> Q [Dec]
gen_sn_ex an sn = case snFilter sn of
                               Just (FtrStrg _) -> return []
                               Just _           -> inst [e| QC.arbitrary |]
                               Nothing          -> inst [e| fmap $(nodeConE an) example |]
  where
    inst e = optionalInstanceD ''Example [nodeRepT an] [simpleD 'example e]


-- | Generate an 'Example' instance for a record:
--
-- > instance Example Foo where
-- >     example = sized $ \ x -> Foo <$> resize (x `div` 2) example <*> ... <*> resize (x `div` 2) example

gen_sr_ex :: APINode -> SpecRecord -> Q [Dec]
gen_sr_ex an sr = optionalInstanceD ''Example [nodeRepT an] [simpleD 'example bdy]
  where
    bdy   = do x <- newName "x"
               appE (varE 'QC.sized) $ lamE [varP x] $
                 applicativeE (nodeConE an) $
                 replicate (length $ srFields sr) $
                 [e| QC.resize ($(varE x) `div` 2) example |]


-- | Generate an 'Example' instance for a union:
--
-- > instance Example Foo where
-- >     example = oneOf [ fmap Bar example, fmap Baz example ]

gen_su_ex :: APINode -> SpecUnion -> Q [Dec]
gen_su_ex an su = optionalInstanceD ''Example [nodeRepT an] [simpleD 'example bdy]
  where
    bdy | null (suFields su) = nodeConE an
        | otherwise          = [e| oneof $(listE alts) |]

    alts = [ [e| fmap $(nodeAltConE an k) example |]
           | (k,_) <- suFields su ]


-- | Generate an 'Example' instance for an enumeration, with no
-- definition for the 'example' method, because we can inherit the
-- behaviour of 'Arbitrary':
--
-- > instance Example Foo

gen_se_ex :: APINode -> SpecEnum -> Q [Dec]
gen_se_ex an _ = optionalInstanceD ''Example [nodeRepT an] []
