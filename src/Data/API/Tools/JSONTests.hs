{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes          #-}

-- Here we use Template Haskell to generate a test suite for the Aeson wrappers
-- from the DSL description of same.

module Data.API.Tools.JSONTests
    ( jsonTestsTool
    ) where

import           Data.API.JSON
import           Data.API.Tools.Combinators
import           Data.API.Types
import           Language.Haskell.TH
import           Test.QuickCheck


-- | Tool to generate a list of tests of type @[('String', 'Property')]@
-- with the given name.  This depends on 'jsonTool' and 'quickCheckTool'.
jsonTestsTool :: Name -> APITool
jsonTestsTool nm api = return [sig, props]
  where
    sig   = SigD nm $ ListT `AppT` (TupleT 2 `AppT` ConT ''String `AppT` ConT ''Property)
    props = FunD nm [Clause [] (NormalB bdy) []]
    bdy   = ListE $ map generateProp [ an | ThNode an <- api ]

-- | For an APINode, generate a (String, Property) pair giving the
-- type name and an appropriate instance of the
-- prop_resultsMatchRoundtrip property
generateProp :: APINode -> Exp
generateProp an = TupE [ LitE $ StringL ty
                       , VarE 'property `AppE` SigE (VarE 'prop_resultsMatchRoundtrip)
                                                    (ArrowT `AppT` ConT ty_nm `AppT` ConT ''Bool) ]
  where
    ty = _TypeName $ anName an
    ty_nm = mkName ty
