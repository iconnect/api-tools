{-# LANGUAGE TemplateHaskell      #-}

-- Here we use Template Haskell to generate a test suite for the Aeson wrappers
-- from the DSL description of same.

module Data.API.Test.GenerateTests
    ( generateTests
    , mk_aet
    , AETest(..)
    ) where

import           Data.API.Types
import           Data.API.Test.SimpleTest
import           Language.Haskell.TH
import           Test.QuickCheck
import           Data.Aeson


-- | generate a SimpleTest test suite the given API and bind it to nm 

generateTests :: API -> String -> Q [Dec]
generateTests api s = return $ [gen_sig nm, gen_suite nm api]
  where
    nm = mkName s

-- calculate the type signature declaration for the test suite
--
--  nm :: [SimpleTest]

gen_sig :: Name -> Dec
gen_sig nm = SigD nm $ AppT ListT $ ConT ''SimpleTest

-- calculate the function definition for the test suite

gen_suite :: Name -> API -> Dec
gen_suite nm api = FunD nm [Clause [] (NormalB e) []]
  where
    e = ListE $ map gen_test api

-- generate a single component of the test suite 

gen_test :: APINode -> Exp
gen_test an =
    AppE (VarE '_AETest) $ SigE (AppE (VarE 'mk_aet) k_s) $ 
                                        AppT (ConT ''AETest) (ConT k_nm)
  where
    k_nm = mkName s

    k_s  = LitE $ StringL s
    
    s    = _TypeName $ anName an

-- AETest adds a phantom index to SimpleTest as a convenience
-- to simplify generators for aeson-round-trip tests.

newtype AETest a = AETest { _AETest :: SimpleTest }

-- Check aeson decode is an inverse of encode, namely that:
--
--       decode (encode [x]) == [x]
--
-- N.B. x rwapped in list as JSON not good for encoding simple
--      data types like strings and numbers and aeson strictly
--      adheres to the standard.

mk_aet :: (Arbitrary a,Show a,FromJSON a,ToJSON a,Eq a) => String -> AETest a
mk_aet nm0 = tst $ \x -> maybe False (==[x]) $ decode $ encode [x]
      where
        nm     = "aeson." ++ nm0

        tst    :: (Arbitrary a,Show a,FromJSON a,ToJSON a,Eq a) => 
                                                        (a->Bool) -> AETest a
        tst p  = AETest $ ST nm $ qc p

        qc p v = if v then verboseCheckResult p else quickCheckResult p
