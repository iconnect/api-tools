
-- Some simple boilerplate for Detailed-0.9/Cabal-1.14.0/QuickCheck
-- test suites

module Data.API.Test.SimpleTest 
    ( SimpleTest(..)
    , mkTests
    , mkTestAll
    , mkTestAllVerbose
    , module Distribution.TestSuite
    ) where

import           Text.Printf
import qualified Test.QuickCheck                as QC
import qualified Distribution.TestSuite         as TS
import           Distribution.TestSuite


-- Our simple QC tests and instances to insert into the Cabal framework.

data SimpleTest = ST
    { nameST :: String
    , testST :: Bool -> IO QC.Result
    }

instance TS.TestOptions SimpleTest where
    name           = nameST
    options        = const []
    defaultOptions = const $ return $ TS.Options []
    check          = const $ const $ return []

instance TS.ImpureTestable SimpleTest where
    runM st _ = cnv_result `fmap` testST st False

cnv_result :: QC.Result -> TS.Result
cnv_result qcr =
        case qcr of
          QC.Success _ _ _ -> TS.Pass
          _                -> TS.Fail "Failed"

-- | Detailed-0.9/Cabal-1.14.0 test suite:

mkTests :: [SimpleTest] -> [TS.Test] 
mkTests = map TS.impure

-- | Something to run in ghci:

mkTestAll :: [SimpleTest] -> IO ()
mkTestAll = mapM_ part
      where
        part st = 
             do putStr $ printf "%-25s: " $ nameST st
                testST st False


-- | Verbose tests from ghci:

mkTestAllVerbose :: [SimpleTest] -> IO ()
mkTestAllVerbose = mapM_ part
      where
        part st = 
             do putStr $ printf "\n%s:\n" $ nameST st
                testST st True
