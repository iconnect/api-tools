{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Data.API.Test.Gen where

import           Data.API.Test.DSL hiding (example)
import qualified Data.API.Test.DSL as DSL
import           Data.API.Tools
import           Data.API.Tools.Example

import           Control.Applicative
import qualified Data.Aeson                     as JS
import           Data.SafeCopy
import qualified Data.Text                      as T
import           Language.Haskell.TH
import           Test.QuickCheck                ( Arbitrary(..) )

$(generate         DSL.example)
$(generateAPITools DSL.example
                   [ enumTool
                   , jsonTool'
                   , cborTool'
                   , quickCheckTool
                   , lensTool
                   , safeCopyTool
                   , exampleTool
                   , samplesTool   (mkName "exampleSamples")
                   , jsonTestsTool (mkName "exampleTestsJSON")
                   , cborTestsTool (mkName "exampleTestsCBOR")
                   , cborToJSONTestsTool 'DSL.example (mkName "exampleTestsCBORToJSON")
                   , jsonToCBORTestsTool 'DSL.example (mkName "exampleTestsJSONToCBOR")
                   ])

$(generateWith (defaultToolSettings { newtypeSmartConstructors = True }) example2)

data Coord = Coord Int Int
    deriving (Eq,Show)

instance Arbitrary Coord where
    arbitrary = Coord <$> arbitrary <*> arbitrary

instance Example Coord

inj_coord :: Applicative p => REP__Coord -> p Coord
inj_coord (REP__Coord x y) = pure $ Coord x y

prj_coord :: Coord -> REP__Coord
prj_coord (Coord x y) = REP__Coord x y

newtype Ssn = Ssn { _Ssn :: Integer }
    deriving(Eq,Show)

instance Arbitrary Ssn where
    arbitrary = Ssn <$> arbitrary

instance Example Ssn

inj_ssn :: Monad m => REP__Ssn -> m Ssn
inj_ssn = return . Ssn . fromIntegral . _REP__Ssn

prj_ssn :: Ssn -> REP__Ssn
prj_ssn = REP__Ssn . fromIntegral . _Ssn


data CHOICE = CHOICE { _CHOICE :: Int }
    deriving(Eq,Show)

instance Arbitrary CHOICE where
    arbitrary = CHOICE <$> arbitrary

instance Example CHOICE

inj_chc :: Monad m => REP__CHOICE -> m CHOICE
inj_chc (CHC_a i) = return $ CHOICE i
inj_chc (CHC_b _) = fail "no choice"

prj_chc :: CHOICE -> REP__CHOICE
prj_chc (CHOICE i) = CHC_a i

newtype ENUM = ENUM Bool
    deriving (Show,Eq)

instance Arbitrary ENUM where
    arbitrary = ENUM <$> arbitrary

instance Example ENUM

inj_enum :: Monad m => REP__ENUM -> m ENUM
inj_enum ENM_e1 = return $ ENUM False
inj_enum ENM_e2 = return $ ENUM True

prj_enum :: ENUM -> REP__ENUM
prj_enum (ENUM False) = ENM_e1
prj_enum (ENUM True ) = ENM_e2


instance Arbitrary FilteredString where
  arbitrary = pure $ UnsafeMkFilteredString "cabbage"

instance Example FilteredString


-- | These instances are required by the generated code, but we don't
-- really want to force them on clients of the library, so just define
-- orphans here.
instance Arbitrary JS.Value where
    arbitrary = JS.String . T.pack <$> arbitrary

instance SafeCopy JS.Value where
  getCopy = error "Not implemented"
  putCopy = error "Not implemented"

$(generateAPIToolsWith (defaultToolSettings { newtypeSmartConstructors = True }) example2
                   [ enumTool
                   , jsonTool'
                   , cborTool'
                   , quickCheckTool
                   , lensTool
                   , safeCopyTool
                   , exampleTool
                   , samplesTool   (mkName "example2Samples")
                   , jsonTestsTool (mkName "example2TestsJSON")
                   , cborTestsTool (mkName "example2TestsCBOR")
                   , jsonTestsToolCBOR (mkName "example2TestsJSONCBOR")
                   , cborToJSONTestsTool 'example2 (mkName "example2TestsCBORToJSON")
                   , jsonToCBORTestsTool 'example2 (mkName "example2TestsJSONToCBOR")
                   ])
