{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Data.API.Test.Gen where

import           Data.API.JSON
import           Data.API.Test.DSL hiding (example)
import qualified Data.API.Test.DSL as DSL
import           Data.API.Tools
import           Data.API.Tools.Example
import           Control.Applicative
import           Language.Haskell.TH
import           Test.QuickCheck                ( Arbitrary(..) )

$(generate         DSL.example)
$(generateAPITools DSL.example
                   [ enumTool
                   , jsonTool
                   , quickCheckTool
                   , lensTool
                   , safeCopyTool
                   , exampleTool
                   , samplesTool   (mkName "exampleSamples")
                   , jsonTestsTool (mkName "exampleSimpleTests")
                   ])

$(generate      example2)

data Coord = Coord Int Int
    deriving (Eq,Show)

instance Arbitrary Coord where
    arbitrary = Coord <$> arbitrary <*> arbitrary

instance Example Coord

inj_coord :: REP__Coord -> ParserWithErrs Coord
inj_coord (REP__Coord x y) = pure $ Coord x y

prj_coord :: Coord -> REP__Coord 
prj_coord (Coord x y) = REP__Coord x y


newtype Ssn = Ssn { _Ssn :: Integer }
    deriving(Eq,Show)

instance Arbitrary Ssn where
    arbitrary = Ssn <$> arbitrary

instance Example Ssn

inj_ssn :: REP__Ssn -> ParserWithErrs Ssn
inj_ssn = return . Ssn . fromIntegral . _REP__Ssn

prj_ssn :: Ssn -> REP__Ssn
prj_ssn = REP__Ssn . fromIntegral . _Ssn


data CHOICE = CHOICE { _CHOICE :: Int }
    deriving(Eq,Show)

instance Arbitrary CHOICE where
    arbitrary = CHOICE <$> arbitrary

instance Example CHOICE

inj_chc :: REP__CHOICE -> ParserWithErrs CHOICE
inj_chc (CHC_a i) = return $ CHOICE i
inj_chc (CHC_b _) = empty

prj_chc :: CHOICE -> REP__CHOICE
prj_chc (CHOICE i) = CHC_a i

newtype ENUM = ENUM Bool
    deriving (Show,Eq)

instance Arbitrary ENUM where
    arbitrary = ENUM <$> arbitrary

instance Example ENUM

inj_enum :: REP__ENUM -> ParserWithErrs ENUM
inj_enum ENM_e1 = return $ ENUM False
inj_enum ENM_e2 = return $ ENUM True

prj_enum :: ENUM -> REP__ENUM
prj_enum (ENUM False) = ENM_e1
prj_enum (ENUM True ) = ENM_e2

$(generateAPITools example2
                   [ enumTool
                   , jsonTool
                   , quickCheckTool
                   , lensTool
                   , safeCopyTool
                   , exampleTool
                   , samplesTool   (mkName "example2Samples")
                   , jsonTestsTool (mkName "example2SimpleTests")
                   ])
