{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Data.API.Test.Gen where

import           Data.API.Test.DSL
import           Data.API.Test.GenerateTests
import           Data.API.Generate
import           Control.Applicative

$(generate      example)
$(generateTools example)
$(generateTests example "exampleSimpleTests")

$(generate      example2)

data Coord = Coord Int Int
    deriving (Eq,Show)

instance Arbitrary Coord where
    arbitrary = Coord <$> arbitrary <*> arbitrary

inj_coord :: REP__Coord -> Parser Coord
inj_coord (REP__Coord x y) = return $ Coord x y

prj_coord :: Coord -> REP__Coord 
prj_coord (Coord x y) = REP__Coord x y

{-
instance FromJSON Coord where
    parseJSON x = parseJSON x >>= inj_coord

instance ToJSON Coord where
    toJSON = toJSON . prj_coord
-}

newtype Ssn = Ssn { _Ssn :: Integer }
    deriving(Eq,Show)

instance Arbitrary Ssn where
    arbitrary = Ssn <$> arbitrary

inj_ssn :: REP__Ssn -> Parser Ssn
inj_ssn = return . Ssn . fromIntegral . _REP__Ssn

prj_ssn :: Ssn -> REP__Ssn
prj_ssn = REP__Ssn . fromIntegral . _Ssn


data CHOICE = CHOICE { _CHOICE :: Int }
    deriving(Eq,Show)

instance Arbitrary CHOICE where
    arbitrary = CHOICE <$> arbitrary

inj_chc :: REP__CHOICE -> Parser CHOICE
inj_chc (CHC_a i) = return $ CHOICE i
inj_chc (CHC_b _) = mzero

prj_chc :: CHOICE -> REP__CHOICE
prj_chc (CHOICE i) = CHC_a i

newtype ENUM = ENUM Bool
    deriving (Show,Eq)

instance Arbitrary ENUM where
    arbitrary = ENUM <$> arbitrary

inj_enum :: REP__ENUM -> Parser ENUM
inj_enum ENM_e1 = return $ ENUM False
inj_enum ENM_e2 = return $ ENUM True

prj_enum :: ENUM -> REP__ENUM
prj_enum (ENUM False) = ENM_e1
prj_enum (ENUM True ) = ENM_e2

$(generateTools example2)
$(generateTests example2 "example2SimpleTests")
