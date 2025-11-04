{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains datatypes generated from the DSL description
-- of the api-tools API; they thus correspond to the types in
-- "Data.API.Types".
module Data.API.API.Gen where

import           Data.API.API.DSL
import           Data.API.Tools

import           Language.Haskell.TH

$(generate         apiAPI)

$(generateAPITools apiAPI
                   [ enumTool
                   , jsonTool'
                   , cborTool
                   , deepSeqTool
                   , quickCheckTool
                   , lensTool
                   , safeCopyTool
                   , exampleTool
                   , samplesTool   (mkName "apiAPISamples")
                   , jsonTestsTool (mkName "apiAPITestsJSON")
                   , cborTestsTool (mkName "apiAPITestsCBOR")
                   , cborToJSONTestsTool 'apiAPI (mkName "apiAPITestsCBORToJSON")
                   , jsonToCBORTestsTool 'apiAPI (mkName "apiAPITestsJSONToCBOR")
                   ])
