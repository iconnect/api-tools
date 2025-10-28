{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | This module contains datatypes generated from the DSL description
-- of the api-tools API; they thus correspond to the types in
-- "Data.API.Types".
module Data.API.API.Gen where

import           Data.API.API.DSL
import           Data.API.Tools
import           GHC.Generics (Generic)

import           Language.Haskell.TH

$(generate         apiAPI)

deriving instance Generic TypeRef
deriving instance Generic Field
deriving instance Generic Conversion
deriving instance Generic UTCRange
deriving instance Generic IntRange
deriving instance Generic RegularExpression
deriving instance Generic SpecNewtype
deriving instance Generic APINode
deriving instance Generic APIType
deriving instance Generic Spec
deriving instance Generic Filter
deriving instance Generic DefaultValue
deriving instance Generic BasicType

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
