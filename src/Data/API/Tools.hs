-- | This module provides an interface for generating TH declarations
-- from an 'API'.  To use it, splice in a call to 'generate' followed
-- by one or more calls to 'generateAPITools', like so:
--
-- > $(generate myAPI)
-- > $(generateAPITools [enumTool, jsonTool', quickCheckTool] myAPI)
--
-- If you wish to override any of the instances generated by the
-- tools, you can do so by writing instance declarations after the
-- call to 'generate' but before the call to 'generateAPITools'.

module Data.API.Tools
    ( generate
    , generateAPITools

      -- * Tool settings
    , generateWith
    , generateAPIToolsWith
    , ToolSettings
    , defaultToolSettings
    , warnOnOmittedInstance
    , newtypeSmartConstructors

      -- * Individual tools
    , enumTool
    , exampleTool
    , deepSeqTool
    , jsonTool
    , jsonTool'
    , jsonTestsTool
    , jsonTestsToolCBOR
    , lensTool
    , quickCheckTool
    , safeCopyTool
    , samplesTool
    ) where

import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Tools.DeepSeq
import           Data.API.Tools.Enum
import           Data.API.Tools.Example
import           Data.API.Tools.JSON
import           Data.API.Tools.JSONTests
import           Data.API.Tools.Lens
import           Data.API.Tools.QuickCheck
import           Data.API.Tools.SafeCopy
import           Data.API.Types

import           Data.Monoid
import           Language.Haskell.TH


-- | Generate the datatypes corresponding to an API.
generate :: API -> Q [Dec]
generate = generateWith defaultToolSettings

-- | Generate the datatypes corresponding to an API, allowing the
-- 'ToolSettings' to be overriden.
generateWith :: ToolSettings -> API -> Q [Dec]
generateWith ts api = generateAPIToolsWith ts api [datatypesTool]

-- | Apply a list of tools to an 'API', generating TH declarations.
-- See the individual tool descriptions for details.  Note that
-- 'generate' must be called first, and some tools have dependencies,
-- which must be included in the same or a preceding call to
-- 'generateAPITools'.
generateAPITools :: API -> [APITool] -> Q [Dec]
generateAPITools = generateAPIToolsWith defaultToolSettings

-- | Apply a list of tools to an 'API', generating TH declarations.
-- This form allows the 'ToolSettings' to be overridden.
generateAPIToolsWith :: ToolSettings -> API -> [APITool] -> Q [Dec]
generateAPIToolsWith ts api tools = runTool (mconcat tools) ts api
