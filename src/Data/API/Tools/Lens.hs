{-# LANGUAGE TemplateHaskell            #-}
module Data.API.Tools.Lens
    ( lensTool
    , binary
    ) where

import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Types

import           Control.Lens


-- | Tool to make lenses for fields in generated types.
lensTool :: APITool
lensTool = apiDataTypeTool $ simpleTool $ makeLenses . rep_type_nm


$(makeLenses ''Binary)
