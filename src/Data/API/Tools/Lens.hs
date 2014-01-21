{-# LANGUAGE TemplateHaskell            #-}
module Data.API.Tools.Lens
    ( lensTool
    , binary
    ) where

import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Types

import           Control.Lens


lensTool :: APITool
lensTool = apiDataTypeTool (makeLenses . rep_type_nm)


$(makeLenses ''Binary)
