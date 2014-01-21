{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Data.API.Tools.SafeCopy
    ( safeCopyTool
    ) where

import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Types

import           Data.SafeCopy


safeCopyTool :: APITool
safeCopyTool = apiDataTypeTool safeCopyNodeTool

safeCopyNodeTool :: APINodeTool
safeCopyNodeTool an = deriveSafeCopy 0 'base $ rep_type_nm an


$(deriveSafeCopy 0 'base ''Binary)
