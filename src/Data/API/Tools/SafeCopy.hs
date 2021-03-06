{-# LANGUAGE TemplateHaskell            #-}

module Data.API.Tools.SafeCopy
    ( safeCopyTool
    ) where

import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes

import           Data.SafeCopy


-- | Tool to derive 'SafeCopy' instances for generated types.  At
-- present, this derives only base version instances.
safeCopyTool :: APITool
safeCopyTool = apiDataTypeTool $ simpleTool $ deriveSafeCopy 0 'base . rep_type_nm
