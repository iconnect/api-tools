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
lensTool = apiDataTypeTool $ mkTool $ \ ts an ->
    if ok ts an then makeLenses $ rep_type_nm an else return []
  where
    -- Exclude newtypes if we are using smart constructors, because
    -- the lens can be used to bypass the invariant
    ok ts an | SpNewtype (SpecNewtype _ (Just _)) <- anSpec an = not (newtypeSmartConstructors ts)
             | otherwise                                       = True


$(makeLenses ''Binary)
