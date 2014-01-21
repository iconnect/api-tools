{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}

-- | This module contains datatypes generated from the DSL description
-- of the api-tools API; they thus correspond to the types in
-- "Data.API.Types".
module Data.API.API.Gen where

import           Data.API.API.DSL
import           Data.API.Tools

$(generate          apiAPI)
$(generateInstances apiAPI)
$(generateTools     apiAPI)
$(generateTests     apiAPI "apiAPISimpleTests")
