{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Data.API.API.Gen where

import           Data.API.API.DSL
import           Data.API.Tools

$(generate          apiAPI)
$(generateInstances apiAPI)
$(generateTools     apiAPI)
$(generateTests     apiAPI "apiAPISimpleTests")
