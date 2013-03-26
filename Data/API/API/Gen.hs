{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Data.API.API.Gen where

import           Data.API.API.DSL
import           Data.API.Test.GenerateTests
import           Data.API.Generate

$(generate      apiAPI)
$(generateTools apiAPI)
$(generateTests apiAPI "apiAPISimpleTests")
