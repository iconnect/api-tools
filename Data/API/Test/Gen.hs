{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Data.API.Test.Gen where

import           Data.API.Test.DSL
import           Data.API.Generate

$(generate        example)
$(generateTools 0 example)

$(generate        example2)
$(generateTools 0 example2)
