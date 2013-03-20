{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Example.Gen where

import           Example.Spec
import           Data.API.Aeson.Generate

$(generate        example)
$(generateTools 0 example)

$(generate        example2)
$(generateTools 0 example2)
