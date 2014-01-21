{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Data.API.Tools
    ( generateAPITools
    , generate
    , generateInstances
    , generateTools
    , generateSamples
    , generateTests
    , generateMigrationKinds
    ) where

import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Tools.Enum
import           Data.API.Tools.Example
import           Data.API.Tools.JSON
import           Data.API.Tools.JSONTests
import           Data.API.Tools.Lens
import           Data.API.Tools.QuickCheck
import           Data.API.Tools.SafeCopy
import           Data.API.Types
import           Data.API.Changes
import           Control.Applicative
import qualified Data.Set                       as Set
import           Language.Haskell.TH


generateAPITools :: [APITool] -> API -> Q [Dec]
generateAPITools tools api' = concat <$> mapM ($ api') tools


generate :: API -> Q [Dec]
generate = generateAPITools [datatypesTool]

generateInstances :: API -> Q [Dec]
generateInstances = generateAPITools [enumTool, jsonTool, quickCheckTool]

generateTools :: API -> Q [Dec]
generateTools = generateAPITools [lensTool, safeCopyTool]

generateSamples :: API -> String -> Q [Dec]
generateSamples api' nm_s = generateAPITools [exampleTool nm_s] api'

generateTests :: API -> String -> Q [Dec]
generateTests api' s = generateAPITools [jsonTestsTool s] api'


generateMigrationKinds :: APIChangelog -> String -> String -> String -> Q [Dec]
generateMigrationKinds changes all_nm rec_nm fld_nm = do
    guardNoDups (all_tags `Set.intersection` rec_tags)
    guardNoDups (all_tags `Set.intersection` fld_tags)
    guardNoDups (rec_tags `Set.intersection` fld_tags)

    return [ DataD [] (mkName all_nm) [] (cons all_nm all_tags) derivs
           , DataD [] (mkName rec_nm) [] (cons rec_nm rec_tags) derivs
           , DataD [] (mkName fld_nm) [] (cons fld_nm fld_tags) derivs ]
  where
    (all_tags, rec_tags, fld_tags) = changelogTags changes

    guardNoDups xs
      | Set.null xs = return ()
      | otherwise   = fail $ "generateMigrationKinds: duplicate custom migrations in changelog: "
                             ++ show (Set.toList xs)

    -- List of constructors must not be empty, otherwise GHC can't
    -- derive Read/Show instances (see GHC Trac #7401)
    cons s xs | not (Set.null xs) = map (\ x -> NormalC (mkName x) []) (Set.toList xs)
              | otherwise         = [NormalC (mkName $ "No" ++ s) []]

    derivs = [''Read, ''Show]
