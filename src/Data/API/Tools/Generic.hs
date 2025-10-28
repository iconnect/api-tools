{-# LANGUAGE TemplateHaskell #-}
module Data.API.Tools.Generic where

import           GHC.Generics

import           Data.API.TH
import           Data.API.Time ()
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Types

genericsTool :: APITool
genericsTool = apiNodeTool $ apiSpecTool gen_sn_to gen_sr_to gen_su_to gen_se_to mempty <> gen_pr

gen_sn_to :: Tool (APINode, SpecNewtype)
gen_sn_to = mkTool $ \ ts (an, _sn) -> optionalStandaloneDerivD ts ''Generic [nodeRepT an]

gen_sr_to :: Tool (APINode, SpecRecord)
gen_sr_to = mkTool $ \ ts (an, _sr) ->
    optionalStandaloneDerivD ts ''Generic [nodeRepT an]

gen_su_to :: Tool (APINode, SpecUnion)
gen_su_to = mkTool $ \ ts (an, _su) -> optionalStandaloneDerivD ts ''Generic [nodeRepT an]

gen_se_to :: Tool (APINode, SpecEnum)
gen_se_to = mkTool $ \ ts (an, _se) -> optionalStandaloneDerivD ts ''Generic [nodeRepT an]

gen_pr :: Tool APINode
gen_pr = mkTool $ \ ts an -> case anConvert an of
  Nothing                 -> return []
  Just (_inj_fn, _prj_fn) -> optionalStandaloneDerivD ts ''Generic [nodeT an]
