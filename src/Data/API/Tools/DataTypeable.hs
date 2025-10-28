{-# LANGUAGE TemplateHaskell #-}
module Data.API.Tools.DataTypeable (
  dataTypeableTool
  ) where

import Prelude

import           Data.API.TH
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Types
import           Data.Data

dataTypeableTool :: APITool
dataTypeableTool = apiNodeTool $
             apiSpecTool gen_sn_to gen_sr_to gen_su_to gen_se_to mempty
             <> gen_pr

gen_sn_to :: Tool (APINode, SpecNewtype)
gen_sn_to = mkTool $ \ ts (an, _sn) -> optionalStandaloneDerivD ts ''Data [nodeRepT an]

gen_sr_to :: Tool (APINode, SpecRecord)
gen_sr_to = mkTool $ \ ts (an, _sr) ->
    optionalStandaloneDerivD ts ''Data [nodeRepT an]

gen_su_to :: Tool (APINode, SpecUnion)
gen_su_to = mkTool $ \ ts (an, _su) -> optionalStandaloneDerivD ts ''Data [nodeRepT an]

gen_se_to :: Tool (APINode, SpecEnum)
gen_se_to = mkTool $ \ ts (an, _se) -> optionalStandaloneDerivD ts ''Data [nodeRepT an]

gen_pr :: Tool APINode
gen_pr = mkTool $ \ ts an -> case anConvert an of
  Nothing                 -> return []
  Just (_inj_fn, _prj_fn) -> optionalStandaloneDerivD ts ''Data [nodeT an]
