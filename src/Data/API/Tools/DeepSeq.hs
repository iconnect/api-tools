{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
module Data.API.Tools.DeepSeq
    ( deepSeqTool
    ) where

import           Data.API.TH
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Types

import           Control.DeepSeq
import           Data.Monoid
import           Language.Haskell.TH


-- | Tool to generate 'NFData' instances for generated types.
deepSeqTool :: APITool
deepSeqTool = apiNodeTool $ apiSpecTool gen_sn gen_sr gen_su gen_se mempty


gen_sn :: Tool (APINode, SpecNewtype)
gen_sn = mkTool $ \ ts (an, _) -> optionalInstanceD ts ''NFData [nodeRepT an]
                                     [simpleD 'rnf (bdy an)]
  where
    bdy an = [e| \ x -> rnf ($(newtypeProjectionE an) x) |]

gen_sr :: Tool (APINode, SpecRecord)
gen_sr = mkTool $ \ ts (an, sr) -> do
    x <- newName "x"
    optionalInstanceD ts ''NFData [nodeRepT an] [simpleD 'rnf (bdy an sr x)]
  where
    bdy an sr x = lamE [varP x] $ foldr f [e|()|] (srFields sr)
      where
        f (fn,_) r = [e| rnf ($(nodeFieldE an fn) $(varE x)) `seq` $r |]

gen_su :: Tool (APINode, SpecUnion)
gen_su = mkTool $ \ ts (an, su) -> do
    x <- newName "x"
    y <- newName "y"
    optionalInstanceD ts ''NFData [nodeRepT an] [simpleD 'rnf (bdy an su x y)]
  where
    bdy an su x y = lamE [varP x] $ caseE (varE x) cs
      where
        cs = map f (suFields su)
        f (fn,_) = match (nodeAltConP an fn [varP y]) (normalB [e|rnf $(varE y)|]) []

gen_se :: Tool (APINode, SpecEnum)
gen_se = mkTool $ \ ts (an, _) -> optionalInstanceD ts ''NFData [nodeRepT an] []
