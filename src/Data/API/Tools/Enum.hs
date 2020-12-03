{-# LANGUAGE TemplateHaskell            #-}

-- | A tool to generate maps to and from 'Text' values corresponding
-- to inhabitants of enumerated types
module Data.API.Tools.Enum
    ( enumTool
    , text_enum_nm
    , map_enum_nm
    ) where

import           Data.API.TH
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Types

import qualified Data.Text                      as T
import qualified Data.Map                       as Map
import           Language.Haskell.TH


-- | Tool to generate the maps between enumerations and 'Text' strings
-- named by 'text_enum_nm' and 'map_enum_nm'.
enumTool :: APITool
enumTool = apiNodeTool $ apiSpecTool mempty mempty mempty enum mempty
  where
    enum = simpleTool (uncurry gen_se_tx) <> simpleTool (gen_se_mp . fst)


-- | For an enum type @E@, name a function @_text_E :: E -> 'Text'@
-- that gives a string corresponding to the inhabitant of the type.
-- For example, we generate something like this:
--
-- >   _text_FrameRate :: FrameRate -> T.Text
-- >   _text_FrameRate fr =
-- >           case fr of
-- >             FRauto    -> "auto"
-- >             FR10      -> "10"
-- >             FR15      -> "15"
-- >             FR23_97   -> "23.97"
-- >             FR24      -> "24"
-- >             FR25      -> "25"
-- >             FR29_97   -> "29.97"
-- >             FR30      -> "30"
-- >             FR60      -> "60"

text_enum_nm :: APINode -> Name
text_enum_nm an = mkName $ "_text_" ++ T.unpack (_TypeName $ anName an)

gen_se_tx :: APINode -> SpecEnum -> Q [Dec]
gen_se_tx as se = simpleSigD (text_enum_nm as)
                             [t| $tc -> T.Text |]
                             bdy
  where
    tc  = conT $ rep_type_nm as

    bdy  = lamCaseE [ match (pt fnm) (bd fnm) []
                    | (fnm,_) <- seAlts se ]

    pt fnm = nodeAltConP as fnm []

    bd fnm = normalB $ stringE $ T.unpack $ _FieldName fnm



-- | For an enum type @E@, name a map from 'Text' values to
-- inhabitants of the type, for example:
--
-- > _map_FrameRate :: Map Text FrameRate
-- > _map_FrameRate = genTextMap _text_FrameRate

map_enum_nm :: APINode -> Name
map_enum_nm  an = mkName $ "_map_"  ++ T.unpack (_TypeName $ anName an)

gen_se_mp :: APINode -> Q [Dec]
gen_se_mp as = simpleSigD (map_enum_nm as)
                          [t| Map.Map T.Text $tc |]
                          [e| genTextMap $(varE $ text_enum_nm as) |]
  where
    tc  = conT $ rep_type_nm as

genTextMap :: (Ord a,Bounded a,Enum a) => (a->T.Text) -> Map.Map T.Text a
genTextMap f = Map.fromList [ (f x,x) | x<-[minBound..maxBound] ]
