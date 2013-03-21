module Data.API.Markdown
    ( markdown
    , markdown1
    ) where

import           Data.API.Types
import qualified Data.CaseInsensitive       as CI
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Data.List
import           Text.Printf


type URL = String


markdown :: (TypeName->URL) -> API -> MDComment
markdown mkl ass = foldr (markdown1 mkl) "" ass

markdown1 :: (TypeName->URL) -> APINode -> MDComment -> MDComment
markdown1 mkl as tl_md = header as $ body mkl as $ "\n\n" ++ tl_md 


header :: APINode -> MDComment -> MDComment
header as tl_md = printf "**%s**\n\n%s\n\n%s" nm_md cm_md tl_md 
  where
    nm_md = type_name_md as
    cm_md = comment_md   as


body :: (TypeName->URL) -> APINode -> MDComment  -> MDComment
body mkl as tl_md =
    case anSpec as of
      SpNewtype sn -> ntype  mkl as sn tl_md
      SpRecord  sr -> record mkl as sr tl_md
      SpUnion   su -> union_ mkl as su tl_md
      SpEnum    se -> enum   mkl as se tl_md

ntype :: (TypeName->URL) -> APINode -> SpecNewtype -> MDComment  -> MDComment
ntype _ as sn tl_md = summary_line as (basic_type_md $ snType sn) tl_md

record :: (TypeName->URL) -> APINode -> SpecRecord -> MDComment  -> MDComment
record mkl as sr tl_md =
    summary_line as "object (record)" $
        foldr fmt tl_md $ Map.toList $ srFields sr
  where
    fmt (fnm,(ty,cmt)) tl = 
                    printf "   %-15s : %-15s -- %s\n%s" fn_s ty_s cmt tl
      where
        fn_s = _FieldName fnm
        ty_s = type_md mkl ty

union_ :: (TypeName->URL) -> APINode -> SpecUnion -> MDComment  -> MDComment
union_ mkl as su tl_md =
    summary_line as "object (union)" $
                        foldr fmt tl_md $ Map.toList $ suFields su
  where
    fmt (fnm,(ty,cmt)) tl = 
                    printf " | %-15s : %-15s -- %s\n%s" fn_s ty_s cmt tl
      where
        fn_s = _FieldName fnm
        ty_s = type_md mkl ty

enum :: (TypeName->URL) -> APINode -> SpecEnum -> MDComment  -> MDComment
enum _ as se tl_md = summary_line as (printf "string (%s)" en_s) tl_md
  where
    en_s = concat $ intersperse "|" $ map _FieldName $ Set.toList $ seAlts se

summary_line :: APINode -> String -> MDComment  -> MDComment
summary_line as smy tl_md = printf "JSON Type : %s%s\n%s" smy pfx tl_md
  where
    pfx = case prefix_md as of
            "" -> ""
            pf -> "   [prefix '"++pf++"']"

type_md :: (TypeName->URL) -> APIType -> MDComment
type_md mkl ty =
    case ty of
      TyList  ty' -> "[" ++ type_md mkl ty' ++ "]"
      TyMaybe ty' -> "? " ++ type_md mkl ty'
      TyName  nm  -> mkl nm
      TyBasic bt  -> basic_type_md bt

basic_type_md :: BasicType -> MDComment
basic_type_md bt =
    case bt of
      BTstring -> "string" 
      BTbinary -> "binary" 
      BTbool   -> "boolean"
      BTint    -> "integral number"

type_name_md, prefix_md, comment_md :: APINode -> MDComment
type_name_md = _TypeName   . anName
prefix_md    = CI.original . anPrefix
comment_md   =               anComment
