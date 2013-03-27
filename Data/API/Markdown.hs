-- Generate API documentation for API (nodes)

module Data.API.Markdown
    ( markdown
    , thing
    , node
    ) where

import           Data.API.Types
import qualified Data.CaseInsensitive       as CI
import           Data.List
import           Text.Printf


type URL = String


markdown :: (TypeName->URL) -> API -> MDComment
markdown mkl ths = foldr (thing mkl) "" ths

thing :: (TypeName->URL) -> Thing -> MDComment  -> MDComment
thing mkl th tl_md =
    case th of
      ThComment md -> md ++ tl_md 
      ThNode    an -> node mkl an tl_md

node :: (TypeName->URL) -> APINode -> MDComment -> MDComment
node mkl an tl_md = 
        header an $ body mkl an $ version an $ vlog an $ "\n\n" ++ tl_md 

header :: APINode -> MDComment -> MDComment
header as tl_md = printf "###%s\n\n%s\n\n%s" nm_md cm_md tl_md 
  where
    nm_md = type_name_md as
    cm_md = comment_md   as

body :: (TypeName->URL) -> APINode -> MDComment  -> MDComment
body mkl as tl_md =
    case anSpec as of
      SpNewtype sn -> block tl_md $ ntype   mkl as sn
      SpRecord  sr -> block tl_md $ record  mkl as sr
      SpUnion   su -> block tl_md $ union_  mkl as su
      SpEnum    se -> block tl_md $ enum    mkl as se
      SpSynonym ty -> block tl_md $ synonym mkl as ty

ntype :: (TypeName->URL) -> APINode -> SpecNewtype -> [MDComment]
ntype _ as sn = summary_lines as (basic_type_md $ snType sn)

record :: (TypeName->URL) -> APINode -> SpecRecord -> [MDComment]
record mkl as sr =
    summary_lines as "object (record)" ++ concat (map fmt (srFields sr))
  where
    fmt (fnm,(ty,cmt)) = field False mkl fnm ty cmt

union_ :: (TypeName->URL) -> APINode -> SpecUnion -> [MDComment]
union_ mkl as su =
    summary_lines as "object (union)" ++ concat (map fmt (suFields su))
  where
    fmt (fnm,(ty,cmt)) = field False mkl fnm ty cmt

enum :: (TypeName->URL) -> APINode -> SpecEnum -> [MDComment]
enum _ as se = summary_lines as (printf "string (%s)" en_s)
  where
    en_s = concat $ intersperse "|" $ map _FieldName $ seAlts se

synonym :: (TypeName->URL) -> APINode -> APIType -> [MDComment]
synonym mkl an ty = summary_lines an $ type_md mkl ty

field :: Bool -> (TypeName->URL) -> FieldName -> APIType -> MDComment -> [MDComment]
field isu mkl fnm ty cmt = printf "  %c %-20s : %-20s %s" c fn_s ty_s cm0 : cms
      where
        c         = if isu then '|' else ' '
        fn_s      = _FieldName fnm
        ty_s      = type_md mkl ty
        
        (cm0,cms) = comment 48 cmt

comment :: Int -> MDComment -> (MDComment,[MDComment])
comment col cmt = 
    case lines cmt of
      []       -> (""      ,[]                              )
      (cm:cms) -> (mk "" cm,map (mk $ replicate col ' ') cms)
  where
    mk pre cm = pre ++ "// " ++ cm

summary_lines :: APINode -> String -> [MDComment]
summary_lines an smy = prl ++ [printf "JSON Type : %s" smy]
  where
    prl = case prefix_md an of
            "" -> []
            pf -> [            printf "prefix    : %s" pf]

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
      BTbinary -> "base64 string" 
      BTbool   -> "boolean"
      BTint    -> "integer"

type_name_md, prefix_md, comment_md :: APINode -> MDComment
type_name_md = _TypeName   . anName
prefix_md    = CI.original . anPrefix
comment_md   =               anComment

block :: MDComment -> [MDComment] -> MDComment
block tl_md cmts = foldr lyo tl_md cmts
  where
    lyo cmt tl = "    " ++ cmt ++ '\n' : tl

version :: APINode -> MDComment -> MDComment 
version an tl_md =
    case anVersion an of
      Vrn n | n==1      -> tl_md
            | otherwise -> printf "version: %d\n%s" n tl_md

vlog :: APINode -> MDComment -> MDComment
vlog an tl_md =
    case anLog an of
      "" -> tl_md
      lg -> printf "\n###Log\n\n%s\n%s" lg tl_md
