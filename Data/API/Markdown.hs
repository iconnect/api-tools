-- Generate API documentation for API (nodes)

module Data.API.Markdown
    ( markdown
    , thing
    , node
    , pp
    ) where

import           Data.API.Types
import qualified Data.CaseInsensitive       as CI
import           Data.List
import           Data.Char
import           Text.Printf
import           Control.Lens


{-      
 ###Foo

a test defn

JSON Type : **union object** (Haskell prefix is 'foo')

|Alternative | Type    | Comment
|----------- | ------- | -------
|_`Baz`_     | boolean | just a bool
|_`Qux`_     | integer | just an int
-}


markdown :: (TypeName->MDComment) -> API -> MDComment
markdown mkl ths = foldr (thing mkl) "" ths

thing :: (TypeName->MDComment) -> Thing -> MDComment  -> MDComment
thing mkl th tl_md =
    case th of
      ThComment md -> pp mkl md tl_md 
      ThNode    an -> node mkl an tl_md

node :: (TypeName->MDComment) -> APINode -> MDComment -> MDComment
node mkl an tl_md = 
        header mkl an $ body mkl an $ version an $ vlog an $ "\n\n" ++ tl_md 

header :: (TypeName->MDComment) -> APINode -> MDComment -> MDComment
header mkl as tl_md = printf "###%s\n\n%s\n\n%s" nm_md (pp mkl cm_md "") tl_md 
  where
    nm_md = type_name_md as
    cm_md = comment_md   as

body :: (TypeName->MDComment) -> APINode -> MDComment  -> MDComment
body mkl as tl_md =
    case anSpec as of
      SpNewtype sn -> block tl_md $ ntype   mkl as sn
      SpRecord  sr -> block tl_md $ record  mkl as sr
      SpUnion   su -> block tl_md $ union_  mkl as su
      SpEnum    se -> block tl_md $ enum_   mkl as se
      SpSynonym ty -> block tl_md $ synonym mkl as ty

ntype :: (TypeName->MDComment) -> APINode -> SpecNewtype -> [MDComment]
ntype _ as sn = summary_lines as (basic_type_md $ snType sn)

record :: (TypeName->MDComment) -> APINode -> SpecRecord -> [MDComment]
record mkl as sr =
    summary_lines as "record object" ++ mk_md_table mkl False (srFields sr)

union_ :: (TypeName->MDComment) -> APINode -> SpecUnion -> [MDComment]
union_ mkl as su =
    summary_lines as "union object" ++ mk_md_table mkl True (suFields su)

enum_ :: (TypeName->MDComment) -> APINode -> SpecEnum -> [MDComment]
enum_ _ as se = summary_lines as (printf "string (%s)" en_s)
  where
    en_s = concat $ intersperse "|" $ map _FieldName $ seAlts se

synonym :: (TypeName->MDComment) -> APINode -> APIType -> [MDComment]
synonym mkl an ty = summary_lines an $ type_md mkl ty

mk_md_table :: (TypeName->MDComment) -> Bool -> 
                            [(FieldName,(APIType,MDComment))] -> [MDComment]
mk_md_table mkl is_u fds = map f $ hdr : dhs : rws
  where
    f          = if all (null . view _3) rws then f2 else f3

    f2 (x,y,_) = ljust lnx x ++ " | " ++           y
    f3 (x,y,z) = ljust lnx x ++ " | " ++ ljust lny y ++ " | " ++ z

    dhs = (replicate lnx '-',replicate lny '-',replicate 7 '-')

    lnx = maximum $ map (length . view _1) $ hdr : rws
    lny = maximum $ map (length . view _2) $ hdr : rws

    hdr = (if is_u then "Alternative" else "Field","Type","Comment")
    
    rws = map fmt fds

    fmt (fn0,(ty,ct)) = (fn',type_md mkl ty,pp mkl "" $ cln ct)
      where
        fn' = if is_u then "_" ++ fn ++ "_" else fn

        fn  = _FieldName fn0

    cln ct = reverse $ dropWhile isSpace $ reverse $ map tr ct
      where
        tr '\n' = ' '
        tr c    = c

summary_lines :: APINode -> String -> [MDComment]
summary_lines an smy =
    [ printf "JSON Type : **%s** [Haskell prefix is `%s`]" smy $ prefix_md an
    , ""
    ]

type_md :: (TypeName->MDComment) -> APIType -> MDComment
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
block tl_md cmts = unlines cmts ++ tl_md

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

ljust :: Int -> String -> String
ljust fw s = s ++ replicate p ' '
  where
    p = max 0 $ fw - length s 

pp :: (TypeName->MDComment) -> MDComment -> MDComment -> MDComment
pp mkl s0 tl_md = pp0 s0
  where
    pp0 []    = tl_md
    pp0 (c:t) = 
        case c of
          '{' -> pp1 $ break ('}' ==) t
          _   -> c : pp0 t

    pp1 (nm,[] ) = '{' : nm ++ tl_md
    pp1 (nm,_:t) = mkl (TypeName nm) ++ pp0 t 
