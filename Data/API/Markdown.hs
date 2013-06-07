-- Generate API documentation for API (nodes)

module Data.API.Markdown
    ( markdown
    , MarkdownMethods(..)
    , defaultMarkdownMethods
    , thing
    , node
    ) where

import           Data.API.Types
import qualified Data.CaseInsensitive       as CI
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

data MarkdownMethods
    = MDM
        { mdmSummaryPostfix :: TypeName -> MDComment
        , mdmLink           :: TypeName -> MDComment
        , mdmPp             :: MDComment -> MDComment -> MDComment
        }

defaultMarkdownMethods :: MarkdownMethods
defaultMarkdownMethods =
    MDM { mdmSummaryPostfix = const ""
        , mdmLink           = _TypeName
		, mdmPp             = (++)
        }

markdown :: MarkdownMethods -> API -> MDComment
markdown mdm ths = foldr (thing mdm) "" ths

thing :: MarkdownMethods -> Thing -> MDComment  -> MDComment
thing mdm th tl_md =
    case th of
      ThComment md -> mdmPp mdm md tl_md 
      ThNode    an -> node  mdm an tl_md

node :: MarkdownMethods -> APINode -> MDComment -> MDComment
node mdm an tl_md = 
        header mdm an $ body mdm an $ version an $ vlog an $ "\n\n" ++ tl_md 

header :: MarkdownMethods -> APINode -> MDComment -> MDComment
header mdm an tl_md =
				printf "###%s\n\n%s\n\n%s" nm_md (mdmPp mdm cm_md "") tl_md 
  where
    nm_md = type_name_md an
    cm_md = comment_md   an

body :: MarkdownMethods -> APINode -> MDComment  -> MDComment
body mdm an tl_md =
    case anSpec an of
      SpNewtype sn -> block tl_md $ ntype   mdm an sn
      SpRecord  sr -> block tl_md $ record  mdm an sr
      SpUnion   su -> block tl_md $ union_  mdm an su
      SpEnum    se -> block tl_md $ enum_   mdm an se
      SpSynonym ty -> block tl_md $ synonym mdm an ty

ntype :: MarkdownMethods -> APINode -> SpecNewtype -> [MDComment]
ntype mdm an sn = summary_lines mdm an (basic_type_md $ snType sn)

record :: MarkdownMethods -> APINode -> SpecRecord -> [MDComment]
record mdm an sr =
    summary_lines mdm an "record object" ++ mk_md_table mdm False (srFields sr)

union_ :: MarkdownMethods -> APINode -> SpecUnion -> [MDComment]
union_ mdm an su =
    summary_lines mdm an "union object" ++ mk_md_table mdm True (suFields su)

enum_ :: MarkdownMethods -> APINode -> SpecEnum -> [MDComment]
enum_ mdm an en =
    summary_lines mdm an "string enumeration" ++ map f (hdr : zip fds cts)
  where
    f (fnm,cmt) = ljust lnx fnm ++ " | " ++ cmt

    lnx         = maximum $ 0 : map length fds

    fds         = map _FieldName fds_

    (fds_,cts)  = unzip   $ seAlts en

    hdr  = ("Enumeration","Comment")


synonym :: MarkdownMethods -> APINode -> APIType -> [MDComment]
synonym mdm an ty = summary_lines mdm an $ type_md mdm ty

mk_md_table :: MarkdownMethods -> Bool -> 
                            [(FieldName,(APIType,MDComment))] -> [MDComment]
mk_md_table mdm is_u fds = map f $ hdr : dhs : rws
  where
    f          = if all (null . view _3) rws then f2 else f3

    f2 (x,y,_) = ljust lnx x ++ " | " ++           y
    f3 (x,y,z) = ljust lnx x ++ " | " ++ ljust lny y ++ " | " ++ z

    dhs  = (replicate lnx '-',replicate lny '-',replicate 7 '-')

    lnx  = maximum $ map (length . view _1) $ hdr : rws
    lny  = maximum $ map (length . view _2) $ hdr : rws

    hdr  = (if is_u then "Alternative" else "Field","Type","Comment")
    
    rws  = map fmt fds

    fmt (fn0,(ty,ct)) = (fn',type_md mdm ty,mdmPp mdm "" $ cln ct)
      where
        fn' = if is_u then "_" ++ fn ++ "_" else fn

        fn  = _FieldName fn0

    cln ct = reverse $ dropWhile isSpace $ reverse $ map tr ct
      where
        tr '\n' = ' '
        tr c    = c

summary_lines :: MarkdownMethods -> APINode -> String -> [MDComment]
summary_lines mdm an smy =
    [ printf "JSON Type : **%s** [Haskell prefix is `%s`] %s" smy pfx pst
    , ""
    ]
  where
    pfx = prefix_md         an
    pst = mdmSummaryPostfix mdm $ anName an

type_md :: MarkdownMethods -> APIType -> MDComment
type_md mdm ty =
    case ty of
      TyList  ty' -> "[" ++ type_md mdm ty' ++ "]"
      TyMaybe ty' -> "? " ++ type_md mdm ty'
      TyName  nm  -> mdmLink mdm nm
      TyBasic bt  -> basic_type_md bt

basic_type_md :: BasicType -> MDComment
basic_type_md bt =
    case bt of
      BTstring -> "string" 
      BTbinary -> "base64 string" 
      BTbool   -> "boolean"
      BTint    -> "integer"
      BTutc    -> "utc"

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

{-
pp :: MarkdownMethods -> MDComment -> MDComment -> MDComment
pp mdm s0 tl_md = pp0 s0
  where
    pp0 []    = tl_md
    pp0 (c:t) = 
        case c of
          '{' -> pp1 $ break ('}' ==) t
          _   -> c : pp0 t

    pp1 (nm,[] ) = '{' : nm ++ tl_md
    pp1 (nm,_:t) = mdmLink mdm (TypeName nm) ++ pp0 t 
-}
