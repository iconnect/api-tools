{-# LANGUAGE RecordWildCards            #-}

-- | This module generates Markdown-formatted documentation for an
-- API, like this:
--
-- > ### Foo
-- >
-- > a test defn
-- >
-- > JSON Type : **union object** (Haskell prefix is 'foo')
-- >
-- > | Alternative | Type    | Comment
-- > | ----------- | ------- | -----------
-- > | _`Baz`_     | boolean | just a bool
-- > | _`Qux`_     | integer | just an int

module Data.API.Markdown
    ( markdown
    , MarkdownMethods(..)
    , defaultMarkdownMethods
    , thing
    ) where

import           Data.API.Time
import           Data.API.Types

import qualified Data.CaseInsensitive       as CI
import           Data.Char
import qualified Data.Text                  as T
import           Text.Printf
import           Control.Applicative
import           Control.Lens


data MarkdownMethods
    = MDM
        { mdmSummaryPostfix :: TypeName -> MDComment
        , mdmLink           :: TypeName -> MDComment
        , mdmPp             :: MDComment -> MDComment -> MDComment
        , mdmFieldDefault   :: FieldName -> APIType -> Maybe DefaultValue
        }

defaultMarkdownMethods :: MarkdownMethods
defaultMarkdownMethods =
    MDM { mdmSummaryPostfix = const ""
        , mdmLink           = T.unpack . _TypeName
        , mdmPp             = (++)
        , mdmFieldDefault   = \ _ _ -> Nothing
        }

-- | Create human-readable API documentation in Markdown format
markdown :: MarkdownMethods -> API -> MDComment
markdown mdm ths = foldr (thing mdm) "" ths

-- | Document a single API comment or node in Markdown format
thing :: MarkdownMethods -> Thing -> MDComment  -> MDComment
thing mdm th tl_md =
    case th of
      ThComment md -> mdmPp mdm md tl_md
      ThNode    an -> node  mdm an tl_md

node :: MarkdownMethods -> APINode -> MDComment -> MDComment
node mdm an tl_md =
        header mdm an $ body mdm an $ version an $ "\n\n" ++ tl_md

header :: MarkdownMethods -> APINode -> MDComment -> MDComment
header mdm an tl_md =
                                printf "### %s\n\n%s\n\n%s" nm_md (mdmPp mdm cm_md "") tl_md
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
ntype mdm an sn =
    summary_lines mdm an (basic_type_md $ snType sn) ++ [f ftr | Just ftr<-[snFilter sn]]
  where
    f (FtrStrg RegEx{..}   ) = "**filter** " ++ show re_text
    f (FtrIntg IntRange{..}) = "**filter** " ++ rg show   ir_lo ir_hi
    f (FtrUTC  UTCRange{..}) = "**filter** " ++ rg (T.unpack . printUTC) ur_lo ur_hi

    rg _  Nothing   Nothing   = "**no restriction**" -- should not happen (not produced by parser)
    rg sh Nothing   (Just hi) = "x <= " ++ sh hi
    rg sh (Just lo) Nothing   = sh lo ++ " <= x"
    rg sh (Just lo) (Just hi) = sh lo ++ " <= x <= " ++ sh hi

record :: MarkdownMethods -> APINode -> SpecRecord -> [MDComment]
record mdm an sr =
    summary_lines mdm an "record object" ++ mk_md_record_table mdm (srFields sr)

union_ :: MarkdownMethods -> APINode -> SpecUnion -> [MDComment]
union_ mdm an su =
    summary_lines mdm an "union object" ++ mk_md_union_table mdm (suFields su)

enum_ :: MarkdownMethods -> APINode -> SpecEnum -> [MDComment]
enum_ mdm an SpecEnum{..} =
    summary_lines mdm an "string enumeration" ++ map f (hdr : dhs : rws)
  where
    f (fnm,cmt)  = ljust lnx fnm ++ " | " ++ cmt

    dhs          = (replicate lnx '-',replicate 7 '-')

    lnx          = maximum $ 0 : map (T.length . _FieldName . fst) seAlts

    rws          = map fmt seAlts

    hdr          = ("Enumeration","Comment")

    fmt (fn0,ct) = (T.unpack (_FieldName fn0), mdmPp mdm "" $ cln ct)

    cln ct       = reverse $ dropWhile isSpace $ reverse $ map tr ct
      where
        tr '\n' = ' '
        tr c    = c

synonym :: MarkdownMethods -> APINode -> APIType -> [MDComment]
synonym mdm an ty = summary_lines mdm an $ type_md mdm ty

mk_md_record_table :: MarkdownMethods -> [(FieldName, FieldType)] -> [MDComment]
mk_md_record_table mdm fds = map f $ hdr : dhs : rws
  where
    f          = if all (null . view _4) rws then f3 else f4

    f3 (x,y,z,_) = ljust lnx x ++ " | " ++ ljust lny y ++ " | " ++ z
    f4 (x,y,z,a) = ljust lnx x ++ " | " ++ ljust lny y ++ " | " ++ ljust lnz z ++ " | " ++ a

    dhs  = (replicate lnx '-',replicate lny '-',replicate lnz '-',replicate 7 '-')

    lnx  = maximum $ map (length . view _1) $ hdr : rws
    lny  = maximum $ map (length . view _2) $ hdr : rws
    lnz  = maximum $ map (length . view _3) $ hdr : rws

    hdr  = ("Field","Type","Default","Comment")

    rws  = map fmt fds

    fmt (fn0,fty) = ( fn, type_md mdm ty, flg_md, mdmPp mdm "" $ cleanComment ct )
      where
        fn  = T.unpack (_FieldName fn0)
        ty  = ftType fty
        ct  = ftComment fty

        flg_md | ftReadOnly fty = "*read-only*"
               | otherwise      = default_md $ ftDefault fty

        default_md mb_dv = maybe "" (backticks . default_value)
                                 (mdmFieldDefault mdm fn0 ty <|> mb_dv)
        backticks s = "`" ++ s ++ "`"

mk_md_union_table :: MarkdownMethods ->
                            [(FieldName, (APIType, MDComment))] -> [MDComment]
mk_md_union_table mdm fds = map f $ hdr : dhs : rws
  where
    f          = if all (null . view _3) rws then f2 else f3

    f2 (x,y,_)  = ljust lnx x ++ " | " ++ y
    f3 (x,y,z)  = ljust lnx x ++ " | " ++ ljust lny y ++ " | " ++ z

    dhs  = (replicate lnx '-',replicate lny '-',replicate 7 '-')

    lnx  = maximum $ map (length . view _1) $ hdr : rws
    lny  = maximum $ map (length . view _2) $ hdr : rws

    hdr  = ("Alternative","Type","Comment")

    rws  = map fmt fds

    fmt (fn0,(ty,ct)) = ("_" ++ fn ++ "_",type_md mdm ty,mdmPp mdm "" $ cleanComment ct)
      where
        fn  = T.unpack (_FieldName fn0)

cleanComment :: MDComment -> MDComment
cleanComment ct = reverse $ dropWhile isSpace $ reverse $ map tr ct
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

default_value :: DefaultValue -> MDComment
default_value dv =
    case dv of
      DefValList     -> "[]"
      DefValMaybe    -> "null"
      DefValString t -> show t
      DefValBool   b -> map toLower $ show b
      DefValInt    i -> show i
      DefValUtc    u -> show $ printUTC u

type_md :: MarkdownMethods -> APIType -> MDComment
type_md mdm ty =
    case ty of
      TyList  ty'  -> "[" ++ type_md mdm ty' ++ "]"
      TyMaybe ty'  -> "? " ++ type_md mdm ty'
      TyName  nm   -> mdmLink mdm nm
      TyBasic bt   -> basic_type_md bt
      TyJSON       -> "json"

basic_type_md :: BasicType -> MDComment
basic_type_md bt =
    case bt of
      BTstring -> "string"
      BTbinary -> "base64 string"
      BTbool   -> "boolean"
      BTint    -> "integer"
      BTutc    -> "utc"

type_name_md, prefix_md, comment_md :: APINode -> MDComment
type_name_md = T.unpack . _TypeName . anName
prefix_md    = CI.original . anPrefix
comment_md   =               anComment

block :: MDComment -> [MDComment] -> MDComment
block tl_md cmts = unlines cmts ++ tl_md

version :: APINode -> MDComment -> MDComment
version _ tl_md = tl_md

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
