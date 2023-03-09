{
{-# OPTIONS_GHC -w #-}

module Data.API.Scan
    ( scan
    , PToken
    , AlexPosn(..)
    , Token(..)
    , keywords
    ) where

import           Data.API.Time
import           Data.API.Types
import           Data.API.Utils

import           Data.Char
import qualified Data.Text                      as T
import           Data.Time
import           Safe
}

%wrapper "posn"

$digit = 0-9            -- digits
$lower = [a-z_]         -- lower case & _
$upper = [A-Z]          -- upper case letters

@d2    = $digit{2}
@d4    = $digit{4}


tokens :-
    $white+                             ;
    "--".*                              ;
    "{-"(\n|[^\-]|\-[^\}])*"-}"         ;
    ";"                                 { simple    Semi            }
    "|"                                 { simple    Bar             }
    "["                                 { simple    Bra             }
    "]"                                 { simple    Ket             }
    "{"                                 { simple    Cu              }
    "}"                                 { simple    Rly             }
    "::"                                { simple    ColCol          }
    ":"                                 { simple    Colon           }
    "="                                 { simple    Equals          }
    "<="                                { simple    LtEq            }
    ">="                                { simple    GtEq            }
    "?"                                 { simple    Query           }
    ","                                 { simple    Comma           }
    version                             { simple    Version         } -- N.B. extend the 'keywords list below
    with                                { simple    With            } -- when adding new keywords!
    integer                             { simple    Integer         }
    boolean                             { simple    Boolean         }
    utc                                 { simple    UTC             }
    string                              { simple    String          }
    binary                              { simple    BInary          }
    json                                { simple    Json            }
    record                              { simple    Record          }
    union                               { simple    Union           }
    enum                                { simple    Enum            }
    basic                               { simple    Basic           }
    changes                             { simple    Changes         }
    added                               { simple    Added           }
    removed                             { simple    Removed         }
    renamed                             { simple    Renamed         }
    changed                             { simple    Changed         }
    default                             { simple    Default         }
    field                               { simple    Field           }
    alternative                         { simple    Alternative     }
    migration                           { simple    Migration       }
    to                                  { simple    To              }
    nothing                             { simple    NOTHING         }
    true                                { simple    TRUE            }
    false                               { simple    FALSE           }
    read\-only                          { simple    Readonly        }
    @d4\-@d2\-(@d2)T@d2\:@d2(:@d2)?Z    { utc_                      }
      $upper [$lower $upper $digit]*    { mk        TypeIden        }
    \'$upper [$lower $upper $digit]*\'  { strip_qs  TypeIden        }
      $lower [$lower $upper $digit]*    { mk        VarIden         }
    \'$lower [$lower $upper $digit]*\'  { strip_qs  VarIden         }
    \"([^\\\"]|\\[\\\'\"])*\"           { string                    }
    \-?$digit+                          { intg                      }
    "//".*                              { line_comment              }
    "(*"(\n|[^\*]|\*[^\)])*"*)"         { block_comment             }

{

keywords :: [String]
keywords = [ "version", "with", "integer", "boolean", "utc", "string"
           , "binary", "json", "record", "union", "enum", "basic", "changes"
           , "added", "removed", "renamed", "changed", "default", "field"
           , "alternative", "migration", "to", "nothing", "true", "false"
           , "read-only"
           ]

type PToken = (AlexPosn,Token)

data Token
    = Semi
    | Bar
    | BInary
    | Bra
    | Ket
    | Cu
    | Rly
    | ColCol
    | Colon
    | Comma
    | Equals
    | LtEq
    | GtEq
    | Boolean
    | Integer
    | UTC
    | Query
    | Record
    | String
    | Json
    | Union
    | Version
    | With
    | Enum
    | Basic
    | Changes
    | Added
    | Removed
    | Renamed
    | Changed
    | Default
    | Field
    | Alternative
    | Migration
    | To
    | NOTHING
    | TRUE
    | FALSE
    | Readonly
    | UTCTIME  UTCTime
    | Comment  String
    | TypeIden String
    | VarIden  String
    | Intg     Int
    | Strg     String
    | ERROR
    deriving (Eq,Show)

utc_ :: AlexPosn -> String -> PToken
utc_ = mk $ \s -> maybe ERROR UTCTIME $ parseUTC (T.pack s)

line_comment :: AlexPosn -> String -> PToken
line_comment = mk $ Comment . munch_ws . tailSafe . tailSafe 

block_comment :: AlexPosn -> String -> PToken
block_comment p (_:_:str) = 
    case reverse $ munch_ws str of
      _:_:rc -> (p,Comment $ reverse $ munch_ws rc)
      _      -> error "Scan.line_comment"
block_comment _ _ = error "Scan.line_comment" 

strip_qs :: (String->Token) -> AlexPosn -> String -> PToken
strip_qs f p (_:s) = (p,f $ initNote "Scan.strip_qs" s)
strip_qs _ _ _     = error "Scan.strip_qs"

munch_ws :: String -> String
munch_ws = dropWhile isSpace

simple :: Token -> AlexPosn -> String -> PToken
simple tk = mk $ const tk

intg :: AlexPosn -> String -> PToken
intg p s = (p,Intg $ readNote "Data.API.Scan.intg" s)

string :: AlexPosn -> String -> PToken
string = mk (Strg . f . chop)
  where
    f ""    = ""
    f (c:s) = case c of
                '\\' -> g s
                _    -> c : f s

    g ""    = ""
    g (c:s) = c : f s

chop :: String -> String
chop ""    = ""
chop (c:s) =
    case reverse s of
      ""   -> ""
      _:rs -> reverse rs

mk :: (String->Token) -> AlexPosn -> String -> PToken
mk f p s = (p,f s)

scan :: String -> [PToken]
scan = pp . alexScanTokens

pp :: [PToken] -> [PToken]
pp [] = []
pp (pt@(p@(AlexPn _ _ cn),_):inp) =
    case cn of
      1 -> (p,Semi):pt:pp inp
      _ ->          pt:pp inp

test :: IO ()
test = 
 do s <- getContents
    print (scan s)
}
