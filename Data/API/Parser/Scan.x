{
module Main (main) where

import           Safe
}

%wrapper "basic"

$digit = 0-9            -- digits
$lower = [a-z_]         -- lower case & _
$upper = [A-Z]          -- upper case letters

tokens :-
    $white+                         ;
    "--".*                          ;
    "|"                             { const Bar             }
    "::"                            { const ColCol          }
    ":"                             { const Colon           }
    "="                             { const Equals          }
    integer                         { const Integer         }
    boolean                         { const Boolean         }
    record                          { const Record          }
    string                          { const String          }
    union                           { const Union           }
    "//".*                          { line_comment          }
    "/*"(\n|[^\*]|\*[^\/])*"*/"     { block_comment         }
    $upper [$lower $upper $digit]*  { TypeIden              }
    $lower [$lower $upper $digit]*  { VarIden               }

{
-- Each action has type :: String -> Token

data Token
    = Bar
    | Boolean
    | ColCol
    | Colon
    | Equals
    | Integer
    | Record
    | String
    | Union
    | Comment  String
    | TypeIden String
    | VarIden  String
    deriving (Eq,Show)

line_comment :: String -> Token
line_comment = Comment . tailSafe . tailSafe 

block_comment :: String -> Token
block_comment (_:_:str) =
    case reverse str of
      _:_:rc -> Comment $ reverse rc
      _      -> error "line_comment"
block_comment _ = error "line_comment" 

main = do
  s <- getContents
  print (alexScanTokens s)
}
