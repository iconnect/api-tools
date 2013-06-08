{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.API.Parse
    ( parseAPI
    ) where

import           Data.API.Types
import           Data.API.Scan
import           Data.Char
import           Data.String
import qualified Data.Text                  as T
import qualified Data.CaseInsensitive       as CI
import           Text.Printf
}

%name parse     API
%name parse_elt RUFields

%tokentype { PToken }


%token 
    ';'                                 { (,) _ Semi            }
    '|'                                 { (,) _ Bar             }
    '['                                 { (,) _ Bra             }
    ']'                                 { (,) _ Ket             }
    '::'                                { (,) _ ColCol          }
    ':'                                 { (,) _ Colon           }
    '='                                 { (,) _ Equals          }
    '?'                                 { (,) _ Query           }
    ','                                 { (,) _ Comma           }
    version                             { (,) _ Version         }
    with                                { (,) _ With            }    
    integer                             { (,) _ Integer         }
    boolean                             { (,) _ Boolean         }
    utc                                 { (,) _ UTC             }
    string                              { (,) _ String          }
    binary                              { (,) _ BInary          }
    record                              { (,) _ Record          }
    union                               { (,) _ Union           }
    enum                                { (,) _ Enum            }
    basic                               { (,) _ Basic           }
    comment                             { (,) _ (Comment  $$)   }
    typeiden                            { (,) _ (TypeIden $$)   }
    variden                             { (,) _ (VarIden  $$)   }
    intlit                              { (,) _ (Intg     $$)   }
    strlit                              { (,) _ (Strg     $$)   }
    true                                { (,) _ TRUE            }
    false                               { (,) _ FALSE           }
    utclit                              { (,) _ (UTCTIME  $$)   }


%%

API :: { API }
API : RAPI                              { reverse $1                        } 

RAPI :: { [Thing] }
RAPI 
    : RAPI ';' Thing                    { $3 : $1                           }
    |                                   { []                                }

Thing :: { Thing }
Thing
    : Node                              { ThNode    $1                      }
    | Comments                          { ThComment $1                      }

Node :: { APINode }
Node
    : Prefix '::' typeiden Comments '=' Spec With Vrn Comments 
                                { APINode (TypeName $3) $4 $1 $6 $7 $8 $9   }

Spec :: { Spec }
Spec
    : Record                            { SpRecord  $ $1                    }
    | Union                             { SpUnion   $ $1                    }
    | Enum                              { SpEnum    $ $1                    }
    | Basic                             { SpNewtype $ $1                    }
    | Type                              { SpSynonym $ $1                    }

With :: { Conversion }
With
    : with FieldName ',' FieldName      { Just ($2,$4)                      }
    |                                   { Nothing                           }

Vrn :: { Vrn }
Vrn : version intlit                    { Vrn $2                            } 
    |                                   { 0                                 }

Comments :: { MDComment }
Comments
    : RCommentList                      { unlines $ reverse $1              }

RCommentList :: { [MDComment] }
RCommentList
    : RCommentList comment              { $2 : $1                           }
    |                                   { []                                }

Prefix :: { Prefix }
Prefix
    : variden                           { CI.mk $1                          }

Record :: { SpecRecord }
Record
    : record RRFields                   { SpecRecord $ reverse $2           }
    
Union :: { SpecUnion }
Union
    : union  RUFields                   { SpecUnion  $ reverse $2           }

RRFields :: { [(FieldName,(APIType,MDComment))] }
RRFields
    : RRFields FieldName ':' Type Comments
                                        {  ($2,($4,$5)) : $1                 }
    |          FieldName ':' Type Comments       
                                        { [($1,($3,$4))]                     }

RUFields :: { [(FieldName,(APIType,MDComment))] }
RUFields
    : RUFields '|' FieldName ':' Type Comments
                                        {  ($3,($5,$6)) : $1                }
    |          '|' FieldName ':' Type Comments
                                        { [($2,($4,$5))]                    }

Enum :: { SpecEnum }
Enum
    : enum REnums                       { SpecEnum $ reverse $2             }

REnums :: { [(FieldName,MDComment)] }
REnums
    : REnums '|' FieldName Comments     { ($3,$4) : $1                      } 
    | FieldName            Comments     { [($1,$2)]                         }

Basic :: { SpecNewtype }
    : basic BasicType                   { SpecNewtype $2                    }

Type :: { APIType }
Type
    : '?' Type                          { TyMaybe            $2             }
    | '[' Type ']'                      { TyList             $2             }
    | typeiden                          { TyName  $ TypeName $1             }
    | BasicType                         { TyBasic            $1             }

BasicType :: { BasicType }
BasicType
    : string                            { BTstring  Nothing                 }
    | strlit                            { BTstring  (Just $ T.pack $1)      }
    | binary                            { BTbinary  Nothing                 }
    | boolean                           { BTbool    Nothing                 }
    | true                              { BTbool    (Just True )            }
    | false                             { BTbool    (Just False)            }
    | integer                           { BTint     Nothing                 }
    | intlit                            { BTint     (Just $1)               }
    | utc                               { BTutc     Nothing                 }
    | utclit                            { BTutc     (Just $1)               }

FieldName :: { FieldName }
FieldName
    : variden                           { FieldName $1                      }


{
happyError :: [PToken] -> a
happyError tks = error $ printf "Syntax error at %s: %s\n" loc $ show (take 5 tks)
  where
    loc = case tks of
            []                    -> "<EOF>"
            (AlexPn ad ln cn,_):_ -> printf "line %d, column %d (@%d)" ln cn ad

parseAPI :: String -> API
parseAPI = parse . scan

{-
sp_type :: APIType -> Spec
sp_type ty =
    case ty of
      TyBasic bty -> SpNewtype $ SpecNewtype bty
      TyList  _   -> SpSynonym ty
      TyMaybe _   -> SpSynonym ty
      TyName  _   -> SpSynonym ty
-}
}
