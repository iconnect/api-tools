{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}


module Data.API.Parse
    ( parseAPI
    , parseAPIWithChangelog
    , api
    , apiWithChangelog
    ) where

import           Data.API.Types
import           Data.API.Scan
import           Data.API.Changes
import           Data.Char
import           Data.String
import qualified Data.Text                  as T
import qualified Data.CaseInsensitive       as CI
import qualified Data.Version               as V
import           Distribution.Text (simpleParse)
import           Language.Haskell.TH.Quote
import           Text.Printf
import           Text.Regex
}

%name parse                API
%name parse_with_changelog APIWithChangelog

%tokentype { PToken }


%token
    ';'                                 { (,) _ Semi            }
    '|'                                 { (,) _ Bar             }
    '['                                 { (,) _ Bra             }
    ']'                                 { (,) _ Ket             }
    '::'                                { (,) _ ColCol          }
    '='                                 { (,) _ Equals          }
    '?'                                 { (,) _ Query           }
    ','                                 { (,) _ Comma           }
    '<='                                { (,) _ LtEq            }
    '>='                                { (,) _ GtEq            }
    version                             { (,) _ Version         }
    with                                { (,) _ With            }
    integer                             { (,) _ Integer         }
    boolean                             { (,) _ Boolean         }
    utc                                 { (,) _ UTC             }
    string                              { (,) _ String          }
    binary                              { (,) _ BInary          }
    json                                { (,) _ Json            }
    record                              { (,) _ Record          }
    union                               { (,) _ Union           }
    enum                                { (,) _ Enum            }
    basic                               { (,) _ Basic           }
    changes                             { (,) _ Changes         }
    added                               { (,) _ Added           }
    removed                             { (,) _ Removed         }
    renamed                             { (,) _ Renamed         }
    changed                             { (,) _ Changed         }
    default                             { (,) _ Default         }
    field                               { (,) _ Field           }
    alternative                         { (,) _ Alternative     }
    migration                           { (,) _ Migration       }
    to                                  { (,) _ To              }
    nothing                             { (,) _ NOTHING         }
    readonly                            { (,) _ Readonly        }
    comment                             { (,) _ (Comment  $$)   }
    typeiden                            { (,) _ (TypeIden $$)   }
    variden                             { (,) _ (VarIden  $$)   }
    intlit                              { (,) _ (Intg     $$)   }
    strlit                              { (,) _ (Strg     $$)   }
    true                                { (,) _ TRUE            }
    false                               { (,) _ FALSE           }
    utclit                              { (,) _ (UTCTIME  $$)   }


%%

APIWithChangelog :: { APIWithChangelog }
APIWithChangelog
    : API changes APIChangelog          { ($1, $3)                          }

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
    : Prefix '::' typeiden Comments '=' Spec With
                                        { APINode (TypeName $3) $4 $1 $6 $7 }

Spec :: { Spec }
Spec
    : Record                            { SpRecord  $1                      }
    | Union                             { SpUnion   $1                      }
    | Enum                              { SpEnum    $1                      }
    | Basic                             { SpNewtype $1                      }
    | Type                              { SpSynonym $1                      }

With :: { Conversion }
With
    : with FieldName ',' FieldName      { Just ($2,$4)                      }
    |                                   { Nothing                           }

Comments :: { MDComment }
Comments
    : RCommentList                      { unlines $ reverse $1              }

RCommentList :: { [MDComment] }
RCommentList
    : RCommentList comment              { $2 : $1                           }
    |                                   { []                                }

Prefix :: { Prefix }
Prefix
    : VarIdentifier                     { CI.mk $1                          }

Record :: { SpecRecord }
Record
    : record RRFields                   { SpecRecord $ reverse $2           }

Union :: { SpecUnion }
Union
    : union  RUFields                   { SpecUnion  $ reverse $2           }

RRFields :: { [(FieldName, FieldType)] }
RRFields
    : RRFields FieldName '::' FieldType {  ($2,$4) : $1                     }
    |          FieldName '::' FieldType { [($1,$3)]                         }

FieldType :: { FieldType }
FieldType
    : Type IsReadOnly MayBasicLit Comments
                                        { FieldType $1 $2 $3 $4             }

IsReadOnly :: { Bool }
    : readonly                          { True                              }
    |                                   { False                             }

RUFields :: { [(FieldName,(APIType,MDComment))] }
RUFields
    : RUFields '|' FieldName '::' Type Comments
                                        {  ($3,($5,$6)) : $1                }
    |          '|' FieldName '::' Type Comments
                                        { [($2,($4,$5))]                    }

Enum :: { SpecEnum }
Enum
    : enum REnums                       { SpecEnum $ reverse $2             }

REnums :: { [(FieldName,MDComment)] }
REnums
    : REnums '|' FieldName Comments     { ($3,$4) : $1                      }
    |        '|' FieldName Comments     { [($2,$3)]                         }

Basic :: { SpecNewtype }
    : basic BasicType MbFilter          { SpecNewtype $2 $3                 }

MbFilter :: { Maybe Filter }
    : '|' Filter                        { Just $2                           }
    |                                   { Nothing                           }

Filter :: { Filter }
    : RegEx                        { FtrStrg $1                             }
    | '>=' intlit                  { FtrIntg $ IntRange (Just $2) Nothing   }
    | '>=' intlit ',' '<=' intlit  { FtrIntg $ IntRange (Just $2) (Just $5) }
    | '<=' intlit                  { FtrIntg $ IntRange Nothing   (Just $2) }
    | '>=' utclit                  { FtrUTC  $ UTCRange (Just $2) Nothing   }
    | '>=' utclit ',' '<=' utclit  { FtrUTC  $ UTCRange (Just $2) (Just $5) }
    | '<=' utclit                  { FtrUTC  $ UTCRange Nothing   (Just $2) }

RegEx :: { RegEx }
    : strlit                            { RegEx (T.pack $1) $ mkRegexWithOpts $1 False True }

Type :: { APIType }
Type
    : '?' Type                          { TyMaybe            $2             }
    | '[' Type ']'                      { TyList             $2             }
    | typeiden                          { TyName   (TypeName $1)            }
    | BasicType                         { TyBasic            $1             }
    | json                              { TyJSON                            }

MayBasicLit :: { Maybe DefaultValue }
    : strlit                            { Just $ DefValString (T.pack $1)   }
    | true                              { Just $ DefValBool   (True     )   }
    | false                             { Just $ DefValBool   (False    )   }
    | intlit                            { Just $ DefValInt    $1            }
    | utclit                            { Just $ DefValUtc    $1            }
    |                                   { Nothing                           }

BasicType :: { BasicType }
BasicType
    : string                            { BTstring                          }
    | binary                            { BTbinary                          }
    | boolean                           { BTbool                            }
    | integer                           { BTint                             }
    | utc                               { BTutc                             }

FieldName :: { FieldName }
FieldName
    : VarIdentifier                     { FieldName $1                      }

VarIdentifier :: { String }
    : variden                           { $1                                }
    | default                           { "default"                         }
    | field                             { "field"                           }
    | alternative                       { "alternative"                     }
    | to                                { "to"                              }

TypeName :: { TypeName }
    : typeiden                          { TypeName $1                       }

APIChangelog :: { APIChangelog }
APIChangelog
    : version Version Changes ';' APIChangelog { ChangesUpTo $2 $3 $5       }
    | version Version                          { ChangesStart $2            }
    | Comments ';' APIChangelog                 { $3                         }

Version :: { V.Version }
    : strlit                            { parseVer $1                       }

Changes :: { [APIChange] }
    : RChanges                          { concat (reverse $1)               }

RChanges :: { [[APIChange]] }
    : RChanges Change                   { $2 : $1                           }
    |                                   { []                                }

Change :: { [APIChange] }
    : added TypeName Spec               { [ChAddType $2 (declNF $3)]        }
    | removed TypeName                  { [ChDeleteType $2]                 }
    | renamed TypeName to TypeName      { [ChRenameType $2 $4]              }
    | changed record TypeName RFieldChanges { map (fldChangeToAPIChange $3)   (reverse $4) }
    | changed union  TypeName RUnionChanges { map (unionChangeToAPIChange $3) (reverse $4) }
    | changed enum   TypeName REnumChanges  { map (enumChangeToAPIChange $3)  (reverse $4) }
    | migration record TypeName MigrationTag { [ChCustomRecord $3 $4]       }
    | migration MigrationTag             { [ChCustomAll $2]                 }
    | comment                            { []                               }

RFieldChanges :: { [FieldChange] }
    : RFieldChanges FieldChange         { $2 ++ $1                           }
    | FieldChange                       { $1                                 }

FieldChange :: { [FieldChange] }
    : field added FieldName '::' Type MbDefaultValue           { [FldChAdd $3 $5 $6]    }
    | field removed FieldName                                  { [FldChDelete $3]       }
    | field renamed FieldName to FieldName                     { [FldChRename $3 $5]    }
    | field changed FieldName '::' Type migration MigrationTag { [FldChChange $3 $5 $7] }
    | comment                                                  { []                     }

MbDefaultValue :: { Maybe DefaultValue }
    : default DefaultValue             { Just $2                            }
    |                                  { Nothing                            }

DefaultValue :: { DefaultValue }
    : '[' ']'                          { DefValList                         }
    | nothing                          { DefValMaybe                        }
    | strlit                           { DefValString (T.pack $1)           }
    | true                             { DefValBool True                    }
    | false                            { DefValBool False                   }
    | intlit                           { DefValInt $1                       }
    | utclit                           { DefValUtc $1                       }

RUnionChanges :: { [UnionChange] }
    : RUnionChanges UnionChange        { $2 ++ $1                           }
    | UnionChange                      { $1                                 }

UnionChange :: { [UnionChange] }
    : alternative added FieldName '::' Type      { [UnChAdd $3 $5]          }
    | alternative removed FieldName              { [UnChDelete $3]          }
    | alternative renamed FieldName to FieldName { [UnChRename $3 $5]       }
    | comment                                    { []                       }

REnumChanges :: { [EnumChange] }
    : REnumChanges EnumChange          { $2 ++ $1                           }
    | EnumChange                       { $1                                 }

EnumChange :: { [EnumChange] }
    : alternative added FieldName                { [EnChAdd $3]             }
    | alternative removed FieldName              { [EnChDelete $3]          }
    | alternative renamed FieldName to FieldName { [EnChRename $3 $5]       }
    | comment                                    { []                       }

MigrationTag :: { MigrationTag }
    : typeiden                         { $1                                 }

{
happyError :: [PToken] -> a
happyError tks = error $ printf "Syntax error at %s: %s\n" loc $ show (take 5 tks)
  where
    loc = case tks of
            []                    -> "<EOF>"
            (AlexPn ad ln cn,_):_ -> printf "line %d, column %d (@%d)" ln cn ad

parseAPI :: String -> API
parseAPI = parse . scan

parseAPIWithChangelog :: String -> APIWithChangelog
parseAPIWithChangelog = parse_with_changelog . scan


data FieldChange = FldChAdd FieldName APIType (Maybe DefaultValue)
                 | FldChDelete FieldName
                 | FldChRename FieldName FieldName
                 | FldChChange FieldName APIType MigrationTag

fldChangeToAPIChange :: TypeName -> FieldChange -> APIChange
fldChangeToAPIChange t (FldChAdd f ty def)  = ChAddField t f ty def
fldChangeToAPIChange t (FldChDelete f)      = ChDeleteField t f
fldChangeToAPIChange t (FldChRename f f')   = ChRenameField t f f'
fldChangeToAPIChange t (FldChChange f ty m) = ChChangeField t f ty m

data UnionChange = UnChAdd FieldName APIType
                 | UnChDelete FieldName
                 | UnChRename FieldName FieldName

unionChangeToAPIChange :: TypeName -> UnionChange -> APIChange
unionChangeToAPIChange t (UnChAdd f ty)    = ChAddUnionAlt t f ty
unionChangeToAPIChange t (UnChDelete f)    = ChDeleteUnionAlt t f
unionChangeToAPIChange t (UnChRename f f') = ChRenameUnionAlt t f f'

data EnumChange = EnChAdd FieldName
                | EnChDelete FieldName
                | EnChRename FieldName FieldName

enumChangeToAPIChange :: TypeName -> EnumChange -> APIChange
enumChangeToAPIChange t (EnChAdd f)       = ChAddEnumVal t f
enumChangeToAPIChange t (EnChDelete f)    = ChDeleteEnumVal t f
enumChangeToAPIChange t (EnChRename f f') = ChRenameEnumVal t f f'


parseVer :: String -> V.Version
parseVer x = case simpleParse x of
                 Just v -> v
                 Nothing -> error $ "Syntax error while parsing version " ++ x

api :: QuasiQuoter
api =
    QuasiQuoter
        { quoteExp  = \s -> [| parseAPI s |]
        , quotePat  = error "api QuasiQuoter used in patten      context"
        , quoteType = error "api QuasiQuoter used in type        context"
        , quoteDec  = error "api QuasiQuoter used in declaration context"
        }

apiWithChangelog :: QuasiQuoter
apiWithChangelog =
    QuasiQuoter
        { quoteExp  = \s -> [| parseAPIWithChangelog s |]
        , quotePat  = error "apiWithChangelog QuasiQuoter used in patten      context"
        , quoteType = error "apiWithChangelog QuasiQuoter used in type        context"
        , quoteDec  = error "apiWithChangelog QuasiQuoter used in declaration context"
        }
}
