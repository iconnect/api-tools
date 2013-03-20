module Data.API.Parse
    ( parseAPI
    , test_p
    ) where

import           Data.API.Types
import           Data.API.Scan
import qualified Data.Set                   as Set
import qualified Data.Map                   as Map
import           Text.Parsec
import           Text.Parsec.Pos
import qualified Data.CaseInsensitive       as CI
import           Control.Applicative((<$>))
import           Control.Monad

--import           Text.Parsec.String
--import           Data.Attempt

    
    

parseAPI :: String -> API
parseAPI inp =
    case parse api_p "" $ scan inp of
      Left  pe  -> error $ show pe
      Right api -> api  

test_p :: Parse a -> String -> a
test_p psr inp =
    case parse psr "" $ scan inp of
      Left  pe -> error $ show pe
      Right y  -> y  


{-
test :: IO () 
test =
 do cts <- readFile "test.txt"
    print $ parse_ cts
-}



type Parse a = Parsec [Token] () a

api_p :: Parse API
api_p = 
 do api <- many node_p
    eof
    return api

node_p :: Parse APINode
node_p =
     do kw_p Semi
        pre <- prefix_p
        kw_p ColCol
        con <- type_name_p
        cts <- comments_p
        kw_p Equals
        spc <- spec_p
        return $ APINode con cts pre spc

spec_p :: Parse Spec
spec_p =
    SpNewtype . SpecNewtype <$> basic_p 
        <|> SpRecord <$> record_p 
        <|> SpUnion  <$> union_p
        <|> SpEnum   <$> enum_p

record_p :: Parse SpecRecord
record_p =
 do kw_p Record
    SpecRecord <$> fields_p False

union_p :: Parse SpecUnion
union_p =
 do kw_p Union
    SpecUnion <$> fields_p True

enum_p :: Parse SpecEnum
enum_p = 
 do fn  <- field_name_p
    fns <- many $ kw_p Bar >> field_name_p
    return $ SpecEnum $ Set.fromList $ fn:fns

fields_p :: Bool -> Parse (Map.Map FieldName (APIType,MDComment))
fields_p is_u = fmap Map.fromList $ many $
     do when is_u $
            kw_p Bar
        fnm <- field_name_p
        kw_p Colon
        typ <- type_p
        cmt <- comments_p
        return $ (fnm,(typ,cmt))

type_p :: Parse APIType
type_p = list_p <|> TyName <$> type_name_p <|> TyBasic <$> basic_p

list_p :: Parse APIType
list_p = 
 do kw_p Bra
    typ <- type_p
    kw_p Ket
    return $ TyList typ

basic_p :: Parse BasicType
basic_p = 
    const BTstring <$> kw_p String        <|>
    const BTbool   <$> kw_p Boolean       <|>
    const BTint    <$> kw_p Integer

comments_p :: Parse MDComment
comments_p = unlines <$> many comment_p

comment_p :: Parse MDComment
comment_p = tok_p p
  where
    p (Comment cmt) = Just cmt
    p _             = Nothing 

prefix_p :: Parse Prefix
prefix_p = tok_p p
  where
    p (VarIden var) = is_prefix var
    p _             = Nothing

type_name_p :: Parse TypeName 
type_name_p = tok_p p
  where
    p (TypeIden tid) = Just $ TypeName tid
    p _              = Nothing

field_name_p :: Parse FieldName 
field_name_p = tok_p p
  where
    p (VarIden var) = Just $ FieldName var
    p _             = Nothing

kw_p :: Token -> Parse () 
kw_p tk = tok_p p
  where
    p tk' = case tk==tk' of
              True  -> Just ()
              False -> Nothing  

tok_p :: (Token->Maybe a) -> Parse a
tok_p = token pretty position

pretty :: Token -> String
pretty = show

position :: Token -> SourcePos
position = const pos

is_prefix :: String -> Maybe Prefix
is_prefix var = Just $ CI.mk var

pos :: SourcePos
pos = initialPos ""
