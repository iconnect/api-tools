module Data.API.Doc.Subst
    ( Dict
    , subst
    , prep
    , mkDict
    , extDict
    )
    where

import qualified Data.Map               as Map
import           Text.Regex
import           Safe


type Dict = Map.Map String String


subst_re :: Regex
subst_re = mkRegexWithOpts "<<[a-zA-Z0-9_'-]+>>" True True

subst :: Dict -> String -> String
subst dct str =
    case matchRegexAll subst_re str of
      Nothing               -> str
      Just (pre,var_,pst,_) -> pre ++ rpl ++ subst dct pst
          where
            rpl = maybe ("<<"++var++">>") id $ Map.lookup var dct

            var = chp $ reverse $ chp $ reverse var_
            
            chp = tailNote "subst.chp" . tailNote "subst.chp"

prep :: Dict -> [String] -> String
prep dct = unlines . map (subst dct) 

mkDict :: [(String,String)] -> Dict
mkDict = Map.fromList

extDict :: [(String,String)] -> Dict -> Dict
extDict al dct = foldr (uncurry Map.insert) dct al