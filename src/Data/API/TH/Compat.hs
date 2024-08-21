{-# LANGUAGE CPP #-}
module Data.API.TH.Compat where

import Language.Haskell.TH

#if MIN_VERSION_template_haskell(2,21,0)
type TyVarBndr' = TyVarBndr BndrVis
#elif MIN_VERSION_template_haskell(2,17,0)
type TyVarBndr' = TyVarBndr ()
#else
type TyVarBndr' = TyVarBndr
#endif

mkDataD :: Cxt -> Name -> [TyVarBndr'] -> [Con] -> [Name] -> Dec
mkDataD cxt1 name tyVarBndrs cons drvs =
#if MIN_VERSION_template_haskell(2,12,0)
  DataD cxt1 name tyVarBndrs Nothing cons [DerivClause Nothing (map ConT drvs)]
#elif MIN_VERSION_template_haskell(2,11,0)
  DataD cxt1 name tyVarBndrs Nothing cons (map ConT drvs)
#else
  DataD cxt1 name tyVarBndrs cons drvs
#endif

mkInstanceD :: Cxt -> Type -> [Dec] -> Dec
mkInstanceD =
#if MIN_VERSION_template_haskell(2,11,0)
  InstanceD Nothing
#else
  InstanceD
#endif

mkNewtypeD :: Cxt -> Name -> [TyVarBndr'] -> Con -> [Name] -> Dec
mkNewtypeD cxt1 name tyVarBndrs cons drvs =
#if MIN_VERSION_template_haskell(2,12,0)
  NewtypeD cxt1 name tyVarBndrs Nothing cons [DerivClause Nothing (map ConT drvs)]
#elif MIN_VERSION_template_haskell(2,11,0)
  NewtypeD cxt1 name tyVarBndrs Nothing cons (map ConT drvs)
#else
  NewtypeD cxt1 name tyVarBndrs cons drvs
#endif

----------------------------------------

type Strictness =
#if MIN_VERSION_template_haskell(2,11,0)
  Bang
#else
  Strict
#endif

annIsStrict :: Strictness
annIsStrict =
#if MIN_VERSION_template_haskell(2,11,0)
  Bang NoSourceUnpackedness SourceStrict
#else
  IsStrict
#endif

annNotStrict :: Strictness
annNotStrict =
#if MIN_VERSION_template_haskell(2,11,0)
  Bang NoSourceUnpackedness NoSourceStrictness
#else
  NotStrict
#endif
