{-# LANGUAGE TemplateHaskell            #-}

module Data.API.TH
    ( app
    , applicativeE
    , mkInstanceIfNotExists
    , optionalInstanceD
    , x_nm
    , funSigD
    , simpleD
    , simpleSigD
    ) where

import           Control.Applicative
import           Language.Haskell.TH


-- | Construct an idiomatic expression (an expression in an
-- Applicative context), i.e.
--     app ke []             => ke
--     app ke [e1,e2,...,en] => ke <$> e1 <*> e2 ... <*> en
app :: Exp -> [Exp] -> Exp
app ke es0 =
    case es0 of
      []   -> ke
      e:es -> app' (ke `dl` e) es
  where
    app' e []      = e
    app' e (e':es) = app' (e `st` e') es

    st e1 e2 = AppE (AppE (VarE '(<*>)) e1) e2
    dl e1 e2 = AppE (AppE (VarE '(<$>)) e1) e2

-- | Construct an idiomatic expression (an expression in an
-- Applicative context), i.e.
--     app ke []             => ke
--     app ke [e1,e2,...,en] => ke <$> e1 <*> e2 ... <*> en
applicativeE :: ExpQ -> [ExpQ] -> ExpQ
applicativeE ke es0 =
    case es0 of
      []   -> ke
      e:es -> app' (ke `dl` e) es
  where
    app' e []      = e
    app' e (e':es) = app' (e `st` e') es

    st e1 e2 = appE (appE (varE '(<*>)) e1) e2
    dl e1 e2 = appE (appE (varE '(<$>)) e1) e2



-- | Add an instance declaration for the class @c@ applied to the
-- types @ts@, if such an instance does not already exist.
mkInstanceIfNotExists :: Name -> [Type] -> [Dec] -> Q [Dec]
mkInstanceIfNotExists c ts ds = do
    exists <- isInstance c ts
    if exists then return []
              else return [InstanceD [] (foldl AppT (ConT c) ts) ds]

-- | Add an instance declaration for a class, if such an instance does
-- not already exist
optionalInstanceD :: Name -> [TypeQ] -> [DecQ] -> Q [Dec]
optionalInstanceD c tqs dqs = do
    ts <- sequence tqs
    ds <- sequence dqs
    mkInstanceIfNotExists c ts ds


x_nm :: Name
x_nm = mkName "x"


-- | Construct a TH function with a type signature
funSigD :: Name -> TypeQ -> [ClauseQ] -> Q [Dec]
funSigD n t cs = (\ x y -> [x,y]) <$> sigD n t <*> funD n cs

-- | Construct a simple TH definition
simpleD :: Name -> ExpQ -> Q Dec
simpleD n e = funD n [clause [] (normalB e) []]

-- | Construct a simple TH definition with a type signature
simpleSigD :: Name -> TypeQ -> ExpQ -> Q [Dec]
simpleSigD n t e = funSigD n t [clause [] (normalB e) []]
