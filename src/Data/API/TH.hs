{-# LANGUAGE TemplateHaskell            #-}

module Data.API.TH
    ( app
    , mkInstanceIfNotExists
    , x_nm
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


-- | Add an instance declaration for the class @c@ applied to the
-- types @ts@, if such an instance does not already exist.
mkInstanceIfNotExists :: Name -> [Type] -> [Dec] -> Q [Dec]
mkInstanceIfNotExists c ts ds = do
    exists <- isInstance c ts
    if exists then return []
              else return [InstanceD [] (foldl AppT (ConT c) ts) ds]


x_nm :: Name
x_nm = mkName "x"
