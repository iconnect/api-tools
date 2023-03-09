{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Data.API.Tools.Traversal
    ( traversalTool
    ) where

import           Data.API.NormalForm
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.TH
import           Data.API.Types

import           Control.Applicative
import qualified Data.Map                       as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                       as Set
import qualified Data.Text                      as T
import           Data.Traversable
import           Language.Haskell.TH
import           Prelude


-- | Build a traversal of the root type (first argument) that updates
-- values of the second type, e.g. @traversalTool "Root" "Sub"@
-- produces
--
-- > traverseSubRoot :: Applicative f => (Sub -> f Sub) -> Root -> f Root
--
-- along with similar functions for all the types nested inside @Root@
-- that depend on @Sub@.
--
-- Note that types with custom representations will not have
-- traversals generated automatically: if required, these must be
-- defined manually in the same module as the call to 'traversalTool',
-- otherwise the generated code will lead to scope errors.
traversalTool :: TypeName -> TypeName -> APITool
traversalTool root x = readTool (apiNodeTool . s)
  where
    s api = apiSpecTool mempty (simpleTool (uncurry $ traversalRecord napi targets x))
                               (simpleTool (uncurry $ traversalUnion  napi targets x)) mempty mempty
      where
        napi    = apiNormalForm api

        -- Calculate the types for which we must provide traversals:
        -- those that the root depends on and that depend on the
        -- traversed type
        targets = (transitiveDeps        napi rootSet `Set.union` rootSet) `Set.intersection`
                  (transitiveReverseDeps napi xSet    `Set.union` xSet)
        rootSet = Set.singleton root
        xSet    = Set.singleton x


-- | @traversalName x tn@ is the name of the function that traverses
-- @x@ values inside @tn@
traversalName :: TypeName -> TypeName -> Name
traversalName x tn = mkNameText $ "traverse" <> _TypeName x <> _TypeName tn

-- | @traversalType x an@ is the type of the function that traverses
-- @x@ values inside @an@
traversalType :: TypeName -> APINode -> TypeQ
traversalType x an = [t| forall f . Applicative f => ($x' -> f $x') -> $ty -> f $ty |]
  where
    x' = conT $ mkNameText $ _TypeName x
    ty = nodeT an


-- | Construct a traversal of the X substructures of the given type
traverser :: NormAPI -> Set.Set TypeName -> TypeName -> APIType -> ExpQ
traverser napi targets x ty = fromMaybe [| const pure |] $ traverser' napi targets x ty

-- | Construct a traversal of the X substructures of the given type,
-- or return 'Nothing' if there are no substructures to traverse
traverser' :: NormAPI -> Set.Set TypeName -> TypeName -> APIType -> Maybe ExpQ
traverser' napi targets x (TyList ty)  = fmap (appE [e|(.) traverse|]) $ traverser' napi targets x ty
traverser' napi targets x (TySet ty)   = fmap (appE [e|(.) traverse|]) $ traverser' napi targets x ty
traverser' napi targets x (TyMaybe ty) = fmap (appE [e|(.) traverse|]) $ traverser' napi targets x ty
traverser' napi targets x (TyName tn)
  | tn == x   = Just [e| id |]
  | not (tn `Set.member` targets) = Nothing
  | otherwise = case Map.lookup tn napi of
                           Nothing                -> error $ "missing API type declaration: " ++ T.unpack (_TypeName tn)
                           Just (NTypeSynonym ty) -> traverser' napi targets x ty
                           Just (NRecordType  _)  -> Just $ varE $ traversalName x tn
                           Just (NUnionType   _)  -> Just $ varE $ traversalName x tn
                           Just (NEnumType    _)  -> Nothing
                           Just (NNewtype     _)  -> Nothing
traverser' _ _ _ (TyBasic _)  = Nothing
traverser' _ _ _ TyJSON       = Nothing


-- | Build a traversal for a record type that applies f to any fields
-- of type X, and traverses nested structures.  For example:
--
-- > traverseXFoo :: Applicative f => (X -> f X) -> Foo -> f Foo
-- > traverseXFoo f x = Foo <$> f (foo_a x) <*> traverseXBar (traverse f) (foo_b x)
--
traversalRecord :: NormAPI -> Set.Set TypeName -> TypeName -> APINode -> SpecRecord -> Q [Dec]
traversalRecord napi targets x an sr
  | not (anName an `Set.member` targets) = return []
  | anConvert an /= Nothing              = return []
  | otherwise                            = simpleSigD nom (traversalType x an) bdy
  where
    nom = traversalName x (anName an)
    bdy = do
      f <- newName "f"
      r <- newName "r"
      lamE [varP f, varP r] $ applicativeE (nodeConE an) $ map (traverseField f r) (srFields sr)
    traverseField f r (fn, fty) = [e| $(traverser napi targets x (ftType fty)) $(varE f) ($(nodeFieldE an fn) $(varE r)) |]


-- | Build a traversal for a union type that traverses nested structures.
-- For example:
--
-- > traverseXBar :: Applicative f => (X -> f X) -> Bar -> f Bar
-- > traverseXBar f (BAR_one a) = BAR_one <$> f a
-- > traverseXBar f (Bar_two b) = BAR_two <$> traverseXBaz f b
--
traversalUnion :: NormAPI -> Set.Set TypeName -> TypeName -> APINode -> SpecUnion -> Q [Dec]
traversalUnion napi targets x an su
  | not (anName an `Set.member` targets) = return []
  | anConvert an /= Nothing              = return []
  | otherwise                            = funSigD nom (traversalType x an) cls
  where
    nom = traversalName x (anName an)
    cls = map cl $ suFields su
    cl (fn,(ty,_)) = do
      f <- newName "f"
      z <- newName "z"
      clause [varP f, nodeAltConP an fn [varP z]] (normalB (bdy fn ty f z)) []
    bdy fn ty f z = [e| $(nodeAltConE an fn) <$> $(traverser napi targets x ty) $(varE f) $(varE z) |]
