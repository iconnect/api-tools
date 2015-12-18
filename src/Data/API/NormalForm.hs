-- | This module defines a normalised representation of APIs, used for
-- comparing them in the migrations changelog, and to analyse dependencies.
module Data.API.NormalForm
    ( -- * Normalised API types
      NormAPI
    , NormTypeDecl(..)
    , NormRecordType
    , NormUnionType
    , NormEnumType

      -- * Converting to normal form
    , apiNormalForm
    , declNF

      -- * Dependency analysis
    , typeDeclsFreeVars
    , typeDeclFreeVars
    , typeFreeVars
    , typeDeclaredInApi
    , typeUsedInApi
    , typeUsedInTransitiveDep
    , transitiveDeps
    , transitiveReverseDeps

      -- * Invariant validation
    , apiInvariant
    , declIsValid
    , typeIsValid

      -- * Modifying types
    , substTypeDecl
    , substType
    , renameTypeUses
    ) where

import           Data.API.PP
import           Data.API.Types

import           Control.DeepSeq
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set


-- | The API type has too much extra info for us to be able to simply compare
-- them with @(==)@. Our strategy is to strip out ancillary information and
-- normalise into a canonical form, and then we can use a simple @(==)@ compare.
--
-- Our normalised API discards most of the details of each type, keeping
-- just essential information about each type. We discard order of types and
-- fields, so we can use just associative maps.
--
type NormAPI = Map TypeName NormTypeDecl

-- | The normal or canonical form for a type declaration, an 'APINode'.
-- Equality of the normal form indicates equivalence of APIs.
--
-- We track all types.
--
data NormTypeDecl
    = NRecordType  NormRecordType
    | NUnionType   NormUnionType
    | NEnumType    NormEnumType
    | NTypeSynonym APIType
    | NNewtype     BasicType
  deriving (Eq, Show)

instance NFData NormTypeDecl where
  rnf (NRecordType  x) = rnf x
  rnf (NUnionType   x) = rnf x
  rnf (NEnumType    x) = rnf x
  rnf (NTypeSynonym x) = rnf x
  rnf (NNewtype     x) = rnf x

-- | The canonical form of a record type is a map from fields to
-- values...
type NormRecordType = Map FieldName APIType
-- | ...similarly a union is a map from fields to alternatives...
type NormUnionType  = Map FieldName APIType
-- | ...and an enum is a set of values.
type NormEnumType   = Set FieldName


-- | Compute the normal form of an API, discarding extraneous information.
apiNormalForm :: API -> NormAPI
apiNormalForm api =
    Map.fromList
      [ (name, declNF spec)
      | ThNode (APINode {anName = name, anSpec = spec}) <- api ]

-- | Compute the normal form of a single type declaration.
declNF :: Spec -> NormTypeDecl
declNF (SpRecord (SpecRecord fields)) = NRecordType $ Map.fromList
                                          [ (fname, ftType ftype)
                                          | (fname, ftype) <- fields ]
declNF (SpUnion (SpecUnion alts))     = NUnionType $ Map.fromList
                                          [ (fname, ftype)
                                          | (fname, (ftype, _)) <- alts ]
declNF (SpEnum (SpecEnum elems))      = NEnumType $ Set.fromList
                                          [ fname | (fname, _) <- elems ]
declNF (SpSynonym t)                  = NTypeSynonym t
declNF (SpNewtype (SpecNewtype bt _)) = NNewtype bt


-------------------------
-- Dependency analysis
--

-- | Find the set of type names used in an API
typeDeclsFreeVars :: NormAPI -> Set TypeName
typeDeclsFreeVars = Set.unions . map typeDeclFreeVars . Map.elems

-- | Find the set of type names used in a declaration
typeDeclFreeVars :: NormTypeDecl -> Set TypeName
typeDeclFreeVars (NRecordType fields) = Set.unions . map typeFreeVars
                                                   . Map.elems $ fields
typeDeclFreeVars (NUnionType  alts)   = Set.unions . map typeFreeVars
                                                   . Map.elems $ alts
typeDeclFreeVars (NEnumType _)        = Set.empty
typeDeclFreeVars (NTypeSynonym t)     = typeFreeVars t
typeDeclFreeVars (NNewtype _)         = Set.empty

-- | Find the set of type names used in an type
typeFreeVars :: APIType -> Set TypeName
typeFreeVars (TyList  t) = typeFreeVars t
typeFreeVars (TyMaybe t) = typeFreeVars t
typeFreeVars (TyName  n) = Set.singleton n
typeFreeVars (TyBasic _) = Set.empty
typeFreeVars  TyJSON     = Set.empty


-- | Check if a type is declared in the API
typeDeclaredInApi :: TypeName -> NormAPI -> Bool
typeDeclaredInApi tname api = Map.member tname api

-- | Check if a type is used anywhere in the API
typeUsedInApi :: TypeName -> NormAPI -> Bool
typeUsedInApi tname api = tname `Set.member` typeDeclsFreeVars api

-- | Check if the first type's transitive dependencies include the
-- second type
typeUsedInTransitiveDep :: TypeName -> TypeName -> NormAPI -> Bool
typeUsedInTransitiveDep root tname api =
    tname == root || tname `Set.member` transitiveDeps api (Set.singleton root)

-- | Compute the transitive dependencies of a set of types
transitiveDeps :: NormAPI -> Set TypeName -> Set TypeName
transitiveDeps api = transitiveClosure $ \ s ->
                         typeDeclsFreeVars $
                         Map.filterWithKey (\ x _ -> x `Set.member` s) api

-- | Compute the set of types that depend (transitively) on the given types
transitiveReverseDeps :: NormAPI -> Set TypeName -> Set TypeName
transitiveReverseDeps api = transitiveClosure $ \ s ->
                         Map.keysSet $
                         Map.filter (intersects s . typeDeclFreeVars) api
  where
    intersects s1 s2 = not $ Set.null $ s1 `Set.intersection` s2

-- | Compute the transitive closure of a relation.  Relations are
-- represented as functions that takes a set of elements to the set of
-- related elements.
transitiveClosure :: Ord a => (Set a -> Set a) -> Set a -> Set a
transitiveClosure rel x = findUsed x0 x0
  where
    x0 = rel x

    findUsed seen old
      | Set.null new = seen
      | otherwise    = findUsed (seen `Set.union` new) new
      where
        new = rel old `Set.difference` seen


-------------------------
-- Invariant validation
--

-- | Test that all the free type names in a type are declared in the
-- API.  If not, return the set of undeclared types.
typeIsValid :: APIType -> NormAPI -> Either (Set TypeName) ()
typeIsValid t api
    | typeVars `Set.isSubsetOf` declaredTypes = return ()
    | otherwise = Left (typeVars Set.\\ declaredTypes)
  where
    typeVars      = typeFreeVars t
    declaredTypes = Map.keysSet api

-- | Test that all the types used in a type declaration are declared
-- in the API.  If not, return the set of undeclared types.
declIsValid :: NormTypeDecl -> NormAPI -> Either (Set TypeName) ()
declIsValid decl api
    | declVars `Set.isSubsetOf` declaredTypes = return ()
    | otherwise = Left (declVars Set.\\ declaredTypes)
  where
    declVars      = typeDeclFreeVars decl
    declaredTypes = Map.keysSet api

-- | Test that all the types used in the API are declared.  If not,
-- return the set of undeclared types.
apiInvariant :: NormAPI -> Either (Set TypeName) ()
apiInvariant api
  | usedTypes `Set.isSubsetOf` declaredTypes = return ()
  | otherwise = Left (usedTypes Set.\\ declaredTypes)
  where
    usedTypes     = typeDeclsFreeVars api
    declaredTypes = Map.keysSet api


-------------------------
-- Modifying types
--

-- | Substitute types for type names in a declaration
substTypeDecl :: (TypeName -> APIType) -> NormTypeDecl -> NormTypeDecl
substTypeDecl f   (NRecordType fields) = NRecordType (Map.map (substType f) fields)
substTypeDecl f   (NUnionType  alts)   = NUnionType (Map.map (substType f) alts)
substTypeDecl _ d@(NEnumType _)        = d
substTypeDecl f   (NTypeSynonym t)     = NTypeSynonym (substType f t)
substTypeDecl _ d@(NNewtype _)         = d

-- | Substitute types for type names in a type
substType :: (TypeName -> APIType) -> APIType -> APIType
substType f (TyList  t)   = TyList (substType f t)
substType f (TyMaybe t)   = TyMaybe (substType f t)
substType f (TyName  n)   = f n
substType _ t@(TyBasic _) = t
substType _ t@TyJSON      = t

-- | Rename the first type to the second throughout the API
renameTypeUses :: TypeName -> TypeName -> NormAPI -> NormAPI
renameTypeUses tname tname' = Map.map (substTypeDecl rename)
  where
    rename tn | tn == tname = TyName tname'
              | otherwise   = TyName tn


instance PPLines NormTypeDecl where
  ppLines (NRecordType flds) = "record" : map (\ (f, ty) -> "  " ++ pp f
                                                            ++ " :: " ++ pp ty)
                                              (Map.toList flds)
  ppLines (NUnionType alts)  = "union"  : map (\ (f, ty) -> "  | " ++ pp f
                                                            ++ " :: " ++ pp ty)
                                              (Map.toList alts)
  ppLines (NEnumType vals)   = "enum"   : map (\ v -> "  | " ++ pp v)
                                              (Set.toList vals)
  ppLines (NTypeSynonym t)   = [pp t]
  ppLines (NNewtype b)       = ["basic " ++ pp b]
