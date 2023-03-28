{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines a generic representation of values belonging
-- to a schema, for use during data migration.
module Data.API.Value
    ( -- * Types
      Value(..)
    , Record
    , Field(..)

      -- * Converting to and from generic values
    , fromDefaultValue
    , fromJSON
    , parseJSON
    , encode
    , decode

      -- * Data validation
    , matchesNormAPI
    , expectRecord
    , expectEnum
    , expectUnion
    , expectList
    , expectSet
    , expectMaybe
    , lookupType

      -- * Manipulating records
    , recordToMap
    , mapToRecord
    , insertField
    , renameField
    , deleteField
    , findField
    , joinRecords

      -- * QuickCheck test infrastructure
    , arbitrary
    , arbitraryOfType
    , arbitraryJSONValue
    , prop_jsonRoundTrip
    , prop_jsonGeneric
    , prop_cborRoundTrip
    , prop_cborGeneric
    ) where

import           Data.API.Error
import           Data.API.JSON
import           Data.API.JSON.Compat
import           Data.API.NormalForm
import           Data.API.Time
import           Data.API.Types
import           Data.API.Utils

import           Control.Applicative
import           Control.Monad
import           Control.DeepSeq
import qualified Data.Aeson                     as JS
import qualified Codec.Serialise          as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import           Data.Binary.Serialise.CBOR.Extra
import qualified Codec.CBOR.FlatTerm as CBOR
import           Data.Binary.Serialise.CBOR.JSON
import           Data.List (sortBy)
import qualified Data.Map.Strict                as Map
import           Data.Monoid
import           Data.Ord
import qualified Data.Set                       as Set
import qualified Data.Text                      as T
import           Data.Traversable
import           Data.Time (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Vector                    as V
import qualified Test.QuickCheck                as QC
import qualified Test.QuickCheck.Property       as QCP
import           Prelude


-- | Generic representation of a data value belonging to a schema
-- type.  This representation has the following properties:
--
--  * it is straightforward to convert into either CBOR or JSON;
--
--  * decoding CBOR or parsing JSON requires the schema, and takes
--    advantage of it by introducing type distinctions and interning
--    field names;
--
--  * decoding CBOR is relatively efficient.
data Value = String  !T.Text
           | UTCTime !UTCTime
           | Bytes   !Binary
           | Bool    !Bool
           | Int     !Int
           | List    ![Value]
           | Set     !(Set.Set Value)
           | Maybe   !(Maybe Value)
           | Union   !FieldName !Value
           | Enum    !FieldName
           | Record  !Record
           | JSON    !JS.Value
    deriving (Eq, Show)

-- | A record is represented as a list of (field name, value) pairs.
--
-- Invariant: these are in ascending order by field name, and there
-- are no duplicates.
--
-- TODO: consider if it would be worth using 'Map.Map' instead.
type Record = [Field]

data Field = Field { fieldName  :: FieldName
                   , fieldValue :: Value
                   }
    deriving (Eq, Show)

instance NFData Value where
  rnf (String t)   = rnf t
  rnf (UTCTime t)  = rnf t
  rnf (Bytes b)    = rnf b
  rnf (Bool b)     = rnf b
  rnf (Int i)      = rnf i
  rnf (List xs)    = rnf xs
  rnf (Set xs)     = rnf xs
  rnf (Maybe mb)   = rnf mb
  rnf (Union fn v) = rnf fn `seq` rnf v
  rnf (Enum fn)    = rnf fn
  rnf (Record xs)  = rnf xs
  rnf (JSON v)     = rnf v

instance NFData Field where
  rnf (Field x y) = rnf x `seq` rnf y


-- | Convert a 'DefaultValue' into a generic 'Value', failing if the
-- type is not compatible.  This requires type information so that it
-- can introduce type distinctions absent in 'DefaultValue', e.g. when
-- 'DefValList' is used at type @'TyMaybe' ('TyList' t)@.
fromDefaultValue :: NormAPI -> APIType -> DefaultValue -> Maybe Value
fromDefaultValue api ty0 dv = case (ty0, dv) of
    (TyList  _, DefValList)    -> pure (List [])
    (TySet   _, DefValSet)     -> pure (Maybe Nothing)
    (TyMaybe _, DefValMaybe)   -> pure (Maybe Nothing)
    (TyMaybe ty, _)            -> Maybe . Just <$> fromDefaultValue api ty dv
    (TyBasic bt, _)            -> fromDefaultValueBasic bt dv
    (TyJSON,    _)             -> pure (JSON (defaultValueAsJsValue dv))
    (TyName tname, _)          -> do d <- Map.lookup tname api
                                     case d of
                                       NTypeSynonym ty -> fromDefaultValue api ty dv
                                       NNewtype bt     -> fromDefaultValueBasic bt dv
                                       NEnumType vals | DefValString s <- dv
                                                      , FieldName s `Set.member` vals
                                                      -> pure (Enum (FieldName s))
                                       _ -> Nothing
    _ -> Nothing

fromDefaultValueBasic :: BasicType -> DefaultValue -> Maybe Value
fromDefaultValueBasic bt dv = case (bt, dv) of
    (BTstring, DefValString s) -> Just (String s)
    (BTbinary, DefValString s) -> case base64ToBinary s of
                                    Right b -> Just (Bytes b)
                                    Left  _ -> Nothing
    (BTbool, DefValBool b)     -> Just (Bool b)
    (BTint, DefValInt i)       -> Just (Int i)
    (BTutc, DefValUtc u)       -> Just (UTCTime u)
    _                          -> Nothing



instance JS.ToJSON Value where
  toJSON v0 = case v0 of
                String t       -> JS.String t
                UTCTime t      -> JS.String (printUTC t)
                Bytes b        -> JS.toJSON b
                Bool b         -> JS.Bool b
                Int i          -> JS.toJSON i
                List vs        -> JS.toJSON vs
                Set  xs        -> JSONCBORSet (map JS.toJSON . Set.toAscList $ xs)
                Maybe Nothing  -> JS.Null
                Maybe (Just v) -> JS.toJSON v
                Union fn v     -> JS.object [fieldNameToKey fn JS..= v]
                Enum fn        -> JS.String (_FieldName fn)
                Record xs      -> JS.object $ map (\ (Field fn v) -> fieldNameToKey fn JS..= v) xs
                JSON js        -> js

-- | Parse a generic 'Value' from a JSON 'JS.Value', given the schema
-- and expected type.  This is not particularly optimized.  For the
-- other direction, use 'JS.toJSON'.
fromJSON :: NormAPI -> APIType -> JS.Value -> Either [(JSONError, Position)] (Value, [(JSONWarning, Position)])
fromJSON api ty v = runParserWithErrsTop defaultParseFlags (parseJSON api ty v)

parseJSON :: NormAPI -> APIType -> JS.Value -> ParserWithErrs Value
parseJSON api ty0 v = case ty0 of
    TyName tn  -> parseJSONDecl api tn (lookupTyName api tn) v
    TyList ty  -> case v of
                    JS.Array arr -> List <$> traverse (parseJSON api ty) (V.toList arr)
                    _            -> failWith (expectedArray v)
    TySet ty   -> case v of
                    JS.Array arr -> Set . Set.fromDistinctAscList <$> traverse (parseJSON api ty) (V.toList arr)
                    _            -> failWith (expectedArray v)
    TyMaybe ty -> case v of
                    JS.Null -> pure (Maybe Nothing)
                    _       -> Maybe . Just <$> parseJSON api ty v
    TyJSON     -> pure (JSON v)
    TyBasic bt -> parseJSONBasic bt v

parseJSONBasic :: BasicType -> JS.Value -> ParserWithErrs Value
parseJSONBasic bt = case bt of
    BTstring -> withText   "String"  (pure . String)
    BTbinary -> withBinary "Bytes"   (pure . Bytes)
    BTbool   -> withBool   "Bool"    (pure . Bool)
    BTint    -> withInt    "Int"     (pure . Int)
    BTutc    -> withUTC    "UTCTime" (pure . UTCTime)

parseJSONDecl :: NormAPI -> TypeName -> NormTypeDecl -> JS.Value -> ParserWithErrs Value
parseJSONDecl api tn d = case d of
    NRecordType nrt -> \ v -> case v of
                                JS.Object hm -> Record <$> traverse (parseField hm) (Map.toList nrt)
                                _            -> failWith (expectedObject v)
    NUnionType  nut -> withUnion (map (\ (fn, ty) -> (_FieldName fn, fmap (Union fn) . parseJSON api ty)) (Map.toList nut))
    NEnumType   net -> withText (T.unpack (_TypeName tn)) $ \ k ->
                           case lookupSet (FieldName k) net of
                             Just fn -> pure (Enum fn)
                             Nothing -> failWith (UnexpectedEnumVal (map _FieldName (Set.toList net)) k)
    NTypeSynonym ty -> parseJSON api ty
    NNewtype     bt -> parseJSONBasic bt
  where
    parseField hm (fn, ty) = Field fn <$> withField (_FieldName fn) (parseJSON api ty) hm



-- | Efficiently encode a generic 'Value' in CBOR format.
encode :: Value -> CBOR.Encoding
encode v0 = case v0 of
    String t   -> CBOR.encodeString t
    UTCTime t  -> CBOR.encode t
    Bytes b    -> CBOR.encode b
    Bool b     -> CBOR.encode b
    Int i      -> CBOR.encode i
    List vs    -> encodeListWith encode vs
    Set  vs    -> encodeSetLikeWith encode (Set.toAscList vs)
    Maybe mb_v -> encodeMaybeWith encode mb_v
    Union fn v -> encodeUnion (_FieldName fn) (encode v)
    Enum fn    -> CBOR.encode (_FieldName fn)
    Record xs  -> CBOR.encodeMapLen (fromIntegral (length xs))
                  <> encodeRecordFields (map (\ (Field fn v) -> CBOR.encode (_FieldName fn)
                                                                    <> encode v) xs)
    JSON js    -> encodeJSON js


-- | Efficiently decode CBOR as a generic 'Value', given the schema
-- and expected type.
decode :: NormAPI -> APIType -> CBOR.Decoder s Value
decode api ty0 = case ty0 of
    TyName tn  -> decodeDecl api (lookupTyName api tn)
    TyList ty  -> List  <$!> decodeListWith (decode api ty)
    TySet ty   -> Set . Set.fromDistinctAscList <$!> decodeSetLikeWith (decode api ty)
    TyMaybe ty -> Maybe <$!> decodeMaybeWith (decode api ty)
    TyJSON     -> JSON  <$!> decodeJSON
    TyBasic bt -> decodeBasic bt

decodeBasic :: BasicType -> CBOR.Decoder s Value
decodeBasic bt = case bt of
    BTstring -> String <$!> CBOR.decode
    BTbinary -> Bytes  <$!> CBOR.decode
    BTbool   -> Bool   <$!> CBOR.decode
    BTint    -> Int    <$!> CBOR.decode
    BTutc    -> UTCTime <$!> CBOR.decode

decodeDecl :: NormAPI -> NormTypeDecl -> CBOR.Decoder s Value
decodeDecl api d = case d of
    NRecordType nrt -> do _ <- CBOR.decodeMapLen
                          go [] (Map.toList nrt)
    NUnionType  nut -> do _ <- CBOR.decodeMapLen
                          k <- CBOR.decodeString
                          case lookupMap (FieldName k) nut of
                            Just (fn, ty) -> Union fn <$!> decode api ty
                            Nothing       -> fail $ "unexpected union alternative: " ++ T.unpack k
    NEnumType   net -> do k <- CBOR.decodeString
                          case lookupSet (FieldName k) net of
                            Just fn -> pure (Enum fn)
                            Nothing -> fail $ "unexpected enum alternative: " ++ T.unpack k
    NTypeSynonym ty -> decode api ty
    NNewtype     bt -> decodeBasic bt
  where
    go xs []            = pure (Record (reverse xs))
    go xs ((fn, ty):ys) = do _  <- CBOR.decodeString
                             !v <- decode api ty
                             go (Field fn v:xs) ys


-- | Check that the value is of the given type in the schema,
-- reporting the first error encountered if it does not conform.
matchesNormAPI :: NormAPI -> APIType -> Value -> Position -> Either (ValueError, Position) ()
matchesNormAPI api ty0 v0 p = case ty0 of
    TyName tn  -> do d <- lookupType tn api ?!? (\ f -> (InvalidAPI f, p))
                     matchesNormAPIDecl api d v0 p
    TyList ty  -> case v0 of
                    List vs -> mapM_ (\ (i, v) -> matchesNormAPI api ty v (InElem i : p)) (zip [0..] vs)
                    _       -> Left (JSONError (expectedArray js_v), p)
    TySet ty  -> case v0 of
                    Set vs -> mapM_ (\ (i, v) -> matchesNormAPI api ty v (InElem i : p)) (zip [0..] $ Set.toAscList vs)
                    _      -> Left (JSONError (expectedSet js_v), p)
    TyMaybe ty -> case v0 of
                    Maybe Nothing -> return ()
                    Maybe (Just v) -> matchesNormAPI api ty v p
                    _              -> Left (JSONError (Expected ExpObject "Maybe" js_v), p)
    TyJSON     -> case v0 of
                    JSON _ -> return ()
                    _      -> Left (JSONError (Expected ExpObject "JSON" js_v), p)
    TyBasic bt -> matchesNormAPIBasic bt v0 p
  where
    js_v = JS.toJSON v0

matchesNormAPIBasic :: BasicType -> Value -> Position -> Either (ValueError, Position) ()
matchesNormAPIBasic bt v p = case (bt, v) of
    (BTstring, String _) -> return ()
    (BTstring, _)        -> Left (JSONError (expectedString js_v), p)
    (BTbinary, Bytes _)  -> return ()
    (BTbinary, _)        -> Left (JSONError (expectedString js_v), p)
    (BTbool, Bool _)     -> return ()
    (BTbool, _)          -> Left (JSONError (expectedBool js_v), p)
    (BTint, Int _)       -> return ()
    (BTint, _)           -> Left (JSONError (expectedInt js_v), p)
    (BTutc, UTCTime _)   -> return ()
    (BTutc, _)           -> Left (JSONError (Expected ExpString "UTCTime" js_v), p)
  where
    js_v = JS.toJSON v

matchesNormAPIDecl :: NormAPI -> NormTypeDecl -> Value -> Position -> Either (ValueError, Position) ()
matchesNormAPIDecl api d v0 p = case d of
    NRecordType nrt -> do xs <- expectRecord v0 p
                          case compare (length xs) (Map.size nrt) of
                            LT -> Left (JSONError MissingField, p)
                            EQ -> mapM_ matchesNormAPIField (zip (Map.toList nrt) xs)
                            GT -> Left (JSONError UnexpectedField, p)
    NUnionType  nut -> do (fn, v) <- expectUnion v0 p
                          case Map.lookup fn nut of
                            Just ty -> matchesNormAPI api ty v (inField fn : p)
                            Nothing -> Left (JSONError UnexpectedField, inField fn : p)
    NEnumType   net -> do fn <- expectEnum v0 p
                          unless (Set.member fn net) $ Left (JSONError (UnexpectedEnumVal (map _FieldName (Set.toList net)) (_FieldName fn)), p)
    NTypeSynonym ty -> matchesNormAPI api ty v0 p
    NNewtype     bt -> matchesNormAPIBasic bt v0 p
  where
    matchesNormAPIField ((fn, ty), Field fn' v)
        | fn == fn' = matchesNormAPI api ty v (inField fn : p)
        | otherwise = Left (JSONError (SyntaxError (unlines ["record out of order: ", show fn, show fn', show d, show v0])), p)

expectRecord :: Value -> Position -> Either (ValueError, Position) Record
expectRecord (Record xs) _ = pure xs
expectRecord v           p = Left (JSONError (Expected ExpObject "Record" (JS.toJSON v)), p)

expectEnum :: Value -> Position -> Either (ValueError, Position) FieldName
expectEnum (Enum s) _ = pure s
expectEnum v        p = Left (JSONError (Expected ExpString "Enum" (JS.toJSON v)), p)

expectUnion :: Value -> Position -> Either (ValueError, Position) (FieldName, Value)
expectUnion (Union fname v) _ = pure (fname, v)
expectUnion v               p = Left (JSONError (Expected ExpObject "Union" (JS.toJSON v)), p)

expectList :: Value -> Position -> Either (ValueError, Position) [Value]
expectList (List xs) _ = pure xs
expectList v         p = Left (JSONError (Expected ExpArray "List" (JS.toJSON v)), p)

expectSet :: Value -> Position -> Either (ValueError, Position) [Value]
expectSet (Set xs) _  = pure $ Set.toAscList $ xs
expectSet v         p = Left (JSONError (Expected ExpArray "List" (JS.toJSON v)), p)

expectMaybe :: Value -> Position -> Either (ValueError, Position) (Maybe Value)
expectMaybe (Maybe v) _ = pure v
expectMaybe v         p = Left (JSONError (Expected ExpArray "Maybe" (JS.toJSON v)), p)

lookupType :: TypeName -> NormAPI -> Either ApplyFailure NormTypeDecl
lookupType tname api = Map.lookup tname api ?! TypeDoesNotExist tname


-- | Given a schema, generate an arbitrary type corresponding to the
-- schema and an arbitrary value of that type.
arbitrary :: NormAPI -> QC.Gen (APIType, Value)
arbitrary api = do tn <- QC.elements (Map.keys api)
                   v  <- arbitraryOfType api (TyName tn)
                   return (TyName tn, v)

-- | Given a schema and a type, generate an arbitrary value of that
-- type.
arbitraryOfType :: NormAPI -> APIType -> QC.Gen Value
arbitraryOfType api ty0 = case ty0 of
    TyName  tn -> arbitraryOfDecl api (lookupTyName api tn)
    TyList  ty -> List  <$> QC.listOf (arbitraryOfType api ty)
    TySet   ty -> Set . Set.fromDistinctAscList <$> QC.listOf (arbitraryOfType api ty)
    TyMaybe ty -> Maybe <$> QC.oneof [pure Nothing, Just <$> arbitraryOfType api ty]
    TyJSON     -> JSON  <$> arbitraryJSONValue
    TyBasic bt -> arbitraryOfBasicType bt

arbitraryOfBasicType :: BasicType -> QC.Gen Value
arbitraryOfBasicType bt = case bt of
    BTstring -> String  <$> QC.arbitrary
    BTbinary -> Bytes   <$> QC.arbitrary
    BTbool   -> Bool    <$> QC.arbitrary
    BTint    -> Int     <$> QC.arbitrary
    BTutc    -> UTCTime
                . posixSecondsToUTCTime
                . (realToFrac :: Int -> NominalDiffTime)
                <$> QC.arbitrary

arbitraryOfDecl :: NormAPI -> NormTypeDecl -> QC.Gen Value
arbitraryOfDecl api d = case d of
    NRecordType nrt -> Record <$> traverse (\ (fn, ty) -> Field fn <$> arbitraryOfType api ty) (Map.toList nrt)
    NUnionType  nut -> do (fn, ty) <- QC.elements (Map.toList nut)
                          Union fn <$> arbitraryOfType api ty
    NEnumType   net -> Enum <$> QC.elements (Set.toList net)
    NTypeSynonym ty -> arbitraryOfType api ty
    NNewtype     bt -> arbitraryOfBasicType bt

-- | A reasonably varied generator for JSON 'JS.Value's.
--
-- Hack alert: we do not generate 'JS.Null', because Aeson fails to
-- round-trip @'Just' 'JS.Null' :: 'Maybe' 'JS.Value'@.
arbitraryJSONValue :: QC.Gen JS.Value
arbitraryJSONValue =
    QC.sized $ \ size ->
        QC.oneof [ JS.Object . listToObject <$> QC.resize (size `div` 2) (QC.listOf ((,) <$> QC.arbitrary <*> arbitraryJSONValue))
                 , JS.Array . V.fromList <$> QC.resize (size `div` 2) (QC.listOf arbitraryJSONValue)
                 , JSONCBORSet <$> QC.arbitrary
                 , JS.String <$> QC.arbitrary
                 , JS.Number . fromInteger <$> QC.arbitrary
                 , JS.Bool <$> QC.arbitrary
                 -- , pure JS.Null
                 ]



-- | QuickCheck property that converting a 'Value' to and from JSON
-- gives back the original value.
prop_jsonRoundTrip :: NormAPI -> QC.Property
prop_jsonRoundTrip api
  = QC.forAll (arbitrary api) $ \ (ty, v) ->
        case fromJSON api ty (JS.toJSON v) of
          Right (y, ws) | v /= y        -> QCP.failed { QCP.reason = "Expected " ++ show v
                                                                       ++ " but got " ++ show y }
                        | not (null ws) -> QCP.failed { QCP.reason = "Unexpected warnings: " ++ show ws }
                        | otherwise     -> QCP.succeeded
          Left err                      -> QCP.failed { QCP.reason = "Parse error: " ++ prettyJSONErrorPositions err }

-- | QuickCheck property that the type-specific JSON serialisation
-- agrees with deserialising as generic JSON and then serialising again.
prop_jsonGeneric :: JS.ToJSON a => API -> TypeName -> a -> QCP.Result
prop_jsonGeneric api tn x = case fromJSON napi (TyName tn) js_v of
    Right (v, ws) | JS.toJSON v /= js_v -> QCP.failed { QCP.reason = "Expected " ++ show js_v
                                                                       ++ " but got " ++ show (JS.toJSON v) }
                  | not (null ws)       -> QCP.failed { QCP.reason = "Unexpected warnings: " ++ show ws }
                  | otherwise           -> QCP.succeeded
    Left err                            -> QCP.failed { QCP.reason = "Parse error: " ++ prettyJSONErrorPositions err }
  where
    napi = apiNormalForm api
    js_v = JS.toJSON x

-- | QuickCheck property that converting a 'Value' to and from CBOR
-- gives back the original value.
prop_cborRoundTrip :: NormAPI -> QC.Property
prop_cborRoundTrip api
  = QC.forAll (arbitrary api) $ \ (ty, v) ->
       case CBOR.fromFlatTerm (decode api ty) (CBOR.toFlatTerm (encode v)) of
         Right v' | v /= v'   -> QCP.failed { QCP.reason = "Expected " ++ show v
                                                             ++ " but got " ++ show v' }
                  | otherwise -> QCP.succeeded
         Left err             -> QCP.failed { QCP.reason = "Parse error: " ++ err }

-- | QuickCheck property that the type-specific CBOR serialisation
-- agrees with deserialising as generic CBOR and then serialising again.
prop_cborGeneric :: CBOR.Serialise a => API -> TypeName -> a -> QCP.Result
prop_cborGeneric api tn x
  | not (CBOR.validFlatTerm bs) = QCP.failed { QCP.reason = "Invalid CBOR: " ++ show bs }
  | otherwise = case CBOR.fromFlatTerm (decode napi (TyName tn)) bs of
    Right v | bs' <- CBOR.toFlatTerm (encode v)
            , bs' /= bs -> QCP.failed { QCP.reason = "Expected " ++ show bs ++ " but got " ++ show bs' }
            | otherwise      -> QCP.succeeded
    Left err                 -> QCP.failed { QCP.reason = "Decode error: " ++ err }
  where
    napi = apiNormalForm api
    bs = CBOR.toFlatTerm (CBOR.encode x)


-- | Look up a type in a schema, failing with an error if it is missing.
lookupTyName :: NormAPI -> TypeName -> NormTypeDecl
lookupTyName api tn = case Map.lookup tn api of
                        Just d  -> d
                        Nothing -> error $ "lookupTyName: missing declaration for "
                                               ++ T.unpack (_TypeName tn)

-- | Look up a key in a set, returning a pointer to the set's copy of
-- the key.  This is useful during deserialisation because it means we
-- can share a single key, avoiding retaining deserialised copies.
lookupSet :: Ord a => a -> Set.Set a -> Maybe a
#if MIN_VERSION_containers(0,5,2)
lookupSet k s = flip Set.elemAt s <$> Set.lookupIndex k s
#else
-- alternative implementation for containers versions without lookupIndex/elemAt
lookupSet k s = case Set.lookupLE k s of
                  Just k' | k == k' -> Just k'
                  _                 -> Nothing
#endif

-- | Look up a key in a map, returning both the value and the map's
-- copy of the key.  This is useful during deserialisation because it
-- means we can share a single key, avoiding retaining deserialised
-- copies.
lookupMap :: Ord k => k -> Map.Map k a -> Maybe (k, a)
lookupMap k m = flip Map.elemAt m <$> Map.lookupIndex k m


recordToMap :: Record -> Map.Map FieldName Value
recordToMap = Map.fromList . map (\ (Field fn v) -> (fn, v))

mapToRecord :: Map.Map FieldName Value -> Record
mapToRecord = map (uncurry Field) . Map.toList

-- | Insert a (field, value) pair into a record, replacing the
-- existing field if it is present and preserving the ordering
-- invariant.
insertField :: FieldName -> Value -> Record -> Record
insertField fname v []                      = [Field fname v]
insertField fname v xxs@(x@(Field fn _):xs) = case compare fname fn of
                                                GT -> x : insertField fname v xs
                                                EQ -> Field fname v : xs
                                                LT -> Field fname v : xxs

-- | Delete a field from a record, trivially preserving the ordering
-- invariant.
deleteField :: FieldName -> Record -> Record
deleteField fname = filter ((fname /=) . fieldName)

-- | Rename a field in a record, preserving the ordering invariant.
renameField :: FieldName -> FieldName -> Record -> Record
renameField fname fname' = sortBy (comparing fieldName) . map f
  where
    f x@(Field fn v) | fn == fname = Field fname' v
                     | otherwise   = x

-- | Split a record at a given field, returning the preceding fields,
-- value and succeeding fields.  Fails if the field is absent.
findField :: FieldName -> Record -> Maybe (Record, Value, Record)
findField fname xs = case break ((fname ==) . fieldName) xs of
                       (ys, (Field _ v):zs) -> Just (ys, v, zs)
                       (_, [])              -> Nothing

-- | Join together two records with a (field, value) pair in between.
-- The ordering invariant is not checked!
joinRecords :: Record -> FieldName -> Value -> Record -> Record
joinRecords ys fname v zs = ys ++ Field fname v : zs
