{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverlappingInstances       #-}

module Data.API.Tools.CBOR
    ( cborTool'
    ) where

import           Data.API.TH
import           Data.API.Tools.Combinators
import           Data.API.Tools.Datatypes
import           Data.API.Tools.Enum
import           Data.API.Types
import           Data.API.Utils

import           Control.Applicative
import           Data.API.JSONToCBOR
import           Data.Binary.Serialise.CBOR.Class
import           Data.Binary.Serialise.CBOR.Decoding
import           Data.Binary.Serialise.CBOR.Encoding
import           Data.List (foldl1')
import qualified Data.Map                       as Map
import           Data.Monoid
import qualified Data.Text                      as T
import           Language.Haskell.TH

-- | Tool to generate 'Serialise' instances for types generated by
-- 'datatypesTool'. This depends on 'enumTool'.
cborTool' :: APITool
cborTool' = apiNodeTool $ apiSpecTool gen_sn_to gen_sr_to gen_su_to gen_se_to mempty
                              <> gen_pr


{-
instance Serialise JobId where
    encode = encode . _JobId
    decode = JobId <$> decode

In this version we don't check the @snFilter@, for simplicity and speed.
This is safe, since the CBOR code is used only internally as a data
representation format, not as a communication format with clients
that could potentially send faulty data.
-}

gen_sn_to :: Tool (APINode, SpecNewtype)
gen_sn_to = mkTool $ \ ts (an, sn) -> optionalInstanceD ts ''Serialise [nodeRepT an]
                                          [ simpleD 'encode (bdy_in an sn)
                                          , simpleD 'decode (bdy_out ts an sn)]
  where
    bdy_in an sn = [e| $(ine sn) . $(newtypeProjectionE an) |]
    bdy_out ts an sn = [e| $(nodeNewtypeConE ts an sn) <$> $(oute sn) |]

    ine sn = case snType sn of
            BTstring -> [e| encodeString |]
            BTbinary -> [e| encode |]
            BTbool   -> [e| encodeBool |]
            BTint    -> [e| encodeInt |]
            BTutc    -> [e| encode |]


    oute sn =
        case snType sn of
            BTstring -> [e| decodeString |]
            BTbinary -> [e| decode |]
            BTbool   -> [e| decodeBool |]
            BTint    -> [e| decodeInt |]
            BTutc    -> [e| decode |]



{-
instance Serialise JobSpecId where
     encode = \ x ->
        encodeRecord
            [ "Id"         .= jsiId         x
            , "Input"      .= jsiInput      x
            , "Output"     .= jsiOutput     x
            , "PipelineId" .= jsiPipelineId x
            ]
     decode (Record v) =
        JobSpecId <$>
            v .: "Id"                               <*>
            v .: "Input"                            <*>
            v .: "Output"                           <*>
            v .: "PipelineId"

TODO (correctness): We need to sort fields by name, so that the format
is insensitive to changes in order given by srFields.
-}

gen_sr_to :: Tool (APINode, SpecRecord)
gen_sr_to = mkTool $ \ ts (an, sr) -> do
    x <- newName "x"
    optionalInstanceD ts ''Serialise [nodeRepT an] [ simpleD 'encode (bdy_in an sr x)
                                                   , simpleD 'decode (cl an sr)
                                                   ]
  where
    bdy_in an sr x =
        let fields = srFields sr
            len = fromIntegral (length fields)  -- to Integer
            lenE = varE 'fromIntegral  -- to Word
                     `appE` (sigE (litE (integerL len))
                                  (conT ''Integer))
            -- Micro-optimization: we use the statically known @len@ value
            -- instead of creating a list of thunks from the argument of
            -- @encodeRecordFields@ and dynamically calculating
            -- it's length, long before the list is fully forced.
            writeRecordHeader = varE 'encodeMapLen `appE` lenE
            encFields =
                varE 'encodeRecordFields `appE`
                    listE [ [e| encodeString $(fieldNameE fn)
                                <> encode ($(nodeFieldE an fn) $(varE x)) |]
                            | (fn, _) <- fields ]
        in lamE [varP x] $
               varE '(<>)
                 `appE` writeRecordHeader
                 `appE` encFields

    cl an sr    = varE '(>>)
                    `appE` (varE 'decodeMapLen)  -- TODO (extra check): check len with srFields
                    `appE` bdy
      where
        bdy = applicativeE (nodeConE an) $ map project (srFields sr)
        project (_fn, ft) = [e| decodeString >> decode |]
          where _ro    = ftReadOnly ft  -- TODO (extra check): use as in withDefaultField
                _mb_dv = ftDefault ft  -- TODO (extra check): use as in withDefaultField
          -- TODO (correctness): check that $(fieldNameE fn) matches the decoded name
          -- and if not, use the default value, etc.

-- We can assume the record has at least 1 field.
encodeRecordFields :: [Encoding] -> Encoding
encodeRecordFields l = foldl1' (<>) l


{-
instance Serialise Foo where
    encode (Bar x) = encodeUnion "x" x
    encode (Baz x) = object [ "y" .= x ]
    decode = decodeUnion [ ("x", fmap Bar . decode)
                         , ("y", fmap Baz . decode) ]

-}

gen_su_to :: Tool (APINode, SpecUnion)
gen_su_to = mkTool $ \ ts (an, su) -> optionalInstanceD ts ''Serialise [nodeRepT an]
                                        [ funD    'encode (cls an su)
                                        , simpleD 'decode (bdy_out an su)
                                        ]
  where
    cls an su = map (cl an . fst) (suFields su)

    cl an fn = do x <- newName "x"
                  clause [nodeAltConP an fn [varP x]] (bdy fn x) []

    bdy fn x = normalB [e| encodeUnion $(fieldNameE fn) $(varE x) |]


    bdy_out an su = varE 'decodeUnion `appE` listE (map (alt an) (suFields su))

    alt an (fn, _) = [e| ( $(fieldNameE fn) , fmap $(nodeAltConE an fn) decode ) |]

decodeUnion :: [(T.Text, Decoder a)] -> Decoder a
decodeUnion ds = do
    _   <- decodeMapLen -- should always be 1
    dfn <- decodeString
    case lookup dfn ds of
      Nothing -> fail "Unexpected field in union in CBOR"
      Just d -> d

{-
instance Serialise FrameRate where
    encode = encodeString . _text_FrameRate
    decode = decodeString >>= cborStrMap_p _map_FrameRate
-}

gen_se_to :: Tool (APINode, SpecEnum)
gen_se_to = mkTool $ \ ts (an, _se) -> optionalInstanceD ts ''Serialise [nodeRepT an]
                                         [ simpleD 'encode (bdy_in an)
                                         , simpleD 'decode (bdy_out an)
                                         ]
  where
    bdy_in an = [e| encodeString . $(varE (text_enum_nm an)) |]

    bdy_out an = [e| decodeString >>= cborStrMap_p $(varE (map_enum_nm an)) |]

-- In a monad, to @fail@ instead of crashing with @error@.
cborStrMap_p :: (Monad m, Ord a) => Map.Map T.Text a -> T.Text -> m a
cborStrMap_p mp t = case Map.lookup t mp of
  Nothing -> fail "Unexpected enumeration key in CBOR"
  Just r -> return r


gen_pr :: Tool APINode
gen_pr = mkTool $ \ ts an -> case anConvert an of
  Nothing               -> return []
  Just (inj_fn, prj_fn) -> optionalInstanceD ts ''Serialise [nodeT an] [ simpleD 'encode bdy_in
                                                                       , simpleD 'decode bdy_out
                                                                       ]
   where
    bdy_in = [e| encode . $prj |]
    prj = varE $ mkName $ _FieldName prj_fn

    bdy_out = [e| decode >>= $inj |]
    inj = varE $ mkName $ _FieldName inj_fn


fieldNameE :: FieldName -> ExpQ
fieldNameE = stringE . _FieldName
