{-# LANGUAGE LambdaCase #-}

-- | CBOR encode and decode for 'SurrealValue', implementing SurrealDB's custom
-- tag conventions. This module is internal; the public entry point is the
-- 'Codec.Serialise.Serialise' instance defined in "Database.Surreal.Value".
--
-- The tag numbers honoured here are: 6 None, 7 Table, 8 RecordId, 9 and 37
-- Uuid (string and binary), 10 Decimal, 0 and 12 DateTime, 13 and 14 Duration,
-- 15 Future, 49 Range, 50 and 51 range bounds, 55 FileRef, 56 Set, and 88 to 94
-- Geometry.
module Database.Surreal.Value.Cbor
  ( encodeSurreal
  , decodeSurreal
  ) where

import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Map.Strict       as Map
import           Data.Scientific       (Scientific)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Time.Format.ISO8601 as ISO
import qualified Data.UUID.Types       as UUID
import           Data.Vector           (Vector)
import qualified Data.Vector           as V
import           Data.Word             (Word32, Word64)
import           Text.Read             (readMaybe)
import           Prelude               hiding (decodeFloat)

import           Database.Surreal.Types

-- | Encode a value to a CBOR 'Encoding'.
encodeSurreal :: SurrealValue -> Encoding
encodeSurreal = \case
  VNull       -> encodeNull
  VNone       -> encodeTag 6 <> encodeNull
  VBool b     -> encodeBool b
  VInt i      -> encodeInt64 i
  VFloat d    -> encodeDouble d
  VDecimal d  -> encodeTag 10 <> encodeString (scientificToText (unDecimal d))
  VString t   -> encodeString t
  VBytes b    -> encodeBytes b
  VArray xs   -> encodeArray xs
  VSet xs     -> encodeTag 56 <> encodeArray xs
  VObject m   -> encodeMapValue m
  VUuid u     -> encodeTag 37 <> encodeBytes (BL.toStrict (UUID.toByteString (unUuid u)))
  VDateTime t -> encodeDateTime t
  VDuration d -> encodeDuration d
  VTable t    -> encodeTag 7 <> encodeString (unTable t)
  VRecordId r -> encodeTag 8 <> encodeListLen 2 <> encodeString (ridTable r) <> encodeSurreal (ridId r)
  VRange r    -> encodeTag 49 <> encodeListLen 2 <> encodeBound (rangeBegin r) <> encodeBound (rangeEnd r)
  VFuture f   -> encodeTag 15 <> encodeSurreal (unFuture f)
  VFileRef f  -> encodeTag 55 <> encodeListLen 2 <> encodeString (frBucket f) <> encodeString (frKey f)
  VGeometry g -> encodeGeometry g

encodeArray :: Vector SurrealValue -> Encoding
encodeArray xs =
  encodeListLen (fromIntegral (V.length xs)) <> V.foldr (\x acc -> encodeSurreal x <> acc) mempty xs

encodeMapValue :: Map.Map Text SurrealValue -> Encoding
encodeMapValue m =
  encodeMapLen (fromIntegral (Map.size m))
    <> Map.foldrWithKey (\k v acc -> encodeString k <> encodeSurreal v <> acc) mempty m

encodeBound :: Bound -> Encoding
encodeBound = \case
  BoundIncluded v -> encodeTag 50 <> encodeSurreal v
  BoundExcluded v -> encodeTag 51 <> encodeSurreal v
  BoundUnbounded  -> encodeNull

encodeDateTime :: DateTime -> Encoding
encodeDateTime (DateTime s n) =
  encodeTag 12 <> encodeListLen 2 <> encodeInt64 s <> encodeWord (fromIntegral n)

encodeDuration :: Duration -> Encoding
encodeDuration (Duration s n)
  | s == 0 && n == 0 = encodeTag 14 <> encodeListLen 0
  | n == 0           = encodeTag 14 <> encodeListLen 1 <> encodeInt64 s
  | otherwise        = encodeTag 14 <> encodeListLen 2 <> encodeInt64 s <> encodeWord (fromIntegral n)

-- Geometry uses nested tagged values, matching SurrealDB: a line is an array of
-- tagged points, a polygon is an array of tagged lines, and so on.
encodeGeometry :: Geometry -> Encoding
encodeGeometry = \case
  GeometryPoint p         -> taggedPoint p
  GeometryLine pts        -> taggedLine pts
  GeometryPolygon rings   -> taggedPolygon rings
  GeometryMultiPoint pts  -> encodeTag 91 <> vecOf taggedPoint pts
  GeometryMultiLine ls    -> encodeTag 92 <> vecOf taggedLine ls
  GeometryMultiPolygon ps -> encodeTag 93 <> vecOf taggedPolygon ps
  GeometryCollection gs   -> encodeTag 94 <> vecOf encodeGeometry gs
  where
    taggedPoint (x, y) =
      encodeTag 88 <> encodeListLen 2 <> encodeDouble x <> encodeDouble y
    taggedLine pts    = encodeTag 89 <> vecOf taggedPoint pts
    taggedPolygon rs  = encodeTag 90 <> vecOf taggedLine rs
    vecOf f xs        =
      encodeListLen (fromIntegral (V.length xs)) <> V.foldr (\x acc -> f x <> acc) mempty xs

scientificToText :: Scientific -> Text
scientificToText = T.pack . show

-- | Decode a value from CBOR.
decodeSurreal :: Decoder s SurrealValue
decodeSurreal = peekTokenType >>= \case
  TypeNull         -> decodeNull >> pure VNull
  TypeBool         -> VBool <$> decodeBool
  TypeUInt         -> VInt <$> decodeInt64
  TypeUInt64       -> VInt <$> decodeInt64
  TypeNInt         -> VInt <$> decodeInt64
  TypeNInt64       -> VInt <$> decodeInt64
  TypeInteger      -> VInt . fromInteger <$> decodeInteger
  TypeFloat16      -> VFloat . realToFrac <$> decodeFloat
  TypeFloat32      -> VFloat . realToFrac <$> decodeFloat
  TypeFloat64      -> VFloat <$> decodeDouble
  TypeBytes        -> VBytes <$> decodeBytes
  TypeBytesIndef   -> VBytes <$> decodeBytes
  TypeString       -> VString <$> decodeString
  TypeStringIndef  -> VString <$> decodeString
  TypeListLen      -> VArray <$> decodeArray
  TypeListLen64    -> VArray <$> decodeArray
  TypeListLenIndef -> VArray <$> decodeArray
  TypeMapLen       -> VObject <$> decodeMapValue
  TypeMapLen64     -> VObject <$> decodeMapValue
  TypeMapLenIndef  -> VObject <$> decodeMapValue
  TypeTag          -> decodeTagged
  TypeTag64        -> decodeTagged
  other            -> fail ("Database.Surreal: unsupported CBOR token " ++ show other)

decodeArray :: Decoder s (Vector SurrealValue)
decodeArray = decodeListLenOrIndef >>= \case
  Just n  -> V.replicateM n decodeSurreal
  Nothing -> V.fromList <$> untilBreak decodeSurreal

decodeMapValue :: Decoder s (Map.Map Text SurrealValue)
decodeMapValue = decodeMapLenOrIndef >>= \case
  Just n  -> Map.fromList <$> sequence (replicate n entry)
  Nothing -> Map.fromList <$> untilBreak entry
  where entry = (,) <$> decodeString <*> decodeSurreal

untilBreak :: Decoder s a -> Decoder s [a]
untilBreak d = go
  where
    go = decodeBreakOr >>= \stop ->
           if stop then pure [] else (:) <$> d <*> go

readTag :: Decoder s Word64
readTag = peekTokenType >>= \case
  TypeTag   -> fromIntegral <$> decodeTag
  TypeTag64 -> decodeTag64
  _         -> fail "Database.Surreal: expected a CBOR tag"

decodeTagged :: Decoder s SurrealValue
decodeTagged = readTag >>= \case
  0  -> do s <- decodeString
           maybe (fail "invalid ISO datetime") (pure . VDateTime . dateTimeFromUTCTime)
                 (ISO.iso8601ParseM (T.unpack s))
  6  -> decodeNull >> pure VNone
  7  -> VTable . Table <$> decodeString
  8  -> do _ <- decodeListLen
           tb <- decodeString
           i  <- decodeSurreal
           pure (VRecordId (RecordId tb i))
  9  -> do s <- decodeString
           maybe (fail "invalid uuid string") (pure . VUuid) (uuidFromText s)
  10 -> do s <- decodeString
           maybe (fail "invalid decimal") (pure . VDecimal . Decimal) (readMaybe (T.unpack s))
  12 -> VDateTime <$> decodeDateTimeCompact
  13 -> do s <- decodeString
           maybe (fail "invalid duration string") (pure . VDuration) (durationFromText s)
  14 -> VDuration <$> decodeDurationCompact
  15 -> VFuture . Future <$> decodeSurreal
  37 -> do b <- decodeBytes
           maybe (fail "invalid uuid bytes") (pure . VUuid . Uuid)
                 (UUID.fromByteString (BL.fromStrict b))
  49 -> do _ <- decodeListLen
           b <- decodeBound
           e <- decodeBound
           pure (VRange (Range b e))
  55 -> do _ <- decodeListLen
           bkt <- decodeString
           key <- decodeString
           pure (VFileRef (FileRef bkt key))
  56 -> VSet <$> decodeArray
  88 -> VGeometry . GeometryPoint <$> decodePoint
  89 -> VGeometry . GeometryLine <$> decodeLine
  90 -> VGeometry . GeometryPolygon <$> decodePolygon
  91 -> VGeometry . GeometryMultiPoint <$> decodeVec decodeTaggedPoint
  92 -> VGeometry . GeometryMultiLine <$> decodeVec decodeTaggedLine
  93 -> VGeometry . GeometryMultiPolygon <$> decodeVec decodeTaggedPolygon
  94 -> VGeometry . GeometryCollection <$> decodeVec decodeGeometry
  t  -> fail ("Database.Surreal: unknown CBOR tag " ++ show t)

decodeBound :: Decoder s Bound
decodeBound = peekTokenType >>= \case
  TypeNull -> decodeNull >> pure BoundUnbounded
  _        -> readTag >>= \case
                50 -> BoundIncluded <$> decodeSurreal
                51 -> BoundExcluded <$> decodeSurreal
                t  -> fail ("Database.Surreal: unknown bound tag " ++ show t)

decodeDateTimeCompact :: Decoder s DateTime
decodeDateTimeCompact = do
  n <- maybe (fail "datetime needs definite length") pure =<< decodeListLenOrIndef
  case n of
    0 -> pure (DateTime 0 0)
    1 -> do s <- decodeInt64; pure (DateTime s 0)
    _ -> do s <- decodeInt64; ns <- decodeWord; pure (DateTime s (fromIntegral ns :: Word32))

decodeDurationCompact :: Decoder s Duration
decodeDurationCompact = do
  n <- maybe (fail "duration needs definite length") pure =<< decodeListLenOrIndef
  case n of
    0 -> pure (Duration 0 0)
    1 -> do s <- decodeInt64; pure (Duration s 0)
    _ -> do s <- decodeInt64; ns <- decodeWord; pure (Duration s (fromIntegral ns :: Word32))

decodePoint :: Decoder s Point
decodePoint = do
  _ <- decodeListLen
  x <- decodeDouble
  y <- decodeDouble
  pure (x, y)

decodeTaggedPoint :: Decoder s Point
decodeTaggedPoint = readTag >>= \case
  88 -> decodePoint
  t  -> fail ("Database.Surreal: expected geometry point tag, got " ++ show t)

decodeLine :: Decoder s (Vector Point)
decodeLine = decodeVec decodeTaggedPoint

decodeTaggedLine :: Decoder s (Vector Point)
decodeTaggedLine = readTag >>= \case
  89 -> decodeLine
  t  -> fail ("Database.Surreal: expected geometry line tag, got " ++ show t)

decodePolygon :: Decoder s (Vector (Vector Point))
decodePolygon = decodeVec decodeTaggedLine

decodeTaggedPolygon :: Decoder s (Vector (Vector Point))
decodeTaggedPolygon = readTag >>= \case
  90 -> decodePolygon
  t  -> fail ("Database.Surreal: expected geometry polygon tag, got " ++ show t)

decodeGeometry :: Decoder s Geometry
decodeGeometry = decodeSurreal >>= \case
  VGeometry g -> pure g
  _           -> fail "Database.Surreal: expected geometry in collection"

decodeVec :: Decoder s a -> Decoder s (Vector a)
decodeVec d = decodeListLenOrIndef >>= \case
  Just n  -> V.replicateM n d
  Nothing -> V.fromList <$> untilBreak d
