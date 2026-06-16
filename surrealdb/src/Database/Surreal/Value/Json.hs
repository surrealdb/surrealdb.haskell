{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JSON encode and decode for 'SurrealValue'.
--
-- This module is internal; the public entry point is the
-- 'Data.Aeson.ToJSON' and 'Data.Aeson.FromJSON' instances defined in
-- "Database.Surreal.Value".
--
-- JSON is a lossy representation compared with CBOR because it has no tag
-- mechanism. The custom types are encoded using SurrealDB's string and object
-- conventions (record ids as @\"table:id\"@, datetimes and durations and uuids
-- and decimals as strings, geometry as GeoJSON). On decode, a JSON string is
-- read back as a plain string and a GeoJSON shaped object is recognised as
-- geometry; the other custom types cannot be recovered from JSON alone, which
-- is why CBOR is the default wire codec.
module Database.Surreal.Value.Json
  ( toJSONValue
  , fromJSONValue
  ) where

import qualified Data.Aeson            as A
import qualified Data.Aeson.Key        as K
import qualified Data.Aeson.KeyMap     as KM
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map.Strict       as Map
import           Data.Scientific       (floatingOrInteger, fromFloatDigits)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Data.Time.Format.ISO8601 as ISO
import           Data.Vector           (Vector)
import qualified Data.Vector           as V

import           Database.Surreal.Types

-- | Encode a value to an aeson 'A.Value'.
toJSONValue :: SurrealValue -> A.Value
toJSONValue = \case
  VNull       -> A.Null
  VNone       -> A.Null
  VBool b     -> A.Bool b
  VInt i      -> A.Number (fromIntegral i)
  VFloat d    -> A.Number (fromFloatDigits d)
  VDecimal d  -> A.String (T.pack (show (unDecimal d)))
  VString t   -> A.String t
  VBytes b    -> A.String (TE.decodeUtf8 (B16.encode b))
  VArray xs   -> A.Array (V.map toJSONValue xs)
  VSet xs     -> A.Array (V.map toJSONValue xs)
  VObject m   -> A.Object (KM.fromList [(K.fromText k, toJSONValue v) | (k, v) <- Map.toList m])
  VUuid u     -> A.String (uuidToText u)
  VDateTime t -> A.String (T.pack (ISO.iso8601Show (dateTimeToUTCTime t)))
  VDuration d -> A.String (durationToText d)
  VTable t    -> A.String (unTable t)
  VRecordId r -> A.String (recordIdToText r)
  VRange r    -> A.String (rangeToText r)
  VFuture f   -> toJSONValue (unFuture f)
  VFileRef f  -> A.String (frBucket f <> ":/" <> frKey f)
  VGeometry g -> geometryToJSON g

recordIdToText :: RecordId -> Text
recordIdToText (RecordId tb i) = tb <> ":" <> idPart
  where idPart = case i of
          VString s -> s
          VInt n    -> T.pack (show n)
          other     -> T.pack (show (toJSONValue other))

rangeToText :: Range -> Text
rangeToText (Range b e) = boundBegin b <> ".." <> boundEnd e
  where
    boundBegin = \case
      BoundIncluded v -> renderBound v
      BoundExcluded v -> renderBound v <> ">"
      BoundUnbounded  -> ""
    boundEnd = \case
      BoundIncluded v -> "=" <> renderBound v
      BoundExcluded v -> renderBound v
      BoundUnbounded  -> ""
    renderBound v = case v of
      VString s -> s
      VInt n    -> T.pack (show n)
      other     -> T.pack (show (toJSONValue other))

geometryToJSON :: Geometry -> A.Value
geometryToJSON g = case g of
  GeometryPoint p          -> obj "Point" (pointJSON p)
  GeometryLine pts         -> obj "LineString" (A.Array (V.map pointJSON pts))
  GeometryPolygon rings    -> obj "Polygon" (ringsJSON rings)
  GeometryMultiPoint pts   -> obj "MultiPoint" (A.Array (V.map pointJSON pts))
  GeometryMultiLine ls     -> obj "MultiLineString" (A.Array (V.map (A.Array . V.map pointJSON) ls))
  GeometryMultiPolygon ps  -> obj "MultiPolygon" (A.Array (V.map ringsJSON ps))
  GeometryCollection gs    ->
    A.object [ "type" A..= ("GeometryCollection" :: Text)
             , "geometries" A..= V.toList (V.map geometryToJSON gs) ]
  where
    obj ty coords = A.object [ "type" A..= (ty :: Text), "coordinates" A..= coords ]
    pointJSON (x, y) = A.Array (V.fromList [A.Number (fromFloatDigits x), A.Number (fromFloatDigits y)])
    ringsJSON rings  = A.Array (V.map (A.Array . V.map pointJSON) rings)

-- | Decode a value from an aeson 'A.Value'. This is total: ambiguous JSON is
-- decoded to the closest generic value.
fromJSONValue :: A.Value -> SurrealValue
fromJSONValue = \case
  A.Null     -> VNull
  A.Bool b   -> VBool b
  A.Number n -> case floatingOrInteger n of
                  Right i -> VInt (fromInteger i)
                  Left d  -> VFloat d
  A.String t -> VString t
  A.Array xs -> VArray (V.map fromJSONValue xs)
  A.Object o -> case geometryFromJSON o of
                  Just g  -> VGeometry g
                  Nothing -> VObject (Map.fromList [(K.toText k, fromJSONValue v) | (k, v) <- KM.toList o])

geometryFromJSON :: KM.KeyMap A.Value -> Maybe Geometry
geometryFromJSON o = do
  A.String ty <- KM.lookup "type" o
  case ty of
    "Point"           -> GeometryPoint <$> (KM.lookup "coordinates" o >>= pointOf)
    "LineString"      -> GeometryLine <$> (KM.lookup "coordinates" o >>= arrayOf pointOf)
    "Polygon"         -> GeometryPolygon <$> (KM.lookup "coordinates" o >>= arrayOf (arrayOf pointOf))
    "MultiPoint"      -> GeometryMultiPoint <$> (KM.lookup "coordinates" o >>= arrayOf pointOf)
    "MultiLineString" -> GeometryMultiLine <$> (KM.lookup "coordinates" o >>= arrayOf (arrayOf pointOf))
    "MultiPolygon"    -> GeometryMultiPolygon <$> (KM.lookup "coordinates" o >>= arrayOf (arrayOf (arrayOf pointOf)))
    "GeometryCollection" -> do
      A.Array gs <- KM.lookup "geometries" o
      GeometryCollection <$> V.mapM geometryFromValue gs
    _ -> Nothing
  where
    geometryFromValue (A.Object inner) = geometryFromJSON inner
    geometryFromValue _                = Nothing

pointOf :: A.Value -> Maybe Point
pointOf (A.Array xs)
  | V.length xs == 2
  , A.Number x <- xs V.! 0
  , A.Number y <- xs V.! 1 = Just (toRealFloat x, toRealFloat y)
  where toRealFloat = realToFrac
pointOf _ = Nothing

arrayOf :: (A.Value -> Maybe a) -> A.Value -> Maybe (Vector a)
arrayOf f (A.Array xs) = V.mapM f xs
arrayOf _ _            = Nothing
