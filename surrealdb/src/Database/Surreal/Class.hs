{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | The user facing mapping between Haskell types and 'SurrealValue'. User
-- records only ever implement 'SurrealRecord'; they never touch a codec, so the
-- CBOR versus JSON distinction cannot leak into application code.
--
-- A record with a 'GHC.Generics.Generic' instance gets 'SurrealRecord' for
-- free:
--
-- > data Person = Person { id :: RecordId, name :: Text, age :: Int }
-- >   deriving (Generic, Show)
-- >
-- > instance SurrealRecord Person
module Database.Surreal.Class
  ( SurrealRecord (..)
  , decodeField
  , describeValue
  ) where

import           Data.Bifunctor    (first)
import           Data.Int          (Int64)
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Data.Time         (UTCTime)
import           Data.Vector       (Vector)
import qualified Data.Vector       as V
import           GHC.Generics

import           Database.Surreal.Error
import           Database.Surreal.Types

-- | Convert a Haskell value to and from the canonical 'SurrealValue' AST.
class SurrealRecord a where
  toSurreal   :: a -> SurrealValue
  fromSurreal :: SurrealValue -> Either DecodeError a

  default toSurreal :: (Generic a, GToObject (Rep a)) => a -> SurrealValue
  toSurreal = VObject . gToObject . from

  default fromSurreal :: (Generic a, GFromObject (Rep a)) => SurrealValue -> Either DecodeError a
  fromSurreal = \case
    VObject m -> to <$> gFromObject m
    other     -> Left (TypeMismatch "object" (describeValue other))

-- | A short human readable description of a value's shape, for error messages.
describeValue :: SurrealValue -> Text
describeValue = \case
  VNull{}     -> "null"
  VNone{}     -> "none"
  VBool{}     -> "bool"
  VInt{}      -> "int"
  VFloat{}    -> "float"
  VDecimal{}  -> "decimal"
  VString{}   -> "string"
  VBytes{}    -> "bytes"
  VArray{}    -> "array"
  VSet{}      -> "set"
  VObject{}   -> "object"
  VUuid{}     -> "uuid"
  VDateTime{} -> "datetime"
  VDuration{} -> "duration"
  VTable{}    -> "table"
  VRecordId{} -> "recordid"
  VRange{}    -> "range"
  VFuture{}   -> "future"
  VFileRef{}  -> "fileref"
  VGeometry{} -> "geometry"

-- | Look up and decode a named field from an object value. Useful when writing
-- 'fromSurreal' by hand.
decodeField :: SurrealRecord a => Text -> Map Text SurrealValue -> Either DecodeError a
decodeField name m =
  first (NestedDecode name) (fromSurreal (Map.findWithDefault VNull name m))

-- Identity and primitive instances ------------------------------------------

instance SurrealRecord SurrealValue where
  toSurreal   = id
  fromSurreal = Right

instance SurrealRecord Bool where
  toSurreal = VBool
  fromSurreal = \case VBool b -> Right b; v -> Left (TypeMismatch "bool" (describeValue v))

instance SurrealRecord Text where
  toSurreal = VString
  fromSurreal = \case VString s -> Right s; v -> Left (TypeMismatch "string" (describeValue v))

instance SurrealRecord Int where
  toSurreal = VInt . fromIntegral
  fromSurreal = \case VInt i -> Right (fromIntegral i); v -> Left (TypeMismatch "int" (describeValue v))

instance SurrealRecord Int64 where
  toSurreal = VInt
  fromSurreal = \case VInt i -> Right i; v -> Left (TypeMismatch "int" (describeValue v))

instance SurrealRecord Integer where
  toSurreal = VInt . fromInteger
  fromSurreal = \case VInt i -> Right (fromIntegral i); v -> Left (TypeMismatch "int" (describeValue v))

instance SurrealRecord Double where
  toSurreal = VFloat
  fromSurreal = \case
    VFloat d -> Right d
    VInt i   -> Right (fromIntegral i)
    v        -> Left (TypeMismatch "float" (describeValue v))

instance {-# OVERLAPPING #-} SurrealRecord String where
  toSurreal = VString . T.pack
  fromSurreal = \case VString s -> Right (T.unpack s); v -> Left (TypeMismatch "string" (describeValue v))

instance SurrealRecord a => SurrealRecord (Maybe a) where
  toSurreal Nothing  = VNone
  toSurreal (Just x) = toSurreal x
  fromSurreal VNull  = Right Nothing
  fromSurreal VNone  = Right Nothing
  fromSurreal v      = Just <$> fromSurreal v

instance SurrealRecord a => SurrealRecord [a] where
  toSurreal = VArray . V.fromList . map toSurreal
  fromSurreal = \case
    VArray xs -> traverse fromSurreal (V.toList xs)
    VSet xs   -> traverse fromSurreal (V.toList xs)
    v         -> Left (TypeMismatch "array" (describeValue v))

instance SurrealRecord a => SurrealRecord (Vector a) where
  toSurreal = VArray . V.map toSurreal
  fromSurreal = \case
    VArray xs -> V.mapM fromSurreal xs
    VSet xs   -> V.mapM fromSurreal xs
    v         -> Left (TypeMismatch "array" (describeValue v))

instance SurrealRecord a => SurrealRecord (Map Text a) where
  toSurreal = VObject . Map.map toSurreal
  fromSurreal = \case
    VObject m -> traverse fromSurreal m
    v         -> Left (TypeMismatch "object" (describeValue v))

-- Custom type instances ------------------------------------------------------

instance SurrealRecord RecordId where
  toSurreal = VRecordId
  fromSurreal = \case VRecordId r -> Right r; v -> Left (TypeMismatch "recordid" (describeValue v))

instance SurrealRecord Table where
  toSurreal = VTable
  fromSurreal = \case VTable t -> Right t; v -> Left (TypeMismatch "table" (describeValue v))

instance SurrealRecord Uuid where
  toSurreal = VUuid
  fromSurreal = \case VUuid u -> Right u; v -> Left (TypeMismatch "uuid" (describeValue v))

instance SurrealRecord DateTime where
  toSurreal = VDateTime
  fromSurreal = \case VDateTime t -> Right t; v -> Left (TypeMismatch "datetime" (describeValue v))

instance SurrealRecord Duration where
  toSurreal = VDuration
  fromSurreal = \case VDuration d -> Right d; v -> Left (TypeMismatch "duration" (describeValue v))

instance SurrealRecord Decimal where
  toSurreal = VDecimal
  fromSurreal = \case VDecimal d -> Right d; v -> Left (TypeMismatch "decimal" (describeValue v))

instance SurrealRecord Geometry where
  toSurreal = VGeometry
  fromSurreal = \case VGeometry g -> Right g; v -> Left (TypeMismatch "geometry" (describeValue v))

instance SurrealRecord FileRef where
  toSurreal = VFileRef
  fromSurreal = \case VFileRef f -> Right f; v -> Left (TypeMismatch "fileref" (describeValue v))

instance SurrealRecord UTCTime where
  toSurreal = VDateTime . dateTimeFromUTCTime
  fromSurreal = \case VDateTime t -> Right (dateTimeToUTCTime t); v -> Left (TypeMismatch "datetime" (describeValue v))

-- Generic deriving for records ----------------------------------------------

-- | Build an object map from a generic record representation.
class GToObject f where
  gToObject :: f p -> Map Text SurrealValue

instance GToObject f => GToObject (D1 d f) where
  gToObject (M1 x) = gToObject x

instance GToObject f => GToObject (C1 c f) where
  gToObject (M1 x) = gToObject x

instance (GToObject f, GToObject g) => GToObject (f :*: g) where
  gToObject (a :*: b) = Map.union (gToObject a) (gToObject b)

instance (Selector s, SurrealRecord a) => GToObject (S1 s (K1 i a)) where
  gToObject m1@(M1 (K1 x)) = Map.singleton (T.pack (selName m1)) (toSurreal x)

-- | Rebuild a generic record representation from an object map.
class GFromObject f where
  gFromObject :: Map Text SurrealValue -> Either DecodeError (f p)

instance GFromObject f => GFromObject (D1 d f) where
  gFromObject m = M1 <$> gFromObject m

instance GFromObject f => GFromObject (C1 c f) where
  gFromObject m = M1 <$> gFromObject m

instance (GFromObject f, GFromObject g) => GFromObject (f :*: g) where
  gFromObject m = (:*:) <$> gFromObject m <*> gFromObject m

instance (Selector s, SurrealRecord a) => GFromObject (S1 s (K1 i a)) where
  gFromObject m =
    let name = T.pack (selName (undefined :: S1 s (K1 i a) p))
    in (M1 . K1) <$> first (NestedDecode name) (fromSurreal (Map.findWithDefault VNull name m))
